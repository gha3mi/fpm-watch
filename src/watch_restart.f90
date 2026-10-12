!> Supervise watcher children and restart them on nonzero exit.
!!
!! Leading CLI flags override manifest supervisor defaults. A zero child exit
!! or user interruption ends supervision; other exits restart after the delay,
!! subject to the restart limit. Command failures only reach the supervisor
!! if the watcher itself exits nonzero, as in one-shot mode. Child generations
!! share the parent's selected log format and use distinct cycle-ID prefixes.
module watch_restart
   use watch_time, only: sleep_seconds, set_low_cpu
   use watch_cmdline, only: get_arg, parse_int, parse_real, quote_arg, starts_with, parse_bool, &
                            watch_option_takes_value
   use watch_config, only: get_restart_defaults
   use watch_platform, only: execute_command
   use watch_log, only: log_event, log_int, log_real, log_string
   use fpm_strings, only: str
   use watch_util, only: ftoa
   implicit none
   private
   public maybe_supervise

contains

   !> Enter supervisor mode when requested, otherwise return immediately.
   !!
   !! If auto-restart is enabled and the current process is not marked as a
   !! child (`--watch-child`), this routine runs a supervisor loop and does not
   !! return.
   subroutine maybe_supervise(log_format)
      character(len=*), intent(in) :: log_format
      logical :: auto_restart, is_child
      real    :: restart_delay
      integer :: restart_max
      character(len=:), allocatable :: self_exe

      call parse_restart_flags(auto_restart, restart_delay, restart_max, is_child, self_exe)

      if (auto_restart .and. (.not. is_child)) then
         call supervisor_loop(max(0.0, restart_delay), restart_max, self_exe, log_format)
         stop 0
      end if
   end subroutine maybe_supervise

   !> Launch watcher children until success, user interruption, or restart exhaustion.
   !!
   !! `max0` counts restarts after the initial launch; nonpositive values are
   !! unlimited. Clamp `delay0` to zero or greater. Each restart increments the
   !! child generation. A user interruption or exhausted limit exits with the
   !! child execution status; a zero child status exits successfully.
   subroutine supervisor_loop(delay0, max0, exe0, log_format)
      real, intent(in) :: delay0
      integer, intent(in) :: max0
      character(len=*), intent(in) :: exe0
      character(len=*), intent(in) :: log_format

      character(len=:), allocatable :: child_cmd
      integer :: code, attempt
      real :: delay
      logical :: interrupted

      attempt = 0
      delay = max(0.0, delay0)

      do
         code = 0
         child_cmd = build_child_command(exe0, log_format, attempt)
         call execute_command(child_cmd, code, interrupted)

         if (interrupted) then
            call log_event("info", "supervisor.stopped", "Supervisor interrupted", [log_string("reason", "interrupt")])
            stop code
         end if

         if (code == 0) exit

         if (max0 > 0 .and. attempt >= max0) then
            call log_event("error", "supervisor.limit_reached", "Child failed; restart limit reached", &
                           [log_int("exit_code", code), log_int("restart_count", attempt)])
            stop code
         end if

         attempt = attempt + 1
         call log_event("warn", "supervisor.restart", &
                        "Child failed (exit " // str(code) // "); restart #" // str(attempt) // " in " // ftoa(delay) // "s", &
                        [log_int("exit_code", code), log_int("restart_count", attempt), log_real("delay_ms", delay * 1000.0)])
         call sleep_seconds(delay)
      end do

      stop 0
   end subroutine supervisor_loop

   !> Serialize child argv with a generation counter and the parent's log format.
   !!
   !! Remove supervisor flags and prior log-format/child markers from the leading
   !! watcher options. Preserve other leading values and every forwarded token
   !! after the subcommand, quoting each original argv token independently.
   function build_child_command(exe0, log_format, restart_count) result(cmd)
      character(len=*), intent(in) :: exe0
      character(len=*), intent(in) :: log_format
      integer, intent(in) :: restart_count
      character(len=:), allocatable :: cmd
      character(len=:), allocatable :: exe, a
      integer :: narg, i
      logical :: skip_next, copy_next, in_prefix

      exe = choose_self_exe(exe0)
      cmd = quote_arg(exe) // " --watch-child=" // str(restart_count) // " --watch-log-format " // log_format

      narg = command_argument_count()
      skip_next = .false.
      copy_next = .false.
      in_prefix = .true.

      do i = 1, narg
         a = get_arg(i)

         if (.not. in_prefix) then
            cmd = cmd // " " // quote_arg(a)
            cycle
         end if

         if (skip_next) then
            skip_next = .false.
            cycle
         end if

         if (copy_next) then
            copy_next = .false.
            cmd = cmd // " " // quote_arg(a)
            cycle
         end if

         if (.not. starts_with(a, "--watch-") .and. a /= "-q" .and. a /= "-v" .and. a /= "-vv") then
            in_prefix = .false.
            cmd = cmd // " " // quote_arg(a)
            cycle
         end if

         if (a == "--watch-auto-restart" .or. starts_with(a, "--watch-auto-restart=")) cycle
         if (a == "--watch-child") cycle
         if (starts_with(a, "--watch-child=")) cycle

         select case (a)
          case ("--watch-restart-delay", "--watch-restart-max", "--watch-self", "--watch-log-format")
            skip_next = .true.
            cycle
          case default
            continue
         end select

         if (starts_with(a, "--watch-restart-delay=")) cycle
         if (starts_with(a, "--watch-restart-max=")) cycle
         if (starts_with(a, "--watch-self=")) cycle
         if (starts_with(a, "--watch-log-format=")) cycle

         copy_next = watch_option_takes_value(a)
         cmd = cmd // " " // quote_arg(a)
      end do
   end function build_child_command

   !> Determine the executable to use when spawning the child watcher.
   !!
   !! Precedence:
   !! 1. Explicit `exe0` argument
   !! 2. Environment `FPM_WATCH_SELF`
   !! 3. `argv[0]`
   !! 4. Fallback `"fpm-watch"`
   function choose_self_exe(exe0) result(exe)
      character(len=*), intent(in) :: exe0
      character(len=:), allocatable :: exe
      character(len=2048) :: buf
      integer :: n, stat
      character(len=:), allocatable :: a0

      if (len_trim(exe0) > 0) then
         exe = trim(exe0)
         return
      end if

      buf = ""
      n = 0
      stat = 0
      call get_environment_variable("FPM_WATCH_SELF", buf, length=n, status=stat)
      if (stat == 0 .and. n > 0) then
         exe = trim(buf(1:n))
         return
      end if

      a0 = get_arg(0)
      if (len_trim(a0) > 0) then
         exe = trim(a0)
         return
      end if

      exe = "fpm-watch"
   end function choose_self_exe

   !> Apply leading restart flags over manifest supervisor defaults.
   !!
   !! Stop scanning at the subcommand and skip other value-option arguments.
   !! Leading low-CPU CLI flags also select the supervisor's waiting backend;
   !! this routine does not read the manifest's low-cpu setting. A child marker
   !! prevents recursive supervision. Numeric parse errors retain fallback values.
   subroutine parse_restart_flags(auto_restart, restart_delay, restart_max, is_child, self_exe)
      logical, intent(out) :: auto_restart, is_child
      real,    intent(out) :: restart_delay
      integer, intent(out) :: restart_max
      character(len=:), allocatable, intent(out) :: self_exe

      integer :: narg, i
      character(len=:), allocatable :: a, v

      call get_restart_defaults(auto_restart, restart_delay, restart_max, self_exe)
      is_child = .false.

      narg = command_argument_count()
      i = 1
      do while (i <= narg)
         a = get_arg(i)

         if (.not. starts_with(a, "--watch-") .and. a /= "-q" .and. a /= "-v" .and. a /= "-vv") exit

         if (a == "--watch-low-cpu") then
            call set_low_cpu(.true.)
            i = i + 1
            cycle
         end if

         if (a == "--watch-no-low-cpu") then
            call set_low_cpu(.false.)
            i = i + 1
            cycle
         end if

         if (starts_with(a, "--watch-low-cpu=")) then
            v = a(len("--watch-low-cpu=")+1:)
            call set_low_cpu(parse_bool(v, .true.))
            i = i + 1
            cycle
         end if

         if (a == "--watch-auto-restart") then
            auto_restart = .true.
            i = i + 1
            cycle
         end if

         if (starts_with(a, "--watch-auto-restart=")) then
            v = a(len("--watch-auto-restart=")+1:)
            auto_restart = parse_bool(v, .true.)
            i = i + 1
            cycle
         end if

         if (a == "--watch-child" .or. starts_with(a, "--watch-child=")) then
            is_child = .true.
            i = i + 1
            cycle
         end if

         if (starts_with(a, "--watch-restart-delay=")) then
            v = a(len("--watch-restart-delay=")+1:)
            restart_delay = parse_real(v, restart_delay)
            i = i + 1
            cycle
         end if

         if (starts_with(a, "--watch-restart-max=")) then
            v = a(len("--watch-restart-max=")+1:)
            restart_max = parse_int(v, restart_max)
            i = i + 1
            cycle
         end if

         if (starts_with(a, "--watch-self=")) then
            self_exe = trim(a(len("--watch-self=")+1:))
            i = i + 1
            cycle
         end if

         if (a == "--watch-restart-delay") then
            if (i+1 <= narg) then
               v = get_arg(i+1)
               restart_delay = parse_real(v, restart_delay)
               i = i + 2
            else
               i = i + 1
            end if
            cycle
         end if

         if (a == "--watch-restart-max") then
            if (i+1 <= narg) then
               v = get_arg(i+1)
               restart_max = parse_int(v, restart_max)
               i = i + 2
            else
               i = i + 1
            end if
            cycle
         end if

         if (a == "--watch-self") then
            if (i+1 <= narg) then
               self_exe = trim(get_arg(i+1))
               i = i + 2
            else
               i = i + 1
            end if
            cycle
         end if

         if (watch_option_takes_value(a)) then
            i = i + 2
         else
            i = i + 1
         end if
      end do

   end subroutine parse_restart_flags

end module watch_restart
