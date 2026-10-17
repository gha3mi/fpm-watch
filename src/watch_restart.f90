!> Supervise watcher children and restart them on nonzero exit.
!!
!! Leading CLI flags override manifest supervisor defaults. A zero child exit
!! or user interruption ends supervision; other exits restart after the delay,
!! subject to the restart limit. Command failures only reach the supervisor
!! if the watcher itself exits nonzero, as in one-shot mode. Child generations
!! share the parent's selected log format and use distinct cycle-ID prefixes.
module watch_restart
   use watch_time, only: sleep_seconds
   use watch_cmdline, only: get_arg, quote_arg, starts_with, &
                            watch_option_takes_value
   use watch_types, only: watch_config_t
   use watch_platform, only: execute_command, create_temp_directory, remove_temp_directory, set_workspace_root
   use watch_log, only: log_event, log_int, log_real, log_string
   use fpm_strings, only: str
   use fpm_environment, only: get_env
   use fpm_error, only: error_t
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
   subroutine maybe_supervise(cfg)
      type(watch_config_t), intent(in) :: cfg

      if (cfg%w%auto_restart .and. (.not. cfg%w%is_child)) then
         call supervisor_loop(cfg%w%restart_delay, cfg%w%restart_max, cfg%w%self_exe, cfg%w%log_format)
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
         call execute_supervised_child(child_cmd, code, interrupted)

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

   !> Own all descendant graph workspaces until the complete child execution ends.
   !!
   !! A forced group/job termination can prevent child cleanup. The surviving
   !! supervisor removes the entire private tree, including nested supervisors'
   !! workspaces, after execution and restores its inherited workspace selection.
   subroutine execute_supervised_child(command, code, interrupted)
      character(len=*), intent(in) :: command
      integer, intent(out) :: code
      logical, intent(out) :: interrupted
      character(len=:), allocatable :: workspace, inherited_root
      logical :: root_defined
      integer :: environment_status
      type(error_t), allocatable :: error

      code = 127
      interrupted = .false.
      inherited_root = get_env("FPM_WATCH_WORKSPACE_ROOT")
      call get_environment_variable("FPM_WATCH_WORKSPACE_ROOT", status=environment_status)
      root_defined = environment_status == 0
      call create_temp_directory(workspace, error)
      if (.not. allocated(error)) call set_workspace_root(workspace, error)
      if (allocated(error)) then
         call log_event("error", "supervisor.workspace_failed", error%message)
         call remove_temp_directory(workspace)
         stop 127
      end if
      call execute_command(command, code, interrupted)
      call set_workspace_root(inherited_root, error, defined=root_defined)
      call remove_temp_directory(workspace)
      if (allocated(error)) then
         call log_event("error", "supervisor.workspace_failed", error%message)
         stop 127
      end if
   end subroutine execute_supervised_child

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

end module watch_restart
