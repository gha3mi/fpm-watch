!> Supervisor and auto-restart support.
!!
!! When enabled, `fpm-watch` can run under a supervisor loop that restarts the
!! watcher on failure (non-zero exit status).
!!
!! Configuration sources:
!! - CLI flags:
!!   - `--watch-auto-restart`
!!   - `--watch-restart-delay <sec>`
!!   - `--watch-restart-max <n>`
!!   - `--watch-self <path>`
!! - `fpm.toml` `[extra.fpm-watch]` keys:
!!   - `auto-restart`, `restart-delay`, `restart-max`, `self` / `self-exe`
!!
!! Low CPU mode is also applied here so the supervisor delay sleeps are idle.
module watch_restart
   use, intrinsic :: iso_fortran_env, only: error_unit
   use watch_time, only: sleep_seconds, set_low_cpu
   use watch_cmdline, only: get_arg, parse_int, parse_real, quote_arg, starts_with
   use watch_config, only: get_restart_defaults
   implicit none
   private
   public maybe_supervise

contains

   !> Enter supervisor mode when requested, otherwise return immediately.
   !!
   !! If auto-restart is enabled and the current process is not marked as a
   !! child (`--watch-child`), this routine runs a supervisor loop and does not
   !! return.
   subroutine maybe_supervise()
      logical :: auto_restart, is_child
      real    :: restart_delay
      integer :: restart_max
      character(len=:), allocatable :: self_exe

      call parse_restart_flags(auto_restart, restart_delay, restart_max, is_child, self_exe)

      if (auto_restart .and. (.not. is_child)) then
         call supervisor_loop(max(0.0, restart_delay), restart_max, self_exe)
         stop 0
      end if
   end subroutine maybe_supervise

   !> Run the supervisor loop, restarting the child on non-zero exit.
   !!
   !! The supervisor will:
   !! - Execute the child command.
   !! - Exit if the child returns `0`.
   !! - Otherwise wait `delay0` seconds and restart, until `max0` is reached.
   subroutine supervisor_loop(delay0, max0, exe0)
      real, intent(in) :: delay0
      integer, intent(in) :: max0
      character(len=*), intent(in) :: exe0

      character(len=:), allocatable :: child_cmd
      integer :: code, attempt
      real :: delay

      child_cmd = build_child_command(exe0)
      attempt = 0
      delay = max(0.0, delay0)

      do
         code = 0
         call execute_command_line(child_cmd, exitstat=code)

         if (code == 0) exit

         attempt = attempt + 1
         if (max0 > 0 .and. attempt >= max0) then
            write(error_unit,'(a)') "fpm-watch: child exited with nonzero status; restart limit reached"
            stop code
         end if

         write(error_unit,'(a,i0,a,i0,a,f0.2,a)') "fpm-watch: child crashed/aborted (exit=", code, "), restart #", attempt, " in ", delay, "s"
         call sleep_seconds(delay)
      end do

      stop 0
   end subroutine supervisor_loop

   !> Build the command line used to spawn the supervised child.
   !!
   !! The child receives `--watch-child` and inherits most argv tokens, with
   !! supervisor-only flags removed to avoid recursion.
   function build_child_command(exe0) result(cmd)
      character(len=*), intent(in) :: exe0
      character(len=:), allocatable :: cmd
      character(len=:), allocatable :: exe, a
      integer :: narg, i
      logical :: skip_next

      exe = choose_self_exe(exe0)
      cmd = quote_arg(exe) // " --watch-child"

      narg = command_argument_count()
      skip_next = .false.

      do i = 1, narg
         a = get_arg(i)

         if (skip_next) then
            skip_next = .false.
            cycle
         end if

         if (a == "--watch-auto-restart") cycle
         if (a == "--watch-child") cycle

         if (a == "--watch-restart-delay") then
            skip_next = .true.
            cycle
         end if
         if (a == "--watch-restart-max") then
            skip_next = .true.
            cycle
         end if
         if (a == "--watch-self") then
            skip_next = .true.
            cycle
         end if

         if (starts_with(a, "--watch-restart-delay=")) cycle
         if (starts_with(a, "--watch-restart-max=")) cycle
         if (starts_with(a, "--watch-self=")) cycle

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

   !> Parse restart-related flags and apply manifest defaults.
   !!
   !! This routine also applies `--watch-low-cpu` to the sleep implementation
   !! so supervisor delays do not busy-wait.
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

         if (a == "--watch-child") then
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

         i = i + 1
      end do

   contains

      pure logical function parse_bool(s, default) result(vb)
         character(len=*), intent(in) :: s
         logical, intent(in) :: default
         character(len=:), allocatable :: t
         t = trim(adjustl(lower_ascii(s)))
         select case (t)
          case ("1","true","on","yes","y","t")
            vb = .true.
          case ("0","false","off","no","n","f")
            vb = .false.
          case default
            vb = default
         end select
      end function parse_bool

      pure function lower_ascii(s) result(r)
         character(len=*), intent(in) :: s
         character(len=len(s)) :: r
         integer :: k, c, da
         da = iachar('a') - iachar('A')
         do k = 1, len(s)
            c = iachar(s(k:k))
            if (c >= iachar('A') .and. c <= iachar('Z')) then
               r(k:k) = achar(c + da)
            else
               r(k:k) = s(k:k)
            end if
         end do
      end function lower_ascii

   end subroutine parse_restart_flags

end module watch_restart
