!> Supervise watcher children and restart them on nonzero exit.
!!
!! Leading CLI flags override manifest supervisor defaults. A zero child exit
!! or user interruption ends supervision; other exits restart after the delay,
!! subject to the restart limit. Command failures only reach the supervisor
!! if the watcher itself exits nonzero, as in one-shot mode. Child generations
!! share the parent's selected log format and use distinct cycle-ID prefixes.
module watch_restart
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use watch_time, only: sleep_seconds
   use watch_cmdline, only: get_arg, quote_arg, starts_with, &
                            watch_option_takes_value
   use watch_types, only: watch_config_t
   use watch_platform, only: execute_command, create_temp_directory, remove_temp_directory, set_workspace_root, &
                             current_executable, stop_file_exists
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
         call supervisor_loop(cfg%w%restart_delay, cfg%w%restart_max, cfg%project_dir, cfg%w%log_format)
         stop 0
      end if
   end subroutine maybe_supervise

   !> Launch watcher children until success, user interruption, or restart exhaustion.
   !!
   !! `max0` counts restarts after the initial launch; nonpositive values are
   !! unlimited. Clamp `delay0` to zero or greater. Each restart increments the
   !! child generation. A user interruption or exhausted limit exits with the
   !! child execution status; a zero child status exits successfully.
   !! Status propagation is quiet so runtime diagnostics cannot fail on a
   !! disconnected terminal and replace the child's exit status.
   subroutine supervisor_loop(delay0, max0, project_dir, log_format)
      real, intent(in) :: delay0
      integer, intent(in) :: max0
      character(len=*), intent(in) :: project_dir
      character(len=*), intent(in) :: log_format

      character(len=:), allocatable :: child_cmd, executable
      type(error_t), allocatable :: error
      integer :: code, attempt
      real :: delay
      logical :: interrupted

      executable = current_executable(error)
      if (allocated(error)) then
         call log_event("error", "supervisor.launch_failed", error%message)
         stop 127
      end if
      attempt = 0
      delay = max(0.0, delay0)

      do
         code = 0
         child_cmd = build_child_command(executable, log_format, attempt)
         call execute_supervised_child(child_cmd, code, interrupted)

         if (interrupted) then
            call log_event("info", "supervisor.stopped", "Supervisor interrupted", [log_string("reason", "interrupt")])
            stop code, quiet=.true.
         end if

         if (stop_file_exists(project_dir)) then
            call log_event("info", "supervisor.stopped", "Supervisor stopped", [log_string("reason", "stop_file")])
            exit
         end if
         if (code == 0) exit

         if (max0 > 0 .and. attempt >= max0) then
            call log_event("error", "supervisor.limit_reached", "Child failed; restart limit reached", &
                           [log_int("exit_code", code), log_int("restart_count", attempt)])
            stop code, quiet=.true.
         end if

         attempt = attempt + 1
         call log_event("warn", "supervisor.restart", &
                        "Child failed (exit " // str(code) // "); restart #" // str(attempt) // " in " // ftoa(delay) // "s", &
                        [log_int("exit_code", code), log_int("restart_count", attempt), log_real("delay_ms", delay * 1000.0)])
         if (wait_for_restart(delay, project_dir)) then
            call log_event("info", "supervisor.stopped", "Supervisor stopped", [log_string("reason", "stop_file")])
            exit
         end if
      end do

      stop 0
   end subroutine supervisor_loop

   !> Wait with low CPU use while checking the selected project's stop marker.
   logical function wait_for_restart(delay, project_dir) result(stopped)
      real, intent(in) :: delay
      character(len=*), intent(in) :: project_dir
      integer(int64) :: started, now, rate
      real(real64) :: elapsed
      real :: slice

      call system_clock(started, rate)
      elapsed = 0.0_real64
      do
         stopped = stop_file_exists(project_dir)
         if (stopped .or. elapsed >= real(delay, real64)) return
         slice = real(min(0.1_real64, real(delay, real64) - elapsed))
         call sleep_seconds(slice)
         if (rate > 0_int64) then
            call system_clock(now)
            elapsed = real(now - started, real64) / real(rate, real64)
         else
            elapsed = elapsed + real(slice, real64)
         end if
      end do
   end function wait_for_restart

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
   function build_child_command(executable, log_format, restart_count) result(cmd)
      character(len=*), intent(in) :: executable
      character(len=*), intent(in) :: log_format
      integer, intent(in) :: restart_count
      character(len=:), allocatable :: cmd
      character(len=:), allocatable :: a
      integer :: narg, i
      logical :: skip_next, copy_next, in_prefix

      cmd = quote_arg(executable) // " --watch-child=" // str(restart_count) // " --watch-log-format " // log_format

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
          case ("--watch-restart-delay", "--watch-restart-max", "--watch-log-format")
            skip_next = .true.
            cycle
          case default
            continue
         end select

         if (starts_with(a, "--watch-restart-delay=")) cycle
         if (starts_with(a, "--watch-restart-max=")) cycle
         if (starts_with(a, "--watch-log-format=")) cycle

         copy_next = watch_option_takes_value(a)
         cmd = cmd // " " // quote_arg(a)
      end do
   end function build_child_command

end module watch_restart
