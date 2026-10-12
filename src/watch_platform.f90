!> Platform-specific helpers.
!!
!! Provides interrupt-aware command execution, shell wrapping, and null-device
!! paths for portable command execution.
!!
!! This module is intentionally minimal and uses `fpm_environment:get_os_type`
!! for OS detection.
module watch_platform
   use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
   use, intrinsic :: iso_fortran_env, only: output_unit
   use watch_log, only: log_event, log_int, log_string
   use fpm_environment, only: get_os_type, OS_WINDOWS
   implicit none
   private
   public is_windows, execute_command, termination_requested
   public begin_raw_output, end_raw_output

   !> Latched cancellation request; ordinary child exit codes do not set it.
   logical, save :: stop_requested = .false.

   interface
      function fpm_watch_execute(command, silent, interrupted, launch_error) bind(C, name="fpm_watch_execute") result(status)
         import c_char, c_int
         implicit none
         !! An assumed-size character array interoperates with a C string.
         ! allow(C071)
         character(kind=c_char), intent(in) :: command(*)
         integer(c_int), value :: silent
         integer(c_int), intent(out) :: interrupted
         integer(c_int), intent(out) :: launch_error
         integer(c_int) :: status
      end function fpm_watch_execute
      function fpm_watch_redirect_stdout() bind(C, name="fpm_watch_redirect_stdout") result(saved)
         import c_int
         implicit none
         integer(c_int) :: saved
      end function fpm_watch_redirect_stdout
      function fpm_watch_restore_stdout(saved) bind(C, name="fpm_watch_restore_stdout") result(error)
         import c_int
         implicit none
         integer(c_int), value :: saved
         integer(c_int) :: error
      end function fpm_watch_restore_stdout
   end interface

contains

   !> Execute a command while preserving user interruption requests.
   !!
   !! The C wrapper waits for the child without ignoring console interrupts, as
   !! some implementations of synchronous `execute_command_line` do. Ordinary
   !! command failures are returned without requesting watcher termination.
   !! POSIX children share the foreground process group for interactive input;
   !! temporary signal handlers forward requests to the immediate child. On
   !! Windows, console requests terminate the child's job and its descendants.
   !! Uninterrupted command exits preserve the lifetime of background children.
   !! Original signal handlers are restored when the command finishes.
   !! Windows executables receive quoted argv directly; command scripts use a
   !! shell fallback. Silent output uses inherited null handles on Windows and
   !! shell redirection on POSIX.
   subroutine execute_command(cmd, exitstat, interrupted, silent, stderr)
      character(len=*), intent(in) :: cmd
      integer, intent(out) :: exitstat
      logical, intent(out), optional :: interrupted
      logical, intent(in), optional :: silent
      logical, intent(in), optional :: stderr
      integer(c_int) :: status, was_interrupted, launch_error, silent_mode
      character(len=:), allocatable :: effective_cmd

      silent_mode = 0_c_int
      if (present(stderr)) then
         if (stderr) silent_mode = 2_c_int
      end if
      if (present(silent)) then
         if (silent) silent_mode = 1_c_int
      end if
      effective_cmd = trim(cmd)
      if (.not. is_windows()) then
         if (silent_mode == 1_c_int) effective_cmd = effective_cmd // " >/dev/null 2>&1"
         if (silent_mode == 2_c_int) effective_cmd = effective_cmd // " 1>&2"
      end if
      status = fpm_watch_execute(effective_cmd // c_null_char, silent_mode, was_interrupted, launch_error)
      stop_requested = stop_requested .or. was_interrupted /= 0_c_int
      if (present(interrupted)) interrupted = was_interrupted /= 0_c_int
      if (launch_error /= 0_c_int) then
         call log_event("error", "command.launch_failed", "Cannot execute command: " // trim(cmd), &
                      [log_int("os_error", int(launch_error)), log_string("command", trim(cmd))])
         exitstat = 127
      else
         exitstat = int(status)
      end if
   end subroutine execute_command

   !> Temporarily send fpm library output to stderr during model operations.
   subroutine begin_raw_output(redirect, saved)
      logical, intent(in) :: redirect
      integer(c_int), intent(out) :: saved
      saved = -1_c_int
      if (.not. redirect) return
      flush(output_unit)
      saved = fpm_watch_redirect_stdout()
      if (saved < 0_c_int) then
         call log_event("error", "log.redirect_failed", "Cannot redirect fpm output to stderr", &
                      [log_int("os_error", int(-saved))])
         stop 127
      end if
   end subroutine begin_raw_output

   !> Flush fpm library output and restore the original stdout destination.
   subroutine end_raw_output(saved)
      integer(c_int), intent(in) :: saved
      integer(c_int) :: error
      if (saved < 0_c_int) return
      flush(output_unit)
      error = fpm_watch_restore_stdout(saved)
      if (error /= 0_c_int) then
         call log_event("error", "log.redirect_failed", "Cannot restore stdout after fpm model operation", &
                      [log_int("os_error", int(error))])
         stop 127
      end if
   end subroutine end_raw_output

   !> Return whether an executed command received a termination request.
   logical function termination_requested() result(requested)
      requested = stop_requested
   end function termination_requested

   !> Return whether the current OS is Windows.
   logical function is_windows()
      is_windows = (get_os_type() == OS_WINDOWS)
   end function is_windows

end module watch_platform
