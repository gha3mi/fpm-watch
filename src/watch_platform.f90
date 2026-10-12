!> Portable synchronous process execution and stdout redirection.
!!
!! C wrappers launch commands, forward user interrupts, and redirect raw fpm
!! library output. An interrupt received while waiting for a command latches
!! `stop_requested`; ordinary nonzero child exits do not. OS detection uses
!! `fpm_environment:get_os_type`.
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
      !> C process wrapper returning child status or 127 for a launch/wait error.
      !!
      !! `command` is NUL-terminated. On Windows, `silent` selects inherited streams:
      !! 0 normal, 1 discard both outputs, 2 redirect stdout to stderr. POSIX ignores
      !! this mode because the Fortran wrapper appends shell redirections.
      !! `interrupted` reports handled user interrupts; `launch_error` holds the OS
      !! error code separately from ordinary child failure.
      function fpm_watch_execute(command, silent, interrupted, launch_error) bind(C, name="fpm_watch_execute") result(status)
         import c_char, c_int
         implicit none
         !> NUL-terminated command bytes interoperating with a C string.
         ! allow(C071)
         character(kind=c_char), intent(in) :: command(*)
         integer(c_int), value :: silent
         integer(c_int), intent(out) :: interrupted
         integer(c_int), intent(out) :: launch_error
         integer(c_int) :: status
      end function fpm_watch_execute
      !> Redirect descriptor 1 to stderr; return a saved stdout descriptor or a negative OS error.
      function fpm_watch_redirect_stdout() bind(C, name="fpm_watch_redirect_stdout") result(saved)
         import c_int
         implicit none
         integer(c_int) :: saved
      end function fpm_watch_redirect_stdout
      !> Restore and close a saved stdout descriptor; return zero on success or an OS error.
      function fpm_watch_restore_stdout(saved) bind(C, name="fpm_watch_restore_stdout") result(error)
         import c_int
         implicit none
         integer(c_int), value :: saved
         integer(c_int) :: error
      end function fpm_watch_restore_stdout
   end interface

contains

   !> Wait for a command and return its status while latching handled user interrupts.
   !!
   !! `silent` suppresses both output streams and takes precedence over `stderr`,
   !! which redirects command stdout to stderr. Both options default to false.
   !! Launch/wait errors log `command.launch_failed` and return 127; ordinary
   !! failures preserve the child status. Optional `interrupted` reports this
   !! command's handled interruption, while `termination_requested` remains latched.
   !!
   !! POSIX uses `/bin/sh -c` and temporarily forwards SIGINT/SIGQUIT/SIGTERM to
   !! the immediate child in the foreground process group. Windows launches
   !! executables directly, with a cmd fallback for scripts, and terminates the
   !! child's job on console interruption. Normal exits retain background children.
   subroutine execute_command(cmd, exitstat, interrupted, silent, stderr)
      character(len=*), intent(in) :: cmd
      !! Serialized command; caller is responsible for quoting individual argv tokens.
      integer, intent(out) :: exitstat
      !! Child/interrupt status, or 127 with a launch-failure event for OS execution errors.
      logical, intent(out), optional :: interrupted
      !! True if the wrapper handled a user interrupt during this command.
      logical, intent(in), optional :: silent
      !! Suppress both command output streams (default false); takes precedence over stderr.
      logical, intent(in), optional :: stderr
      !! Redirect command stdout to stderr (default false).
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

   !> Optionally redirect process stdout to stderr, flushing Fortran output first.
   !!
   !! Return a saved C descriptor in `saved`, or -1 when redirection is disabled.
   !! Pair successful redirection with `end_raw_output`. Redirection failure logs
   !! an error and exits with status 127; this is process-wide, not thread-local.
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

   !> Flush raw library output, restore stdout, and close the saved C descriptor.
   !!
   !! Negative `saved` values are no-ops. Restoration failure logs an error and
   !! exits with status 127; a saved descriptor must only be restored once.
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

   !> Return the process-wide interruption flag latched by prior command executions.
   logical function termination_requested() result(requested)
      requested = stop_requested
   end function termination_requested

   !> Return whether the current OS is Windows.
   logical function is_windows()
      is_windows = (get_os_type() == OS_WINDOWS)
   end function is_windows

end module watch_platform
