!> Portable synchronous process execution and stdout redirection.
!!
!! C wrappers launch commands, forward user interrupts, and redirect raw fpm
!! library output. An interrupt received while waiting for a command latches
!! `stop_requested`; ordinary nonzero child exits do not. OS detection uses
!! `fpm_environment:get_os_type`.
module watch_platform
   use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t, c_null_char
   use, intrinsic :: iso_fortran_env, only: output_unit
   use watch_log, only: log_event, log_int, log_string
   use fpm_environment, only: get_os_type, OS_WINDOWS, get_env
   use fpm_error, only: error_t, fatal_error
   use fpm_filesystem, only: exists, join_path
   implicit none
   private
   public is_windows, execute_command, termination_requested
   public begin_raw_output, end_raw_output, create_temp_directory, remove_temp_directory, set_temp_directory
   public set_workspace_root
   public current_executable, stop_file_exists

   !> Latched cancellation request; ordinary child exit codes do not set it.
   logical, save :: stop_requested = .false.

   interface
      !> Resolve the running native image; return zero or an OS error without truncation.
      function fpm_watch_current_executable(path, capacity) bind(C, name="fpm_watch_current_executable") result(error)
         import c_char, c_int, c_size_t
         implicit none
         ! allow(C071, C072)
         character(kind=c_char), intent(out) :: path(*)
         integer(c_size_t), value :: capacity
         integer(c_int) :: error
      end function fpm_watch_current_executable
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
      !> Copy the last failed native launch operation into a bounded NUL-terminated buffer.
      subroutine fpm_watch_launch_operation(buffer, capacity) bind(C, name="fpm_watch_launch_operation")
         import c_char, c_size_t
         implicit none
         ! allow(C071, C072)
         character(kind=c_char), intent(out) :: buffer(*)
         integer(c_size_t), value :: capacity
      end subroutine fpm_watch_launch_operation
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
      !> Reserve a uniquely named private temporary directory; return an OS error or zero.
      function fpm_watch_create_temp_directory(path, capacity) bind(C, name="fpm_watch_create_temp_directory") result(error)
         import c_char, c_int, c_size_t
         implicit none
         !> The C helper writes at most the caller-provided capacity, including NUL.
         ! allow(C071, C072)
         character(kind=c_char), intent(out) :: path(*)
         integer(c_size_t), value :: capacity
         integer(c_int) :: error
      end function fpm_watch_create_temp_directory
      !> Remove a workspace without following links; a missing path is already clean.
      function fpm_watch_remove_temp_directory(path) bind(C, name="fpm_watch_remove_temp_directory") result(error)
         import c_char, c_int
         implicit none
         ! allow(C071)
         character(kind=c_char), intent(in) :: path(*)
         integer(c_int) :: error
      end function fpm_watch_remove_temp_directory
      !> Set all native temporary-directory variables inside a graph worker.
      function fpm_watch_set_temp_directory(path) bind(C, name="fpm_watch_set_temp_directory") result(error)
         import c_char, c_int
         implicit none
         ! allow(C071)
         character(kind=c_char), intent(in) :: path(*)
         integer(c_int) :: error
      end function fpm_watch_set_temp_directory
      !> Select an inherited supervisor workspace, or clear it with an empty path.
      function fpm_watch_set_workspace_root(path, defined) bind(C, name="fpm_watch_set_workspace_root") result(error)
         import c_char, c_int
         implicit none
         ! allow(C071)
         character(kind=c_char), intent(in) :: path(*)
         integer(c_int), value :: defined
         integer(c_int) :: error
      end function fpm_watch_set_workspace_root
   end interface

contains

   !> Return the absolute native executable path for internal worker and supervisor launches.
   function current_executable(error) result(path)
      type(error_t), allocatable, intent(out) :: error
      character(len=:), allocatable :: path
      character(kind=c_char) :: buffer(32768)
      integer(c_int) :: status
      character(len=32) :: code

      path = ""
      status = fpm_watch_current_executable(buffer, int(size(buffer), c_size_t))
      if (status /= 0_c_int) then
         write(code, "(i0)") status
         call fatal_error(error, "Cannot locate the running watcher executable (OS error " // trim(code) // ")")
         return
      end if
      path = from_c_string(buffer)
   end function current_executable

   !> Copy a bounded NUL-terminated C path into an allocated Fortran string.
   pure function from_c_string(buffer) result(path)
      character(kind=c_char), intent(in) :: buffer(:)
      character(len=:), allocatable :: path
      integer :: i, n
      n = 0
      do i = 1, size(buffer)
         if (buffer(i) == c_null_char) exit
         n = n + 1
      end do
      path = repeat(" ", n)
      do i = 1, n
         path(i:i) = buffer(i)
      end do
   end function from_c_string

   !> Check the stop marker in the selected project, independently of the process directory.
   logical function stop_file_exists(project_dir) result(stopped)
      character(len=*), intent(in) :: project_dir
      stopped = exists(join_path(project_dir, ".fpm-watch.stop"))
   end function stop_file_exists

   !> Wait for a command and return its status while latching handled user interrupts.
   !!
   !! `silent` suppresses both output streams and takes precedence over `stderr`,
   !! which redirects command stdout to stderr. Both options default to false.
   !! Launch/wait errors log `command.launch_failed` and return 127; ordinary
   !! failures preserve the child status. Optional `interrupted` reports this
   !! command's handled interruption, while `termination_requested` remains latched.
   !!
   !! POSIX uses a managed process group and a foreground monitor around `/bin/sh`.
   !! HUP/INT/QUIT/TERM reach the group, including nested supervisors; cancellation
   !! escalates after one second. Terminal ownership is restored after execution.
   !! Windows launches executables directly and safely encodes batch-script argv.
   !! Console cancellation terminates its job. Normal exits retain background children.
   subroutine execute_command(cmd, exitstat, interrupted, silent, stderr, inherit_workspace)
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
      logical, intent(in), optional :: inherit_workspace
      !! Keep supervisor workspace ownership for internal watcher/graph launches (default true).
      !! External commands clear that internal setting so surviving descendants own their workspaces.
      integer(c_int) :: status, was_interrupted, launch_error, silent_mode
      character(kind=c_char) :: operation(64)
      character(len=:), allocatable :: effective_cmd, inherited_root
      logical :: isolate_workspace, root_defined
      integer :: environment_status
      type(error_t), allocatable :: error, restore_error

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
      isolate_workspace = .false.
      if (present(inherit_workspace)) isolate_workspace = .not. inherit_workspace
      if (present(interrupted)) interrupted = .false.
      if (isolate_workspace) then
         inherited_root = get_env("FPM_WATCH_WORKSPACE_ROOT")
         call get_environment_variable("FPM_WATCH_WORKSPACE_ROOT", status=environment_status)
         root_defined = environment_status == 0
         call set_workspace_root("", error)
         if (allocated(error)) then
            call set_workspace_root(inherited_root, restore_error, defined=root_defined)
            call log_event("error", "command.workspace_failed", error%message)
            exitstat = 127
            return
         end if
      end if
      status = fpm_watch_execute(effective_cmd // c_null_char, silent_mode, was_interrupted, launch_error)
      stop_requested = stop_requested .or. was_interrupted /= 0_c_int
      if (present(interrupted)) interrupted = was_interrupted /= 0_c_int
      if (isolate_workspace) then
         call set_workspace_root(inherited_root, error, defined=root_defined)
         if (allocated(error)) then
            call log_event("error", "command.workspace_failed", error%message)
            exitstat = 127
            return
         end if
      end if
      if (launch_error /= 0_c_int) then
         call fpm_watch_launch_operation(operation, int(size(operation), c_size_t))
         call log_event("error", "command.launch_failed", "Cannot execute command: " // trim(cmd), &
                      [log_int("os_error", int(launch_error)), log_string("operation", from_c_string(operation)), &
                       log_string("command", trim(cmd))])
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

   !> Reserve a private temporary directory and return its absolute path.
   !!
   !! POSIX permissions are restricted to its owner. Pair success with
   !! `remove_temp_directory`, including cancellation and failure paths.
   subroutine create_temp_directory(path, error)
      character(len=:), allocatable, intent(out) :: path
      type(error_t), allocatable, intent(out) :: error
      character(kind=c_char) :: buffer(32768)
      character(len=32) :: code
      integer(c_int) :: status

      path = ""
      status = fpm_watch_create_temp_directory(buffer, int(size(buffer), c_size_t))
      if (status /= 0_c_int) then
         write(code, "(i0)") status
         call fatal_error(error, "Cannot create watcher workspace (OS error " // trim(code) // &
                          "); check the temporary directory and its permissions")
         return
      end if
      path = from_c_string(buffer)
   end subroutine create_temp_directory

   !> Remove a workspace and its contents without following links, tolerating repeated cleanup.
   subroutine remove_temp_directory(path)
      character(len=*), intent(in) :: path
      integer(c_int) :: status
      if (len(path) == 0) return
      status = fpm_watch_remove_temp_directory(path // c_null_char)
      if (status /= 0_c_int) then
         call log_event("warn", "temp.cleanup_failed", "Cannot remove watcher workspace: " // path, &
                        [log_int("os_error", int(status)), log_string("path", path)])
      end if
   end subroutine remove_temp_directory

   !> Direct native temporary files into the worker's private workspace.
   !!
   !! Call only in the isolated worker; this updates process-wide TMPDIR/TMP/TEMP.
   subroutine set_temp_directory(path, error)
      character(len=*), intent(in) :: path
      type(error_t), allocatable, intent(out) :: error
      integer(c_int) :: status
      character(len=32) :: code
      status = fpm_watch_set_temp_directory(path // c_null_char)
      if (status /= 0_c_int) then
         write(code, "(i0)") status
         call fatal_error(error, "Cannot select graph workspace (OS error " // trim(code) // "): " // path)
      end if
   end subroutine set_temp_directory

   !> Nest descendant workspaces under an owner that survives forced child termination.
   !!
   !! This internal setting leaves the user's native temporary-directory variables
   !! unchanged. Restore the inherited value after each supervised execution.
   subroutine set_workspace_root(path, error, defined)
      character(len=*), intent(in) :: path
      type(error_t), allocatable, intent(out) :: error
      logical, intent(in), optional :: defined
      !! Explicit presence preserves a previously defined empty environment value.
      integer(c_int) :: status
      integer(c_int) :: present_value
      character(len=32) :: code
      present_value = merge(1_c_int, 0_c_int, len(path) > 0)
      if (present(defined)) present_value = merge(1_c_int, 0_c_int, defined)
      status = fpm_watch_set_workspace_root(path // c_null_char, present_value)
      if (status /= 0_c_int) then
         write(code, "(i0)") status
         call fatal_error(error, "Cannot select supervisor workspace (OS error " // trim(code) // "): " // path)
      end if
   end subroutine set_workspace_root

   !> Return the process-wide interruption flag latched by prior command executions.
   logical function termination_requested() result(requested)
      requested = stop_requested
   end function termination_requested

   !> Return whether the current OS is Windows.
   logical function is_windows()
      is_windows = (get_os_type() == OS_WINDOWS)
   end function is_windows

end module watch_platform
