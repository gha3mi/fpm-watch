!> Select affected targets, execute fpm, and emit command-cycle events.
!!
!! Commands run synchronously through `watch_platform`. Structured logging
!! sends raw command stdout to stderr; optional boundary markers correlate
!! that stream with command events. Target injection preserves explicit names.
module watch_exec
   use, intrinsic :: iso_fortran_env, only: int64, error_unit
   use watch_types, only: watch_opts_t, root_info_t
   use watch_util, only: ftoa
   use watch_cmdline, only: quote_arg
   use watch_platform, only: execute_command, termination_requested
   use fpm_strings, only: string_t, str
   use fpm_command_line, only: fpm_build_settings, fpm_test_settings, fpm_run_settings
   use watch_log, only: log_event, log_int, log_real, log_string, log_strings, log_bool, structured_logging
   implicit none
   private
   public report_changes, run_command_and_report, build_run_command
   !> Process-local command counter, incremented before each start event.
   integer(int64), save :: cycle_number = 0_int64

contains

   !> Emit changed paths and a summary unless quiet mode suppresses them.
   !!
   !! `changed_idx(:changed_count)` contains 1-based indices into `files`.
   !! Structured summaries include every valid path; verbosity 1 emits individual records
   !! for at most the first eight changed indices.
   subroutine report_changes(files, changed_idx, changed_count, w)
      type(string_t), allocatable, intent(in) :: files(:)
      integer, intent(in) :: changed_idx(:)
      integer, intent(in) :: changed_count
      type(watch_opts_t), intent(in) :: w
      integer :: i, idx, n
      type(string_t), allocatable :: paths(:)
      character(len=:), allocatable :: message
      if (w%verbosity < 0 .or. changed_count == 0) return
      if (.not. allocated(files)) return
      idx = changed_idx(1)
      message = str(changed_count) // " files changed"
      if (idx >= 1 .and. idx <= size(files)) then
         if (changed_count == 1) message = "Changed " // files(idx)%s
      end if
      if (structured_logging()) then
         allocate(paths(changed_count))
         n = 0
         do i = 1, changed_count
            idx = changed_idx(i)
            if (idx < 1 .or. idx > size(files)) cycle
            n = n + 1
            paths(n) = files(idx)
         end do
         call log_event("info", "watch.changed", message, &
                        [log_int("file_count", changed_count), log_strings("paths", paths(:n)), &
                         log_real("debounce_ms", w%debounce * 1000.0)])
      else
         call log_event("info", "watch.changed", message)
      end if
      do i = 1, min(changed_count, 8)
         idx = changed_idx(i)
         if (idx < 1 .or. idx > size(files)) cycle
         call log_event("info", "watch.changed_file", "Changed " // files(idx)%s, &
                        [log_string("path", files(idx)%s)], verbosity=1)
      end do
   end subroutine report_changes

   !> Execute synchronously and report start, completion, exit status, and seconds.
   !!
   !! `cycle_id` combines the supervisor generation and a process-local counter.
   !! Structured mode sends raw command stdout to stderr; silent mode suppresses
   !! both child streams. In structured mode only, output markers bracket stderr
   !! and force both command events through quiet filtering. Failures are errors;
   !! a latched user interruption is reported as an interrupted completion.
   subroutine run_command_and_report(cmd, w, exitstat, secs)
      character(len=*), intent(in) :: cmd
      !! Complete serialized command to execute synchronously.
      type(watch_opts_t), intent(in) :: w
      !! Output suppression, cycle generation, and optional output-marker settings.
      integer, intent(out) :: exitstat
      !! Command status, including 127 when the platform wrapper cannot launch or wait.
      real, intent(out) :: secs
      !! Elapsed command time in seconds, excluding event formatting and boundary writes.
      integer(int64) :: rate, t0, t1
      character(len=:), allocatable :: action, message, level, cycle_id
      logical :: interrupted, protocol

      action = "Command"
      if (index(cmd, "fpm build") == 1) action = "Build"
      if (index(cmd, "fpm test") == 1) action = "Tests"
      if (index(cmd, "fpm run") == 1) action = "Application"
      cycle_number = cycle_number + 1_int64
      cycle_id = str(w%restart_count) // ":" // str(cycle_number)
      protocol = w%output_markers .and. structured_logging()
      call log_event("info", "command.started", "Running " // trim(cmd), &
                     [log_string("command", trim(cmd)), log_bool("silent_fpm", w%silent_fpm), &
                      log_string("cycle_id", cycle_id)], force=protocol)
      call output_boundary("started", cycle_id, protocol)
      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64
      call system_clock(t0)
      call execute_command(trim(cmd), exitstat, silent=w%silent_fpm, stderr=structured_logging(), &
                           inherit_workspace=.false.)
      call system_clock(t1)
      secs = real(t1 - t0) / real(rate)
      interrupted = termination_requested()
      level = "info"
      message = action // " succeeded in " // ftoa(secs) // "s"
      if (exitstat /= 0) then
         level = "error"
         message = action // " failed (exit " // str(exitstat) // ") after " // ftoa(secs) // "s"
      end if
      if (interrupted) then
         level = "info"
         message = action // " interrupted"
      end if
      call log_event(level, "command.finished", message, &
                     [log_string("command", trim(cmd)), log_int("exit_code", exitstat), &
                      log_int("duration_ms", nint(secs * 1000.0, int64)), log_bool("interrupted", interrupted), &
                      log_string("cycle_id", cycle_id)], force=protocol)
      call output_boundary("finished", cycle_id, protocol)
   end subroutine run_command_and_report

   !> Flush a stderr boundary marker when the structured output protocol is enabled.
   !!
   !! The marker follows a blank line and contains the same opaque `cycle_id`
   !! as the command events; stdout and stderr delivery order need not match.
   subroutine output_boundary(state, cycle_id, enabled)
      character(len=*), intent(in) :: state, cycle_id
      logical, intent(in) :: enabled
      if (.not. enabled) return
      write(error_unit, "(a)") ""
      write(error_unit, "(a)") "@@fpm-watch:" // state // ":" // cycle_id // "@@"
      flush(error_unit)
   end subroutine output_boundary

   !> Inject affected root names into an unpinned run/test command.
   !!
   !! `changed_idx(:changed_count)` indexes `file_mask`, whose bits correspond
   !! to `roots`. Combine those bits and quote the matching root names before
   !! the program-argument `--` delimiter. Return `full_cmdline` unchanged for
   !! build mode, explicit target patterns, unavailable masks/roots, a changed
   !! zero-mask file (global metadata), no matched names, or an empty prefix.
   function build_run_command(settings, full_cmdline, cmd_prefix, cmd_rest, roots, file_mask, changed_idx, changed_count) result(cmd)
      use watch_cmdsplice, only: inject_names_into_cmd
      class(fpm_build_settings), intent(in) :: settings
      !! Run/test type and explicit target patterns used to decide whether injection is allowed.
      character(len=*), intent(in) :: full_cmdline
      !! Complete serialized command, returned when no target injection is possible.
      character(len=*), intent(in) :: cmd_prefix
      !! Serialized executable/subcommand prefix for the replacement command.
      character(len=*), intent(in) :: cmd_rest
      !! Serialized forwarded fpm/program arguments after the subcommand.
      type(root_info_t), allocatable, intent(in) :: roots(:)
      !! Root executable names and their single-bit membership masks.
      integer(int64), allocatable, intent(in) :: file_mask(:)
      !! Root bits per watched file, indexed by changed_idx.
      integer, intent(in) :: changed_idx(:)
      !! 1-based file indices; only the first changed_count entries are read.
      integer, intent(in) :: changed_count
      !! Number of changed indices in the active buffer prefix.
      character(len=:), allocatable :: cmd

      logical :: is_test, is_run
      integer(int64) :: hitmask
      character(len=:), allocatable :: names_str
      integer :: j, idx
      logical :: user_pinned_names

      is_test = .false.
      is_run  = .false.
      user_pinned_names = .false.

      select type (s => settings)
       type is (fpm_test_settings)
         is_test = .true.
         if (allocated(s%name)) user_pinned_names = (size(s%name) > 0)
       type is (fpm_run_settings)
         is_run = .true.
         if (allocated(s%name)) user_pinned_names = (size(s%name) > 0)
       class default
         continue
      end select

      if (user_pinned_names) then
         cmd = trim(full_cmdline)
         return
      end if

      if (.not. is_test .and. .not. is_run) then
         cmd = trim(full_cmdline)
         return
      end if

      if (.not. allocated(roots)) then
         cmd = trim(full_cmdline)
         return
      end if

      if (size(roots) == 0) then
         cmd = trim(full_cmdline)
         return
      end if

      if (.not. allocated(file_mask)) then
         cmd = trim(full_cmdline)
         return
      end if

      hitmask = 0_int64
      do j = 1, changed_count
         idx = changed_idx(j)
         if (idx < 1 .or. idx > size(file_mask)) cycle
         if (file_mask(idx) == 0_int64) then
            cmd = trim(full_cmdline)
            return
         end if
         hitmask = ior(hitmask, file_mask(idx))
      end do

      if (hitmask == 0_int64) then
         cmd = trim(full_cmdline)
         return
      end if

      names_str = join_names_from_mask(roots, hitmask)
      if (len_trim(names_str) == 0) then
         cmd = trim(full_cmdline)
         return
      end if

      if (len_trim(cmd_prefix) == 0) then
         cmd = trim(full_cmdline)
         return
      end if

      cmd = inject_names_into_cmd(cmd_prefix, cmd_rest, names_str)

   contains

      !> Join quoted, nonempty root names whose bits intersect `mask`, in root order.
      function join_names_from_mask(roots, mask) result(s)
         type(root_info_t), intent(in) :: roots(:)
         integer(int64), intent(in) :: mask
         character(len=:), allocatable :: s
         integer :: rr
         character(len=:), allocatable :: nm

         s = ""
         do rr = 1, size(roots)
            if (iand(mask, roots(rr)%mask) == 0_int64) cycle
            if (.not. allocated(roots(rr)%name)) cycle
            nm = trim(roots(rr)%name)
            if (len_trim(nm) == 0) cycle
            if (len(s) > 0) s = s // " "
            s = s // quote_arg(nm)
         end do
      end function join_names_from_mask

   end function build_run_command

end module watch_exec
