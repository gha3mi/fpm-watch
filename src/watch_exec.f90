!> Command execution and run reporting.
!!
!! Provides the user-facing change summary and the mechanics to construct and
!! execute the effective `fpm` command.
module watch_exec
   use, intrinsic :: iso_fortran_env, only: int64, output_unit
   use watch_types, only: watch_opts_t, root_info_t
   use watch_util, only: ftoa
   use fpm_strings, only: string_t, str
   use fpm_filesystem, only: run
   use fpm_command_line, only: fpm_build_settings, fpm_test_settings, fpm_run_settings
   use face, only: colorize
   implicit none
   private
   public report_changes, run_command_and_report, build_run_command

contains

   !> Print a summary of changed files.
   !!
   !! The output is intentionally limited (cap=8) to keep logs compact.
   !! This routine is suppressed in quiet mode (`w%verbosity < 0`).
   subroutine report_changes(files, changed_idx, changed_count, w)
      type(string_t), allocatable, intent(in) :: files(:)
      integer, intent(in) :: changed_idx(:)
      integer, intent(in) :: changed_count
      type(watch_opts_t), intent(in) :: w
      integer :: i, cap, idx

      if (w%verbosity < 0) return

      cap = 8
      write(output_unit,'(a)') &
         colorize("change     |", color_fg='yellow_intense', style='bold_on') // "  " // &
         colorize(str(changed_count), color_fg='yellow_intense', style='bold_on') // &
         " files (debounced " // colorize(ftoa(w%debounce), color_fg='yellow') // "s)"

      if (changed_count == 0) return
      if (.not. allocated(files)) return

      do i = 1, min(changed_count, cap)
         idx = changed_idx(i)
         if (idx < 1 .or. idx > size(files)) cycle
         write(output_unit,'(a)') "           |  " // colorize(trim(files(idx)%s), color_fg='yellow')
      end do
      if (changed_count > cap) then
         write(output_unit,'(a)') &
            "           |  ... +" // colorize(str(changed_count-cap), color_fg='yellow') // " more"
      end if
   end subroutine report_changes

   !> Execute a command and print pre/post status lines.
   !!
   !! This routine is responsible for:
   !! - Printing a "run" line (unless quiet),
   !! - Executing the command via `fpm_filesystem:run`,
   !! - Timing the execution with `system_clock`,
   !! - Printing a final OK/FAIL summary and returning `exitstat` and `secs`.
   !!
   !! When `w%silent_fpm` is true, the executed command's output is suppressed
   !! (but the status lines from `fpm-watch` remain visible).
   subroutine run_command_and_report(cmd, w, exitstat, secs)
      character(len=*), intent(in) :: cmd
      type(watch_opts_t), intent(in) :: w
      integer, intent(out) :: exitstat
      real, intent(out) :: secs

      integer(int64) :: rate, t0, t1
      logical :: verbose_run
      character(len=:), allocatable :: status_color
      character(len=:), allocatable :: status_word

      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64

      verbose_run = .not. w%silent_fpm

      if (w%verbosity >= 0) then
         if (w%silent_fpm) then
            write(output_unit,'(a)') &
               colorize("run        |", color_fg='cyan_intense', style='bold_on') // "  " // &
               colorize(trim(cmd), color_fg='white_intense') // "  " // &
               colorize("(fpm output silenced)", color_fg='magenta_intense', style='italics_on')
         else
            write(output_unit,'(a)') &
               colorize("run        |", color_fg='cyan_intense', style='bold_on') // "  " // &
               colorize(trim(cmd), color_fg='white_intense')
         end if
      end if

      if (w%verbosity >= 0) flush(output_unit)

      call system_clock(t0)
      call run(trim(cmd), echo=.false., exitstat=exitstat, verbose=verbose_run)
      call system_clock(t1)

      secs = real(t1 - t0) / real(rate)

      if (exitstat == 0) then
         status_color = "green_intense"
         status_word  = "OK"
      else
         status_color = "red_intense"
         status_word  = "FAIL"
      end if

      if (w%verbosity >= 0) then
         write(output_unit,'(a)') ""
         write(output_unit,'(a)') &
            colorize("done       |", color_fg=status_color, style='bold_on') // &
            "  " // colorize(status_word, color_fg=status_color, style='bold_on') // &
            "  exit=" // colorize(str(exitstat), color_fg=status_color, style='bold_on') // &
            "  time=" // colorize(ftoa(secs), color_fg='yellow_intense') // "s"

         write(output_unit,'(a)') &
            colorize("command    |", color_fg='cyan_intense', style='bold_on') // "  " // &
            colorize(trim(cmd), color_fg='white_intense')

         write(output_unit,'(a)') &
            colorize("watch      |", color_fg='cyan_intense', style='bold_on') // &
            "  waiting (poll=" // colorize(ftoa(w%poll), color_fg='yellow') // "s" // &
            ", debounce=" // colorize(ftoa(w%debounce), color_fg='yellow') // "s)  " // &
            colorize("Ctrl+C", color_fg='red_intense', style='bold_on') // " to stop"
      end if
   end subroutine run_command_and_report

   !> Construct the effective command to run based on changes and target masks.
   !!
   !! For `fpm build`, the original command is returned unchanged.
   !!
   !! For `fpm run` and `fpm test`, if the user did not already pin names via
   !! `--name`/positional patterns, this routine can inject a computed set of
   !! target names corresponding to the changed files. This reduces redundant
   !! builds and speeds up iterative workflows.
   function build_run_command(settings, full_cmdline, cmd_prefix, cmd_rest, roots, file_mask, changed_idx, changed_count) result(cmd)
      use watch_cmdsplice, only: inject_names_into_cmd
      class(fpm_build_settings), intent(in) :: settings
      character(len=*), intent(in) :: full_cmdline
      character(len=*), intent(in) :: cmd_prefix
      character(len=*), intent(in) :: cmd_rest
      type(root_info_t), allocatable, intent(in) :: roots(:)
      integer(int64), allocatable, intent(in) :: file_mask(:)
      integer, intent(in) :: changed_idx(:)
      integer, intent(in) :: changed_count
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

      if (.not. allocated(roots) .or. size(roots) == 0) then
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
            s = s // nm
         end do
      end function join_names_from_mask

   end function build_run_command

end module watch_exec
