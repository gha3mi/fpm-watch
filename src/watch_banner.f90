!> Console banner and watch-list reporting.
!!
!! This module centralizes user-facing console output that is shown on startup
!! and when printing the computed watch list. It is intentionally "UI-only":
!! it does not compute any watch state, it only presents it.
!!
!! Output is generally suppressed when `w%verbosity < 0` (quiet mode).
module watch_banner
   use, intrinsic :: iso_fortran_env, only: output_unit
   use watch_types, only: watch_opts_t
   use watch_util,  only: ftoa, trim_or_default, join_csv
   use fpm_strings, only: string_t, str
   use fpm_command_line, only: fpm_build_settings, fpm_test_settings, fpm_run_settings
   use face, only: colorize
   implicit none
   private
   public print_banner, print_file_list

contains

   !> Print the startup banner and an initialization timing summary.
   !!
   !! The banner is a compact summary of the effective configuration and the
   !! computed watch set.
   !!
   !! ### Verbosity rules
   !! - If `w%verbosity < 0`, this routine returns without printing.
   !! - If `w%debug` is enabled, an extra debug line is printed.
   !!
   !! ### Arguments
   !! - `full_cmdline`: Full command that will be executed (typically `fpm ...`).
   !! - `settings`: Effective `fpm` settings used to build the model.
   !! - `w`: Watcher options (verbosity, polling, flags shown in banner).
   !! - `files`: Computed list of watched file paths (may be unallocated).
   !! - `build_secs`: Total init time (seconds).
   !! - `roots_count`: Number of root targets included in the watch closure.
   !! - `secs_model`, `secs_targets`, `secs_watch`: Timing breakdown (seconds).
   subroutine print_banner(full_cmdline, settings, w, files, build_secs, roots_count, secs_model, secs_targets, secs_watch)
      character(len=*), intent(in) :: full_cmdline
      class(fpm_build_settings), intent(in) :: settings
      type(watch_opts_t), intent(in) :: w
      type(string_t), allocatable, intent(in) :: files(:)
      real, intent(in) :: build_secs
      integer, intent(in) :: roots_count
      real, intent(in) :: secs_model, secs_targets, secs_watch

      integer :: nfiles
      character(len=:), allocatable :: mode
      character(len=:), allocatable :: initrun
      character(len=:), allocatable :: deps
      character(len=:), allocatable :: prune
      character(len=:), allocatable :: lowcpu
      character(len=:), allocatable :: silent
      character(len=:), allocatable :: bdir

      if (w%verbosity < 0) return

      nfiles = 0
      if (allocated(files)) nfiles = size(files)

      mode = command_mode(settings)

      if (w%run_on_start) then
         initrun = "on"
      else
         initrun = "off"
      end if

      if (w%watch_deps) then
         deps = "on"
      else
         deps = "off"
      end if

      if (settings%prune) then
         prune = "on"
      else
         prune = "off"
      end if

      if (w%low_cpu) then
         lowcpu = "on"
      else
         lowcpu = "off"
      end if

      if (w%silent_fpm) then
         silent = "on"
      else
         silent = "off"
      end if

      bdir = trim_or_default(settings%build_dir, "build")

      write(output_unit,'(a)') &
         colorize("fpm-watch", color_fg='cyan_intense', style='bold_on') // &
         "  |  watching: " // colorize(mode, color_fg='yellow_intense', style='bold_on') // &
         "  |  " // colorize("Ctrl+C", color_fg='red_intense', style='bold_on') // " to stop"

      write(output_unit,'(a)') &
         colorize("command    |", color_fg='cyan_intense', style='bold_on') // "  " // &
         colorize(trim(full_cmdline), color_fg='white_intense')

      write(output_unit,'(a)') &
         colorize("build-dir  |", color_fg='cyan_intense', style='bold_on') // "  " // &
         colorize(trim(bdir), color_fg='yellow_intense')

      write(output_unit,'(a)') &
         colorize("options    |", color_fg='cyan_intense', style='bold_on') // &
         "  prune="       // colorize(prune,  color_fg='yellow') // &
         "   deps="       // colorize(deps,   color_fg='yellow') // &
         "   low-cpu="    // colorize(lowcpu, color_fg='yellow') // &
         "   silent-fpm=" // colorize(silent, color_fg='yellow') // &
         "   verbosity="  // colorize(str(w%verbosity), color_fg='yellow')

      write(output_unit,'(a)') &
         colorize("compiler   |", color_fg='cyan_intense', style='bold_on') // "  " // &
         colorize(trim_or_default(settings%compiler, "(auto)"), color_fg='green_intense')

      write(output_unit,'(a)') &
         colorize("profile    |", color_fg='cyan_intense', style='bold_on') // "  " // &
         colorize(active_profile(settings), color_fg='magenta_intense')

      write(output_unit,'(a)') &
         colorize("features   |", color_fg='cyan_intense', style='bold_on') // "  " // &
         colorize(active_features(settings), color_fg='blue_intense')

      write(output_unit,'(a)') &
         colorize("watch      |", color_fg='cyan_intense', style='bold_on') // &
         "  files=" // colorize(str(nfiles), color_fg='yellow_intense', style='bold_on') // &
         "  roots=" // colorize(str(roots_count), color_fg='yellow_intense', style='bold_on') // &
         "  poll=" // colorize(ftoa(w%poll), color_fg='yellow') // "s" // &
         "  debounce=" // colorize(ftoa(w%debounce), color_fg='yellow') // "s" // &
         "  rescan=" // colorize(ftoa(w%rescan), color_fg='yellow') // "s" // &
         "  init-run=" // colorize(initrun, color_fg='yellow')

      write(output_unit,'(a)') &
         colorize("init       |", color_fg='cyan_intense', style='bold_on') // &
         "  model=" // colorize(ftoa(secs_model), color_fg='yellow') // "s" // &
         "  targets=" // colorize(ftoa(secs_targets), color_fg='yellow') // "s" // &
         "  watchlist=" // colorize(ftoa(secs_watch), color_fg='yellow') // "s" // &
         "  total=" // colorize(ftoa(build_secs), color_fg='yellow_intense', style='bold_on') // "s"

      write(output_unit,'(a)') &
         colorize("stop       |", color_fg='cyan_intense', style='bold_on') // &
         "  Ctrl+C or touch " // colorize(".fpm-watch.stop", color_fg='yellow_intense')

      if (w%debug) then
         write(output_unit,'(a)') &
            colorize("debug      |", color_fg='magenta_intense', style='bold_on') // &
            "  debug=on  verbosity=" // colorize(str(w%verbosity), color_fg='magenta_intense')
      end if
   end subroutine print_banner

   !> Print the computed watch list (limited output).
   !!
   !! The printed list is capped to avoid overwhelming the terminal.
   !!
   !! ### Verbosity rules
   !! - Printed only when `w%verbosity >= 2`.
   !! - Returns immediately if `files` is not allocated.
   !!
   !! ### Arguments
   !! - `files`: Computed list of watched file paths.
   !! - `w`: Watcher options (controls whether printing is enabled).
   subroutine print_file_list(files, w)
      type(string_t), allocatable, intent(in) :: files(:)
      type(watch_opts_t), intent(in) :: w
      integer :: i, cap
      character(len=:), allocatable :: pfx

      if (w%verbosity < 2) return
      if (.not. allocated(files)) return

      cap = 200
      pfx = colorize("fpm-watch info:", color_fg='blue_intense', style='bold_on')

      write(output_unit,'(a)') &
         pfx // " watched files list (cap=200), n=" // &
         colorize(str(size(files)), color_fg='yellow_intense', style='bold_on')

      do i = 1, min(size(files), cap)
         write(output_unit,'(a)') pfx // "   " // colorize(trim(files(i)%s), color_fg='white_intense')
      end do

      if (size(files) > cap) then
         write(output_unit,'(a)') &
            pfx // "   ... (" // colorize(str(size(files)-cap), color_fg='yellow') // " more)"
      end if
   end subroutine print_file_list

   !> Derive a human-readable mode label from `fpm` settings.
   !!
   !! The returned string is used only for banner presentation.
   pure function command_mode(settings) result(mode)
      class(fpm_build_settings), intent(in) :: settings
      character(len=:), allocatable :: mode
      select type (s => settings)
       type is (fpm_test_settings)
         mode = "test"
       type is (fpm_run_settings)
         if (s%example) then
            mode = "run --example"
         else
            mode = "run"
         end if
       class default
         mode = "build"
      end select
   end function command_mode

   !> Return the active build profile for banner display.
   pure function active_profile(settings) result(p)
      class(fpm_build_settings), intent(in) :: settings
      character(len=:), allocatable :: p
      p = trim_or_default(settings%profile, "default")
   end function active_profile

   !> Return a comma-separated feature list for banner display.
   pure function active_features(settings) result(f)
      class(fpm_build_settings), intent(in) :: settings
      character(len=:), allocatable :: f
      if (allocated(settings%features)) then
         f = join_csv(settings%features, "(none)")
      else
         f = "(none)"
      end if
   end function active_features

end module watch_banner
