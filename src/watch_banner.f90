!> Present watcher startup and file-list events through the shared logger.
module watch_banner
   use watch_types, only: watch_opts_t
   use watch_util, only: ftoa, trim_or_default, join_csv
   use watch_log, only: log_event, log_int, log_real, log_string, log_strings, log_bool
   use fpm_strings, only: string_t, str
   use fpm_command_line, only: fpm_build_settings, fpm_test_settings, fpm_run_settings
   implicit none
   private
   public print_banner, print_file_list, print_configuration

contains

   !> Emit a startup summary and optional configuration/timing records.
   !!
   !! `watch.ready` uses normal verbosity; configuration/timings require level 1.
   !! Read `mode` from typed settings. Unallocated files count as zero; time
   !! inputs are seconds and event timing fields are milliseconds. Quiet mode
   !! suppresses these records. File-list output is handled separately by the engine.
   subroutine print_banner(full_cmdline, settings, w, files, build_secs, roots_count, secs_model, secs_targets, secs_watch)
      character(len=*), intent(in) :: full_cmdline
      class(fpm_build_settings), intent(in) :: settings
      type(watch_opts_t), intent(in) :: w
      type(string_t), allocatable, intent(in) :: files(:)
      real, intent(in) :: build_secs, secs_model, secs_targets, secs_watch
      integer, intent(in) :: roots_count
      integer :: nfiles
      character(len=:), allocatable :: mode, message

      nfiles = 0
      if (allocated(files)) nfiles = size(files)
      mode = command_mode(settings)
      message = "Watching " // str(nfiles) // " files (" // mode // ")"
      if (w%once) then
         message = message // "; one-shot"
      else
         message = message // "; Ctrl+C or touch .fpm-watch.stop to stop"
      end if
      call log_event("info", "watch.ready", message, &
                     [log_int("file_count", nfiles), log_int("root_count", roots_count), &
                      log_string("mode", mode), log_string("command", full_cmdline), log_bool("once", w%once)])
      call print_configuration(settings, w)
      call log_event("info", "watch.initialized", "Watch list initialized in " // ftoa(build_secs) // "s", &
                     [log_real("duration_ms", build_secs * 1000.0), log_real("model_ms", secs_model * 1000.0), &
                      log_real("targets_ms", secs_targets * 1000.0), log_real("watch_list_ms", secs_watch * 1000.0)], verbosity=1)
   end subroutine print_banner

   !> Report effective model/watch settings at verbosity 1 after startup or refresh.
   !!
   !! Features are a comma-separated fpm feature string; include/ignore attributes
   !! are arrays. Poll/debounce fields are milliseconds and rescan is seconds.
   subroutine print_configuration(settings, w)
      class(fpm_build_settings), intent(in) :: settings
      type(watch_opts_t), intent(in) :: w
      character(len=:), allocatable :: features, profile, compiler, build_dir

      profile = trim_or_default(settings%profile, "default")
      compiler = trim_or_default(settings%compiler, "(auto)")
      build_dir = trim_or_default(settings%build_dir, "build")
      features = "(none)"
      if (allocated(settings%features)) features = join_csv(settings%features, "(none)")
      call log_event("info", "watch.configuration", &
                     "Compiler " // compiler // "; profile " // profile // "; build directory " // build_dir // &
                     "; features " // features, &
                     [log_string("compiler", compiler), log_string("profile", profile), log_string("build_dir", build_dir), &
                      log_string("features", features), log_bool("prune", settings%prune), log_bool("deps", w%watch_deps), &
                      log_bool("silent_fpm", w%silent_fpm), &
                      log_bool("run_on_start", w%run_on_start), log_real("poll_ms", w%poll * 1000.0), &
                      log_real("debounce_ms", w%debounce * 1000.0), log_real("rescan_s", w%rescan), &
                      log_strings("ignore", w%ignore), log_strings("include", w%include)], verbosity=1)
   end subroutine print_configuration

   !> Emit up to 200 `watch.file` records when verbosity is at least 2.
   !!
   !! Larger lists produce an omitted-count record. Unallocated lists are ignored.
   !! The engine calls this at startup only when `print_files_once` is enabled,
   !! and after successful refreshes whenever verbosity is at least 2.
   subroutine print_file_list(files, w)
      type(string_t), allocatable, intent(in) :: files(:)
      type(watch_opts_t), intent(in) :: w
      integer :: i
      integer, parameter :: cap = 200
      if (w%verbosity < 2) return
      if (.not. allocated(files)) return
      do i = 1, min(size(files), cap)
         call log_event("info", "watch.file", files(i)%s, [log_string("path", files(i)%s)], verbosity=2)
      end do
      if (size(files) > cap) then
         call log_event("info", "watch.files_truncated", str(size(files)-cap) // " more watched files", &
                        [log_int("omitted_count", size(files)-cap)], verbosity=2)
      end if
   end subroutine print_file_list

   !> Describe the selected fpm command.
   pure function command_mode(settings) result(mode)
      class(fpm_build_settings), intent(in) :: settings
      character(len=:), allocatable :: mode
      select type (s => settings)
       type is (fpm_test_settings)
         mode = "test"
       type is (fpm_run_settings)
         mode = "run"
         if (s%example) mode = "run --example"
       class default
         mode = "build"
      end select
   end function command_mode

end module watch_banner
