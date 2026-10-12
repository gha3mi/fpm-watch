!> Watch loop engine and watcher state.
!!
!! This module contains the `watcher_t` type, which:
!! - Builds and maintains the watch list,
!! - Tracks file fingerprints for change detection,
!! - Runs `fpm` commands when changes are detected,
!! - Coordinates optional feature plugins.
!!
!! The engine is intentionally "poll-based" to remain portable across platforms.
module watch_engine
   use, intrinsic :: iso_fortran_env, only: int64
   use watch_types, only: watch_config_t, root_info_t
   use watch_feature_manager, only: feature_manager_t
   use watch_feature_factory, only: enable_features
   use watch_log, only: log_event, log_int, log_real, log_string, configure_logging
   use fpm_error, only: error_t
   use watch_fpm_graph, only: compute_watch_files_from_settings
   use watch_fingerprint, only: init_fingerprints, scan_changes, accept_changes, manifest_key_from_files, watch_state_changed
   use watch_cli, only: parse_cli_config
   use watch_platform, only: termination_requested
   use watch_time, only: sleep_seconds, set_low_cpu
   use watch_exec, only: report_changes, build_run_command, run_command_and_report
   use watch_banner, only: print_banner, print_file_list, print_configuration
   use fpm_strings, only: string_t, str
   use fpm_filesystem, only: basename, exists
   implicit none
   private
   public watcher_t

   !> Main watcher state and methods.
   !!
   !! `watcher_t` owns:
   !! - The effective configuration (`cfg`)
   !! - Feature plugins (`fm`)
   !! - Watch list (`files`) plus per-file target masks (`file_mask`)
   !! - Fingerprinting buffers for change detection (`fp_prev`, `fp_now`)
   !! - The current manifest key used to detect "rebuild needed" events (`man_prev`)
   type watcher_t
      type(watch_config_t) :: cfg
      type(feature_manager_t) :: fm

      type(string_t), allocatable :: files(:)
      integer(int64), allocatable :: file_mask(:)
      type(root_info_t), allocatable :: roots(:)

      integer(int64), allocatable :: fp_prev(:)
      integer(int64), allocatable :: fp_now(:)
      integer, allocatable :: changed_idx(:)

      integer(int64) :: man_prev = 0_int64
      !! Exit status of the most recently executed command (also used by --watch-once).
      integer :: last_exit = 0
      !! Last refresh error, used to avoid repeating identical diagnostics.
      character(len=:), allocatable :: last_refresh_error
      !! Retry a failed manifest refresh, including its settings and command.
      logical :: manifest_refresh_pending = .false.
   contains
      procedure :: init => watcher_init
      procedure :: run  => watcher_run
   end type watcher_t

contains

   !> Initialize a watcher instance from configuration.
   !!
   !! This routine:
   !! - Applies low-CPU mode selection to the sleep implementation.
   !! - Enables requested feature plugins and calls their init/start callbacks.
   !! - Builds the initial watch list and fingerprints.
   !! - Optionally performs an initial run (`w%run_on_start`).
   subroutine watcher_init(self, cfg)
      class(watcher_t), intent(inout) :: self
      type(watch_config_t), intent(in) :: cfg

      self%cfg = cfg
      call configure_logging(cfg%w)
      call set_low_cpu(self%cfg%w%low_cpu)

      call enable_features(self%fm, self%cfg%w)
      call self%fm%init_all(self%cfg)
      call self%fm%on_start_all()

      call rebuild_watch_list(self, print_header=.true.)

      if (self%cfg%w%run_on_start) then
         call run_once(self, trim(self%cfg%fpm_cmdline))
      end if
   end subroutine watcher_init

   !> Run the main watch loop until termination.
   !!
   !! Termination conditions:
   !! - The sentinel file `.fpm-watch.stop` exists, or
   !! - The process receives an external termination (e.g., Ctrl+C).
   !!
   !! Each iteration performs:
   !! 1. Sleep for `poll` seconds.
   !! 2. Scan for changes (fingerprints).
   !! 3. If changes were found, sleep for `debounce` seconds and rescan.
   !! 4. If `fpm.toml` changed, rebuild watch list.
   !! 5. Otherwise, run the computed `fpm` command (with target injection
   !!    for run/test), notify plugins, and accept fingerprints.
   subroutine watcher_run(self)
      class(watcher_t), intent(inout) :: self

      integer :: changed_count
      type(string_t), allocatable :: changed(:)
      character(len=:), allocatable :: cmd
      integer :: j, idx
      logical :: rescan_changed, refreshed

      integer(int64) :: rate, t_last_rescan, t_now
      real :: dt
      if (self%cfg%w%once) return

      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64
      call system_clock(t_last_rescan)

      do
         if (termination_requested()) exit
         if (exists(".fpm-watch.stop")) exit

         call sleep_seconds(self%cfg%w%poll)

         if (termination_requested()) exit
         if (exists(".fpm-watch.stop")) exit

         call scan_changes(self%files, self%fp_prev, self%fp_now, self%changed_idx, changed_count)

         if (changed_count > 0) then
            call sleep_seconds(self%cfg%w%debounce)
            if (termination_requested()) exit
            if (exists(".fpm-watch.stop")) exit
            call scan_changes(self%files, self%fp_prev, self%fp_now, self%changed_idx, changed_count)
         end if

         if (changed_count > 0) then
            if (self%manifest_refresh_pending .or. &
                any_manifest_changed(self%files, self%changed_idx, changed_count)) then
               call handle_manifest_change(self, refreshed)
               if (.not. refreshed) then
                  call accept_changes(self%fp_prev, self%fp_now, self%changed_idx, changed_count)
               end if
               cycle
            end if

            allocate(changed(changed_count))
            do j = 1, changed_count
               idx = self%changed_idx(j)
               if (idx >= 1 .and. idx <= size(self%files)) then
                  changed(j)%s = self%files(idx)%s
               else
                  changed(j)%s = ""
               end if
            end do

            call self%fm%on_change_detected_all(changed)
            call report_changes(self%files, self%changed_idx, changed_count, self%cfg%w)

            cmd = build_run_command( &
               settings      = self%cfg%settings,    &
               full_cmdline  = self%cfg%fpm_cmdline, &
               cmd_prefix    = self%cfg%cmd_prefix,  &
               cmd_rest      = self%cfg%cmd_rest,    &
               roots         = self%roots,           &
               file_mask     = self%file_mask,       &
               changed_idx   = self%changed_idx,     &
               changed_count = changed_count )

            call run_once(self, cmd)

            call accept_changes(self%fp_prev, self%fp_now, self%changed_idx, changed_count)

            deallocate(changed)
            if (termination_requested()) exit
         end if

         if (self%cfg%w%rescan > 0.0) then
            call system_clock(t_now)
            if (t_now < t_last_rescan) then
               t_last_rescan = t_now
            else
               dt = real(t_now - t_last_rescan) / real(rate)
               if (dt >= self%cfg%w%rescan) then
                  call log_event("info", "watch.rescan", "Rescanning watch list", verbosity=1)
                  if (self%manifest_refresh_pending) then
                     call handle_manifest_change(self, refreshed)
                  else
                     call rebuild_watch_list(self, print_header=.false., watch_changed=rescan_changed)
                     if (rescan_changed) call run_once(self, self%cfg%fpm_cmdline)
                  end if
                  call system_clock(t_last_rescan)
               end if
            end if
         end if
      end do
      if (termination_requested()) then
         call log_event("info", "watch.stopped", "Stopped watching", [log_string("reason", "interrupt")])
      else
         call log_event("info", "watch.stopped", "Stopped watching", [log_string("reason", "stop_file")])
      end if
   end subroutine watcher_run

   !> Handle a manifest change (`fpm.toml`) by rebuilding the watch list.
   !!
   !! This also notifies feature plugins via `on_manifest_changed`.
   subroutine handle_manifest_change(self, refreshed)
      class(watcher_t), intent(inout) :: self
      logical, intent(out) :: refreshed
      integer(int64) :: man_old
      type(watch_config_t) :: old_cfg

      man_old = self%man_prev
      old_cfg = self%cfg
      if (.not. self%manifest_refresh_pending) then
         call log_event("info", "watch.manifest_changed", "Manifest changed; refreshing watch list")
      end if
      self%manifest_refresh_pending = .true.
      call parse_cli_config(self%cfg, log_format=old_cfg%w%log_format)
      call rebuild_watch_list(self, print_header=.false., refreshed=refreshed)
      if (.not. refreshed) then
         self%cfg = old_cfg
         call configure_logging(self%cfg%w)
         return
      end if
      if (self%man_prev /= man_old) call self%fm%on_manifest_changed_all(man_old, self%man_prev)
      self%manifest_refresh_pending = .false.
      call set_low_cpu(self%cfg%w%low_cpu)
      call run_once(self, self%cfg%fpm_cmdline)
   end subroutine handle_manifest_change

   !> Return whether the changed set contains `fpm.toml`.
   !!
   !! This detection is based on the basename only and therefore triggers if
   !! any watched file named `fpm.toml` changes (main or dependency manifests).
   logical function any_manifest_changed(files, changed_idx, changed_count) result(hit)
      type(string_t), allocatable, intent(in) :: files(:)
      integer, intent(in) :: changed_idx(:)
      integer, intent(in) :: changed_count
      integer :: j, idx
      character(len=:), allocatable :: b
      hit = .false.
      if (.not. allocated(files)) return
      do j = 1, changed_count
         idx = changed_idx(j)
         if (idx < 1 .or. idx > size(files)) cycle
         b = basename(files(idx)%s)
         if (b == "fpm.toml") then
            hit = .true.
            return
         end if
      end do
   end function any_manifest_changed

   !> Run the configured command once without change detection.
   !!
   !! Used for the optional "run on start" behavior.
   subroutine run_once(self, cmd0)
      class(watcher_t), intent(inout) :: self
      character(len=*), intent(in) :: cmd0
      character(len=:), allocatable :: cmd
      integer :: exitstat
      real :: secs
      cmd = trim(cmd0)
      call self%fm%on_before_run_all(cmd)
      call run_command_and_report(cmd, self%cfg%w, exitstat, secs)
      self%last_exit = exitstat
      call self%fm%on_after_run_all(exitstat, secs)
   end subroutine run_once

   !> Recompute the watch list, rebuild fingerprints, and print/log the result.
   !!
   !! Called:
   !! - During initialization,
   !! - When the manifest changes,
   !! - Periodically when `rescan > 0`.
   subroutine rebuild_watch_list(self, print_header, watch_changed, refreshed)
      class(watcher_t), intent(inout) :: self
      logical, intent(in) :: print_header
      logical, intent(out), optional :: watch_changed
      logical, intent(out), optional :: refreshed

      integer(int64) :: rate, t0, t1, candidate_manifest_key
      real :: build_secs_total
      real :: secs_model, secs_targets, secs_watch
      integer :: nfiles, nroots
      real :: dummy_build

      type(string_t), allocatable :: new_files(:)
      integer(int64), allocatable :: new_mask(:)
      type(root_info_t), allocatable :: new_roots(:)
      type(string_t), allocatable :: old_files(:)
      integer(int64), allocatable :: old_fp(:)
      type(watch_config_t) :: candidate_cfg
      type(error_t), allocatable :: error

      if (present(watch_changed)) watch_changed = .false.
      if (present(refreshed)) refreshed = .false.
      candidate_cfg = self%cfg

      if (present(watch_changed)) then
         if (allocated(self%files)) old_files = self%files
         if (allocated(self%fp_prev)) old_fp = self%fp_prev
      end if

      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64
      call system_clock(t0)

      call compute_watch_files_from_settings( &
         settings     = candidate_cfg%settings, &
         w            = self%cfg%w,        &
         files        = new_files,         &
         file_mask    = new_mask,          &
         roots        = new_roots,         &
         manifest_key = candidate_manifest_key, &
         build_secs   = dummy_build,       &
         secs_model   = secs_model,        &
         secs_targets = secs_targets,      &
         secs_watch   = secs_watch,        &
         error        = error )

      if (allocated(error)) then
         if (print_header) then
            call log_event("error", "watch.init_failed", "Cannot initialize watch list: " // error%message)
            stop 1
         end if
         if (allocated(self%last_refresh_error)) then
            if (self%last_refresh_error == error%message) return
         end if
         self%last_refresh_error = error%message
         call log_event("error", "watch.refresh_failed", "Watch list refresh failed: " // error%message // "; waiting for edits")
         return
      end if
      if (allocated(self%last_refresh_error)) deallocate(self%last_refresh_error)
      call move_alloc(candidate_cfg%settings, self%cfg%settings)

      call move_alloc(new_files, self%files)
      call move_alloc(new_mask,  self%file_mask)
      call move_alloc(new_roots, self%roots)

      call self%fm%on_watch_list_built_all(self%files, self%roots)

      self%man_prev = manifest_key_from_files(self%cfg%settings, self%files)

      call init_fingerprints(self%files, self%fp_prev, self%fp_now, self%changed_idx)
      if (present(watch_changed)) then
         watch_changed = watch_state_changed(old_files, old_fp, self%files, self%fp_prev)
      end if
      if (present(refreshed)) refreshed = .true.

      call system_clock(t1)
      build_secs_total = real(t1 - t0) / real(rate)

      nfiles = 0
      if (allocated(self%files)) nfiles = size(self%files)

      nroots = 0
      if (allocated(self%roots)) nroots = size(self%roots)

      if (self%cfg%w%debug) then
         call log_event("debug", "watch.fingerprint", "Watch graph fingerprint updated", &
                        [log_int("manifest_key", self%man_prev), log_int("root_count", nroots), log_int("file_count", nfiles)])
      end if

      if (print_header) then
         call print_banner( &
            full_cmdline = self%cfg%fpm_cmdline, &
            settings     = self%cfg%settings,    &
            w            = self%cfg%w,           &
            files        = self%files,           &
            build_secs   = build_secs_total,     &
            roots_count  = nroots,               &
            secs_model   = secs_model,           &
            secs_targets = secs_targets,         &
            secs_watch   = secs_watch )
         if (self%cfg%w%print_files_once) call print_file_list(self%files, self%cfg%w)
      else
         call print_configuration(self%cfg%settings, self%cfg%w)
         call log_event("info", "watch.refreshed", "Watch list updated; " // str(nfiles) // " files", &
                       [log_int("file_count", nfiles), log_real("duration_ms", build_secs_total * 1000.0)], verbosity=1)
         if (self%cfg%w%verbosity >= 2) call print_file_list(self%files, self%cfg%w)
      end if
   end subroutine rebuild_watch_list

end module watch_engine
