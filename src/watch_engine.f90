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
   use watch_log, only: log_info
   use watch_fpm_graph, only: compute_watch_files_from_settings
   use watch_fingerprint, only: init_fingerprints, scan_changes, accept_changes, manifest_key_from_files
   use watch_time, only: sleep_seconds, set_low_cpu
   use watch_exec, only: report_changes, build_run_command, run_command_and_report
   use watch_banner, only: print_banner, print_file_list
   use watch_util, only: ftoa
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
      integer :: exitstat, j, idx
      real :: secs

      integer(int64) :: rate, t_last_rescan, t_now
      real :: dt

      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64
      call system_clock(t_last_rescan)

      do
         if (exists(".fpm-watch.stop")) exit

         call sleep_seconds(self%cfg%w%poll)

         call scan_changes(self%files, self%fp_prev, self%fp_now, self%changed_idx, changed_count)

         if (changed_count > 0) then
            call sleep_seconds(self%cfg%w%debounce)
            call scan_changes(self%files, self%fp_prev, self%fp_now, self%changed_idx, changed_count)
         end if

         if (changed_count > 0) then
            if (any_manifest_changed(self%files, self%changed_idx, changed_count)) then
               call handle_manifest_change(self)
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

            call self%fm%on_before_run_all(cmd)
            call run_command_and_report(cmd, self%cfg%w, exitstat, secs)
            call self%fm%on_after_run_all(exitstat, secs)

            call accept_changes(self%fp_prev, self%fp_now, self%changed_idx, changed_count)

            deallocate(changed)
         end if

         if (self%cfg%w%rescan > 0.0) then
            call system_clock(t_now)
            if (t_now < t_last_rescan) then
               t_last_rescan = t_now
            else
               dt = real(t_now - t_last_rescan) / real(rate)
               if (dt >= self%cfg%w%rescan) then
                  call log_info(self%cfg%w, "rescan triggered -> rebuilding watch list")
                  call rebuild_watch_list(self, print_header=.false.)
                  call system_clock(t_last_rescan)
               end if
            end if
         end if
      end do
   end subroutine watcher_run

   !> Handle a manifest change (`fpm.toml`) by rebuilding the watch list.
   !!
   !! This also notifies feature plugins via `on_manifest_changed`.
   subroutine handle_manifest_change(self)
      class(watcher_t), intent(inout) :: self
      integer(int64) :: man_now

      man_now = manifest_key_from_files(self%cfg%settings, self%files)
      if (man_now /= self%man_prev) then
         call self%fm%on_manifest_changed_all(self%man_prev, man_now)
         self%man_prev = man_now
      end if
      call log_info(self%cfg%w, "manifest changed -> rebuilding watch list")
      call rebuild_watch_list(self, print_header=.false.)
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
      call self%fm%on_after_run_all(exitstat, secs)
   end subroutine run_once

   !> Recompute the watch list, rebuild fingerprints, and print/log the result.
   !!
   !! Called:
   !! - During initialization,
   !! - When the manifest changes,
   !! - Periodically when `rescan > 0`.
   subroutine rebuild_watch_list(self, print_header)
      class(watcher_t), intent(inout) :: self
      logical, intent(in) :: print_header

      integer(int64) :: rate, t0, t1
      real :: build_secs_total
      real :: secs_model, secs_targets, secs_watch
      integer :: nfiles, nroots
      real :: dummy_build

      type(string_t), allocatable :: new_files(:)
      integer(int64), allocatable :: new_mask(:)
      type(root_info_t), allocatable :: new_roots(:)

      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64
      call system_clock(t0)

      call compute_watch_files_from_settings( &
         settings     = self%cfg%settings, &
         w            = self%cfg%w,        &
         files        = new_files,         &
         file_mask    = new_mask,          &
         roots        = new_roots,         &
         manifest_key = self%man_prev,     &
         build_secs   = dummy_build,       &
         secs_model   = secs_model,        &
         secs_targets = secs_targets,      &
         secs_watch   = secs_watch )

      call move_alloc(new_files, self%files)
      call move_alloc(new_mask,  self%file_mask)
      call move_alloc(new_roots, self%roots)

      call self%fm%on_watch_list_built_all(self%files, self%roots)

      self%man_prev = manifest_key_from_files(self%cfg%settings, self%files)

      call init_fingerprints(self%files, self%fp_prev, self%fp_now, self%changed_idx)

      call system_clock(t1)
      build_secs_total = real(t1 - t0) / real(rate)

      nfiles = 0
      if (allocated(self%files)) nfiles = size(self%files)

      nroots = 0
      if (allocated(self%roots)) nroots = size(self%roots)

      if (self%cfg%w%debug) then
         call log_info(self%cfg%w, "debug: manifest_key=" // str(self%man_prev))
         call log_info(self%cfg%w, "debug: roots=" // str(nroots) // " files=" // str(nfiles))
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
         call log_info(self%cfg%w, "watch list updated in " // ftoa(build_secs_total) // "s; files=" // str(nfiles))
         if (self%cfg%w%verbosity >= 2) call print_file_list(self%files, self%cfg%w)
      end if
   end subroutine rebuild_watch_list

end module watch_engine
