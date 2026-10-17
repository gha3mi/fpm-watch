!> Portable polling, watch-list refreshes, and synchronous command execution.
!!
!! The engine debounces changes, maps source edits to run/test targets, and
!! refreshes the graph after watched configuration edits or optional periodic rescans.
!! Failed refreshes preserve the accepted watch list. Commands block polling;
!! ordinary command failures do not terminate continuous watching.
module watch_engine
   use, intrinsic :: iso_fortran_env, only: int64
   use watch_types, only: watch_config_t, root_info_t, watch_snapshot_t
   use watch_log, only: log_event, log_int, log_real, log_string, configure_logging
   use fpm_error, only: error_t
   use watch_graph_worker, only: compute_watch_snapshot
   use watch_fingerprint, only: init_fingerprints, scan_changes, accept_changes, watch_state_changed, preserve_fingerprints
   use watch_cli, only: parse_cli_config
   use watch_platform, only: termination_requested, stop_file_exists
   use watch_time, only: sleep_seconds
   use watch_exec, only: report_changes, build_run_command, run_command_and_report
   use watch_banner, only: print_banner, print_file_list, print_configuration
   use fpm_strings, only: string_t, str
   use fpm_filesystem, only: basename, join_path, canon_path, is_absolute_path
   use fpm_os, only: get_current_directory
   implicit none
   private
   public watcher_t

   !> Accepted configuration and one aligned watch-state snapshot.
   !!
   !! File masks and roots describe affected-target membership. A failed manifest
   !! refresh leaves a retry flag while accepted configuration and watch-list state
   !! are retained.
   type watcher_t
      !> Accepted watcher settings and serialized fpm command.
      type(watch_config_t) :: cfg
      !> Aligned accepted paths, root membership, and fingerprint baselines.
      type(watch_snapshot_t) :: snapshot

      !> Most recent command exit status; zero before a run, propagated by one-shot mode.
      integer :: last_exit = 0
      !> Last graph-refresh error text, retained to suppress repeated diagnostics.
      character(len=:), allocatable :: last_refresh_error
      !> Retry manifest settings/graph refresh on a later edit or periodic rescan.
      logical :: manifest_refresh_pending = .false.
   contains
      procedure :: init => watcher_init
      procedure :: run  => watcher_run
   end type watcher_t

contains

   !> Initialize logging, the watch list, and fingerprints.
   !!
   !! A failed initial graph terminates with status 1. Run the full configured command if
   !! `run_on_start` is enabled, including in one-shot mode.
   subroutine watcher_init(self, cfg)
      class(watcher_t), intent(inout) :: self
      type(watch_config_t), intent(in) :: cfg
      !! Parsed configuration with allocated model settings and normalized watcher options.

      type(error_t), allocatable :: error

      self%cfg = cfg
      call configure_logging(cfg%w)
      if (.not. allocated(self%cfg%project_dir)) then
         call get_current_directory(self%cfg%project_dir, error)
         if (allocated(error)) then
            call log_event("error", "watch.directory_failed", error%message)
            stop 1
         end if
      end if

      self%last_exit = 0
      self%manifest_refresh_pending = .false.
      if (allocated(self%last_refresh_error)) deallocate(self%last_refresh_error)

      call rebuild_watch_list(self, print_header=.true.)

      if (self%cfg%w%run_on_start) then
         call run_once(self, trim(self%cfg%fpm_cmdline))
      end if
   end subroutine watcher_init

   !> Poll until a stop file or termination request is detected.
   !!
   !! Return immediately in one-shot mode. Otherwise sleep, fingerprint files,
   !! debounce detected changes, and resample before acting. A watched manifest
   !! change or pending manifest refresh reparses settings and refreshes the graph.
   !! Other edits select an affected-target command, accept sampled fingerprints,
   !! and refresh dependency membership before running. A changed target mapping
   !! or failed refresh falls back to the original command. Existing baselines survive
   !! the refresh so edits during synchronous commands are detected on the next poll.
   !!
   !! Optional rescans refresh the graph and run the full command if paths or
   !! fingerprints changed. A pending manifest refresh is retried on later edits
   !! or rescans. Stop-file checks occur between commands and around waits.
   subroutine watcher_run(self)
      class(watcher_t), intent(inout) :: self

      integer :: changed_count
      character(len=:), allocatable :: cmd
      logical :: rescan_changed, refreshed, selection_changed

      integer(int64) :: rate, t_last_rescan, t_now
      real :: dt
      if (self%cfg%w%once) return

      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64
      call system_clock(t_last_rescan)

      do
         if (termination_requested()) exit
         if (stop_file_exists(self%cfg%project_dir)) exit

         call sleep_seconds(self%cfg%w%poll)

         if (termination_requested()) exit
         if (stop_file_exists(self%cfg%project_dir)) exit

         call scan_changes(self%snapshot%files, self%snapshot%fp_prev, self%snapshot%fp_now, self%snapshot%changed_idx, changed_count)

         if (changed_count > 0) then
            call sleep_seconds(self%cfg%w%debounce)
            if (termination_requested()) exit
            if (stop_file_exists(self%cfg%project_dir)) exit
            call scan_changes(self%snapshot%files, self%snapshot%fp_prev, self%snapshot%fp_now, self%snapshot%changed_idx, changed_count)
         end if

         if (changed_count > 0) then
            if (self%manifest_refresh_pending .or. &
                any_configuration_changed(self%snapshot%files, self%snapshot%changed_idx, changed_count, &
                                          self%cfg%response_files)) then
               call accept_changes(self%snapshot%fp_prev, self%snapshot%fp_now, self%snapshot%changed_idx, changed_count)
               call handle_manifest_change(self, refreshed)
               cycle
            end if

            call report_changes(self%snapshot%files, self%snapshot%changed_idx, changed_count, self%cfg%w)

            cmd = build_run_command( &
               settings      = self%cfg%settings,    &
               full_cmdline  = self%cfg%fpm_cmdline, &
               cmd_prefix    = self%cfg%cmd_prefix,  &
               cmd_rest      = self%cfg%cmd_rest,    &
               roots         = self%snapshot%roots,           &
               file_mask     = self%snapshot%file_mask,       &
               changed_idx   = self%snapshot%changed_idx,     &
               changed_count = changed_count )

            call accept_changes(self%snapshot%fp_prev, self%snapshot%fp_now, self%snapshot%changed_idx, changed_count)
            call rebuild_watch_list(self, print_header=.false., refreshed=refreshed, &
                                    preserve_baseline=.true., selection_changed=selection_changed)
            if (.not. refreshed .or. selection_changed) cmd = self%cfg%fpm_cmdline
            call run_once(self, cmd)

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
                     call rebuild_watch_list(self, print_header=.false., watch_changed=rescan_changed, preserve_baseline=.true.)
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

   !> Reparse settings, refresh the graph, and run after a configuration change.
   !!
   !! Pin the current log format. On refresh failure restore the accepted
   !! configuration and leave a retry pending. On success accept the new settings
   !! and run the full command. `refreshed` reports graph-refresh success, not
   !! command success.
   subroutine handle_manifest_change(self, refreshed)
      class(watcher_t), intent(inout) :: self
      logical, intent(out) :: refreshed
      type(watch_config_t) :: old_cfg
      type(error_t), allocatable :: error

      refreshed = .false.
      old_cfg = self%cfg
      if (.not. self%manifest_refresh_pending) then
         call log_event("info", "watch.manifest_changed", "Configuration changed; refreshing watch list")
      end if
      self%manifest_refresh_pending = .true.
      call parse_cli_config(self%cfg, log_format=old_cfg%w%log_format, error=error)
      if (allocated(error)) then
         self%cfg = old_cfg
         call configure_logging(self%cfg%w)
         call report_refresh_error(self, error%message)
         return
      end if
      call rebuild_watch_list(self, print_header=.false., refreshed=refreshed, preserve_baseline=.true.)
      if (.not. refreshed) then
         self%cfg = old_cfg
         call configure_logging(self%cfg%w)
         return
      end if
      self%manifest_refresh_pending = .false.
      call run_once(self, self%cfg%fpm_cmdline)
   end subroutine handle_manifest_change

   !> Return whether changed manifests or response candidates require reparsing settings.
   !!
   !! Manifest detection uses the basename and therefore triggers if any
   !! watched `fpm.toml` changes (main or dependency manifests).
   !! Response paths are absolute candidates supplied by the CLI adapter.
   logical function any_configuration_changed(files, changed_idx, changed_count, response_files) result(hit)
      type(string_t), allocatable, intent(in) :: files(:)
      integer, intent(in) :: changed_idx(:)
      integer, intent(in) :: changed_count
      type(string_t), allocatable, intent(in) :: response_files(:)
      integer :: j, idx, k
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
         if (.not. allocated(response_files)) cycle
         do k = 1, size(response_files)
            if (files(idx)%s /= response_files(k)%s) cycle
            hit = .true.
            return
         end do
      end do
   end function any_configuration_changed

   !> Keep one actionable refresh diagnostic while retaining the accepted snapshot.
   subroutine report_refresh_error(self, message)
      class(watcher_t), intent(inout) :: self
      character(len=*), intent(in) :: message
      if (allocated(self%last_refresh_error)) then
         if (self%last_refresh_error == message) return
      end if
      self%last_refresh_error = message
      call log_event("error", "watch.refresh_failed", "Watch list refresh failed: " // message // "; waiting for edits")
   end subroutine report_refresh_error

   !> Execute one command cycle and retain its status.
   !!
   !! Store the resulting exit status in `self%last_exit`; command failure does not stop
   !! the engine. Used for startup, edits, manifest refreshes, and rescans.
   subroutine run_once(self, cmd0)
      class(watcher_t), intent(inout) :: self
      character(len=*), intent(in) :: cmd0
      character(len=:), allocatable :: cmd
      integer :: exitstat
      real :: secs
      if (termination_requested()) then
         self%last_exit = 130
         return
      end if
      cmd = trim(cmd0)
      call run_command_and_report(cmd, self%cfg%w, exitstat, secs)
      self%last_exit = exitstat
   end subroutine run_once

   !> Build a candidate graph and replace the accepted watch-list snapshot.
   !!
   !! `print_header` selects initialization: failures exit with status 1, and
   !! success prints startup details. Later failures preserve files, masks, roots,
   !! fingerprints, and accepted model settings; identical diagnostics are suppressed.
   !! `refreshed` is true only after success. If requested, `watch_changed` compares
   !! paths and fingerprints with the old snapshot before resetting the baseline.
   !! A source-triggered refresh preserves baselines so concurrent edits remain pending.
   subroutine rebuild_watch_list(self, print_header, watch_changed, refreshed, preserve_baseline, selection_changed)
      class(watcher_t), intent(inout) :: self
      logical, intent(in) :: print_header
      !! True for initial setup (fatal errors and startup logging); false for recoverable refreshes.
      logical, intent(out), optional :: watch_changed
      !! True on a successful refresh if paths or fingerprints differ from the previous snapshot.
      logical, intent(out), optional :: refreshed
      !! True only after the candidate graph and new fingerprint baseline are accepted.

      logical, intent(in), optional :: preserve_baseline
      !! Preserve accepted baselines for existing paths while initializing new paths.
      logical, intent(out), optional :: selection_changed
      !! True unless paths, root bits, and root names match the accepted selection mapping.

      integer(int64) :: rate, t0, t1
      real :: build_secs_total
      real :: secs_model, secs_targets, secs_watch
      integer :: nfiles, nroots, i
      logical :: preserve
      character(len=:), allocatable :: original_directory
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
      if (present(selection_changed)) selection_changed = .true.
      candidate_cfg = self%cfg

      preserve = .false.
      if (present(preserve_baseline)) preserve = preserve_baseline
      if (present(watch_changed) .or. preserve) then
         if (allocated(self%snapshot%files)) old_files = self%snapshot%files
         if (allocated(self%snapshot%fp_prev)) old_fp = self%snapshot%fp_prev
      end if

      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64
      call system_clock(t0)

      call get_current_directory(original_directory, error)
      if (.not. allocated(error)) then
         call compute_watch_snapshot( &
            cfg          = candidate_cfg,    &
            files        = new_files,         &
            file_mask    = new_mask,          &
            roots        = new_roots,         &
            build_secs   = dummy_build,       &
            secs_model   = secs_model,        &
            secs_targets = secs_targets,      &
            secs_watch   = secs_watch,        &
            error        = error )
      end if
      if (termination_requested()) return
      if (.not. allocated(error)) then
         if (canon_path(original_directory) /= canon_path(self%cfg%project_dir)) then
            do i = 1, size(new_files)
               if (is_absolute_path(new_files(i)%s)) cycle
               new_files(i)%s = canon_path(join_path(self%cfg%project_dir, new_files(i)%s))
            end do
         end if
      end if

      if (allocated(error)) then
         if (print_header) then
            call log_event("error", "watch.init_failed", "Cannot initialize watch list: " // error%message)
            stop 1
         end if
         call report_refresh_error(self, error%message)
         return
      end if
      if (allocated(self%last_refresh_error)) deallocate(self%last_refresh_error)
      if (present(selection_changed)) then
         selection_changed = watch_state_changed(self%snapshot%files, self%snapshot%file_mask, new_files, new_mask)
         if (.not. selection_changed) selection_changed = root_mapping_changed(self%snapshot%roots, new_roots)
      end if
      call move_alloc(candidate_cfg%settings, self%cfg%settings)

      call move_alloc(new_files, self%snapshot%files)
      call move_alloc(new_mask,  self%snapshot%file_mask)
      call move_alloc(new_roots, self%snapshot%roots)

      call init_fingerprints(self%snapshot%files, self%snapshot%fp_prev, self%snapshot%fp_now, self%snapshot%changed_idx)
      if (preserve) call preserve_fingerprints(old_files, old_fp, self%snapshot%files, self%snapshot%fp_prev)
      if (present(watch_changed)) then
         watch_changed = watch_state_changed(old_files, old_fp, self%snapshot%files, self%snapshot%fp_prev)
      end if
      if (present(refreshed)) refreshed = .true.

      call system_clock(t1)
      build_secs_total = real(t1 - t0) / real(rate)

      nfiles = 0
      if (allocated(self%snapshot%files)) nfiles = size(self%snapshot%files)

      nroots = 0
      if (allocated(self%snapshot%roots)) nroots = size(self%snapshot%roots)

      if (print_header) then
         call print_banner( &
            full_cmdline = self%cfg%fpm_cmdline, &
            settings     = self%cfg%settings,    &
            w            = self%cfg%w,           &
            files        = self%snapshot%files,           &
            build_secs   = build_secs_total,     &
            roots_count  = nroots,               &
            secs_model   = secs_model,           &
            secs_targets = secs_targets,         &
            secs_watch   = secs_watch )
      else
         call print_configuration(self%cfg%settings, self%cfg%w)
         call log_event("info", "watch.refreshed", "Watch list updated; " // str(nfiles) // " files", &
                       [log_int("file_count", nfiles), log_real("duration_ms", build_secs_total * 1000.0)], verbosity=1)
      end if
      if (self%cfg%w%verbosity >= 2) call print_file_list(self%snapshot%files, self%cfg%w)
   end subroutine rebuild_watch_list

   !> Return whether root names or their bits changed during a dependency refresh.
   pure logical function root_mapping_changed(old_roots, new_roots) result(changed)
      type(root_info_t), allocatable, intent(in) :: old_roots(:), new_roots(:)
      integer :: i
      changed = .true.
      if (.not. allocated(old_roots) .or. .not. allocated(new_roots)) return
      if (size(old_roots) /= size(new_roots)) return
      do i = 1, size(old_roots)
         if (old_roots(i)%mask /= new_roots(i)%mask) return
         if (.not. allocated(old_roots(i)%name) .or. .not. allocated(new_roots(i)%name)) return
         if (old_roots(i)%name /= new_roots(i)%name) return
      end do
      changed = .false.
   end function root_mapping_changed

end module watch_engine
