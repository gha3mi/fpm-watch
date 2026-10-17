!> Shared types used throughout `fpm-watch`.
!!
!! This module defines the configuration records used to pass options and
!! `fpm` settings into the engine.
module watch_types
   use, intrinsic :: iso_fortran_env, only: int64
   use fpm_strings, only: string_t
   use fpm_command_line, only: fpm_build_settings
   implicit none
   private
   public watch_opts_t, root_info_t, watch_config_t, watch_snapshot_t

   !> Watcher options assembled from defaults, manifest values, and leading CLI flags.
   !!
   !! Timing fields are seconds. `restart_count` identifies the child generation
   !! used in command cycle IDs. Watcher and supervisor share these parsed options.
   !! `set_watch_defaults` initializes format/executable strings and list arrays.
   type watch_opts_t
      !> Verbosity level:
      !!
      !! - `-1`: quiet (errors and enabled output protocol)
      !! - `0`: normal
      !! - `1`: more detail
      !! - `2`: watch paths and debug records
      integer :: verbosity = 0
      !> Output format: text (default), json (JSON Lines), or logfmt.
      character(len=:), allocatable :: log_format
      !> Executable used for build/test/run; defaults to `fpm` on PATH.
      character(len=:), allocatable :: fpm_executable
      !> Seconds between polling scans; normalized to at least 0.05.
      real    :: poll = 0.5
      !> Seconds to wait after detecting a change before resampling.
      real    :: debounce = 0.2
      !> Optional periodic rescan interval (seconds). `0` disables periodic rescans.
      real    :: rescan = 0.0
      !> Run the command once immediately on startup.
      logical :: run_on_start = .true.
      !> Suppress both command output streams; watcher and model records remain separate.
      logical :: silent_fpm = .false.
      !> Bracket command stderr and force cycle events in structured logging only.
      logical :: output_markers = .false.
      !> Internal supervisor generation, retained during manifest refresh.
      integer :: restart_count = 0
      !> Also watch dependency project sources (can include paths under build dir).
      logical :: watch_deps = .false.
      !> Ignore globs, applied after include rules; manifest/version metadata bypass filtering.
      type(string_t), allocatable :: ignore(:)
      !> Include globs; empty means all eligible files. Manifest/version metadata always remain.
      type(string_t), allocatable :: include(:)
      !> Run once (init + optional run-on-start) and exit (CI-friendly).
      logical :: once = .false.
      !> Restart failed watcher children until success or the configured limit.
      logical :: auto_restart = .false.
      !> Internal child marker preventing recursive supervision.
      logical :: is_child = .false.
      !> Seconds between failed child exits and the next launch.
      real :: restart_delay = 1.0
      !> Maximum restarts after initial launch; zero means unlimited.
      integer :: restart_max = 0
   end type watch_opts_t

   !> Executable basename and one root bit used for affected-target selection.
   !!
   !! Graph construction assigns unique bits to up to 64 run/test roots; graphs outside
   !! that range return an empty root array and do not support name injection.
   type root_info_t
      !> Executable output basename without suffix, used as an fpm target pattern.
      character(len=:), allocatable :: name
      !> Single bit identifying this root in target and file membership masks.
      integer(int64) :: mask = 0_int64
   end type root_info_t

   !> One aligned watch-list snapshot, including membership and change baselines.
   type watch_snapshot_t
      type(string_t), allocatable :: files(:)
      integer(int64), allocatable :: file_mask(:)
      type(root_info_t), allocatable :: roots(:)
      integer(int64), allocatable :: fp_prev(:), fp_now(:)
      integer, allocatable :: changed_idx(:)
   end type watch_snapshot_t

   !> Fully assembled watcher configuration.
   !!
   !! This record is the primary input to `watcher_t%init()`.
   type watch_config_t
      !> Watcher options.
      type(watch_opts_t) :: w
      !> Absolute project directory shared by manifest/model/file resolution.
      character(len=:), allocatable :: project_dir
      !> Absolute named response-file candidates, retained to detect edits or new overrides.
      type(string_t), allocatable :: response_files(:)
      !> Full command line string to execute (e.g., `fpm test -- ...`).
      character(len=:), allocatable :: fpm_cmdline
      !> Selected `fpm` subcommand (`build`, `test`, or `run`).
      character(len=:), allocatable :: subcmd
      !> Typed `fpm` settings corresponding to `subcmd`.
      class(fpm_build_settings), allocatable :: settings
      !> Cached prefix up to and including the subcommand token (for injection).
      character(len=:), allocatable :: cmd_prefix
      !> Cached remainder after the subcommand token (for injection).
      character(len=:), allocatable :: cmd_rest
   end type watch_config_t

end module watch_types
