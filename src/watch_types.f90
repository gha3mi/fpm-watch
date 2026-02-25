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
   public watch_opts_t, root_info_t, watch_config_t

   !> Watcher configuration options.
   !!
   !! Values are typically assembled from:
   !! - hard-coded defaults,
   !! - `fpm.toml` overrides (`[extra.fpm-watch]`),
   !! - command-line flags (`--watch-*`).
   !!
   !! Time values (`poll`, `debounce`, `rescan`) are in seconds.
   type watch_opts_t
      !! Verbosity level:
      !! - `-1`: quiet (errors only)
      !! - `0`: normal
      !! - `1..2`: more detail
      integer :: verbosity = 0
      !! Enable extra debug output and internal timing/status messages.
      logical :: debug = .false.
      !! Poll interval (seconds) between change scans.
      real    :: poll = 0.5
      !! Debounce interval (seconds) used after the first detected change.
      real    :: debounce = 0.2
      !! Optional periodic rescan interval (seconds). `0` disables periodic rescans.
      real    :: rescan = 0.0
      !! Run the command once immediately on startup.
      logical :: run_on_start = .true.
      !! Print the watched file list once at startup (requires high verbosity).
      logical :: print_files_once = .false.
      !! Suppress `fpm` output; keep only `fpm-watch` status lines.
      logical :: silent_fpm = .false.
      !! Also watch dependency project sources (can include paths under build dir).
      logical :: watch_deps = .false.
      !! Use OS sleep for waiting to minimize idle CPU usage.
      logical :: low_cpu = .false.
      !! Glob patterns to ignore.
      type(string_t), allocatable :: ignore(:)
      !! Glob patterns to include; if empty, includes all.
      type(string_t), allocatable :: include(:)
      !! Names of optional feature plugins to enable.
      type(string_t), allocatable :: enabled_features(:)
      !! Run once (init + optional run-on-start) and exit (CI-friendly).
      logical :: once = .false.
   end type watch_opts_t

   !> Root target metadata used for change → target mapping.
   !!
   !! The `mask` is a single-bit value used to mark membership of files and
   !! target closures.
   type root_info_t
      character(len=:), allocatable :: name
      integer(int64) :: mask = 0_int64
   end type root_info_t

   !> Fully assembled watcher configuration.
   !!
   !! This record is the primary input to `watcher_t%init()`.
   type watch_config_t
      !! Watcher options.
      type(watch_opts_t) :: w
      !! Full command line string to execute (e.g., `fpm test -- ...`).
      character(len=:), allocatable :: fpm_cmdline
      !! Selected `fpm` subcommand (`build`, `test`, or `run`).
      character(len=:), allocatable :: subcmd
      !! Typed `fpm` settings corresponding to `subcmd`.
      class(fpm_build_settings), allocatable :: settings
      !! Cached prefix up to and including the subcommand token (for injection).
      character(len=:), allocatable :: cmd_prefix
      !! Cached remainder after the subcommand token (for injection).
      character(len=:), allocatable :: cmd_rest
   end type watch_config_t

end module watch_types
