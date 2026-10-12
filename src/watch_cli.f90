!> Command-line parsing and configuration assembly.
!!
!! This module:
!! - Applies built-in defaults (`set_watch_defaults`),
!! - Applies project-level overrides from `fpm.toml` (`apply_watch_from_manifest`),
!! - Parses `--watch-*` options and `fpm` subcommand arguments,
!! - Produces a `watch_config_t` that drives the watcher engine.
!!
!! Parsing is deliberately simple and "argv-token based": it does not implement
!! a full shell parser. The `fpm_cmdline` string is built for display and for
!! execution via `watch_platform:execute_command`.
module watch_cli
   use, intrinsic :: iso_fortran_env, only: error_unit
   use watch_types,  only: watch_config_t, watch_opts_t
   use watch_config, only: set_watch_defaults, apply_watch_from_manifest, normalize_watch_opts, push_feature
   use watch_cmdline, only: get_arg, join_argv, parse_int, parse_real, starts_with, watch_option_takes_value
   use fpm_strings,  only: string_t, split
   use fpm_command_line, only: fpm_build_settings, fpm_run_settings, fpm_test_settings, get_fpm_env
   use watch_log, only: configure_logging, valid_log_format, log_event, structured_logging
   implicit none
   private
   public parse_cli_config

contains

   !> Parse CLI arguments and build the full watcher configuration.
   !!
   !! Produces a `watch_config_t`:
   !! - `cfg%w` contains watcher options (polling, debounce, flags, etc.)
   !! - `cfg%settings` holds the typed `fpm` settings for the selected subcommand
   !! - `cfg%fpm_cmdline` is the effective command string
   !! - `cfg%cmd_prefix` and `cfg%cmd_rest` are helper fields used to inject
   !!   target names into `run`/`test` commands.
   !! The optional format retains the startup selection during manifest refresh.
   subroutine parse_cli_config(cfg, log_format)
      use watch_cmdsplice, only: split_cmd_after_subcmd
      type(watch_config_t), intent(out) :: cfg
      character(len=*), intent(in), optional :: log_format

      character(len=:), allocatable :: fpm_cmdline
      class(fpm_build_settings), allocatable :: settings
      character(len=:), allocatable :: subcmd

      call parse_cli(cfg%w, settings, fpm_cmdline, subcmd, log_format)

      cfg%fpm_cmdline = fpm_cmdline
      cfg%subcmd      = subcmd
      call move_alloc(settings, cfg%settings)

      call split_cmd_after_subcmd(cfg%fpm_cmdline, cfg%cmd_prefix, cfg%cmd_rest)
   end subroutine parse_cli_config

   !> Parse the complete CLI and produce watcher options + typed `fpm` settings.
   !!
   !! Parsing order:
   !! 1. Initialize watcher options from defaults and `fpm.toml`.
   !! 2. Consume leading `--watch-*` options and verbosity flags (`-q`, `-v`).
   !! 3. Identify the `fpm` subcommand (`build|test|run`).
   !! 4. Parse selected `fpm` settings used to build the model (profile, flags,
   !!    compilers, build-dir, features, and run/test names).
   !!
   !! All arguments after the subcommand are also preserved in `fpm_cmdline`.
   subroutine parse_cli(w, settings, fpm_cmdline, subcmd, log_format)
      type(watch_opts_t), intent(out) :: w
      class(fpm_build_settings), allocatable, intent(out) :: settings
      character(len=:), allocatable, intent(out) :: fpm_cmdline
      character(len=:), allocatable, intent(out) :: subcmd
      character(len=*), intent(in), optional :: log_format

      integer :: narg, i, subcmd_pos, eq
      character(len=:), allocatable :: a
      character(len=:), allocatable :: profile, build_dir
      character(len=:), allocatable :: compiler, c_compiler, cxx_compiler, archiver
      character(len=:), allocatable :: flag, cflag, cxxflag, ldflag
      character(len=:), allocatable :: runner, runner_args, config_file
      type(string_t), allocatable :: features(:)
      type(string_t), allocatable :: names(:)
      logical :: prune, example, build_tests

      call set_watch_defaults(w)
      call apply_watch_from_manifest(w)
      if (present(log_format)) w%log_format = log_format
      if (valid_log_format(w%log_format)) call configure_logging(w)

      prune   = .true.
      example = .false.
      build_tests = .false.

      profile = ""
      build_dir = get_fpm_env("BUILD_DIR", default_build_dir())
      compiler = get_fpm_env("FC", "gfortran")
      c_compiler = get_fpm_env("CC", " ")
      cxx_compiler = get_fpm_env("CXX", " ")
      archiver = get_fpm_env("AR", " ")
      flag = get_fpm_env("FFLAGS", " ")
      cflag = get_fpm_env("CFLAGS", " ")
      cxxflag = get_fpm_env("CXXFLAGS", " ")
      ldflag = get_fpm_env("LDFLAGS", " ")
      runner = ""
      runner_args = ""
      config_file = ""

      allocate(features(0))
      allocate(names(0))

      narg = command_argument_count()
      if (narg < 1) then
         call usage_and_stop("missing fpm subcommand (build/test/run)")
      end if

      a = get_arg(1)
      if (a == "--help" .or. a == "-h") then
         call usage_and_stop_ok()
      end if

      i = 1
      do while (i <= narg)
         a = get_arg(i)

         if (is_watch_flag(a)) then
            call apply_watch_flag(a, i, narg, w)
            i = i + 1
            cycle
         end if

         if (a == "-q") then
            w%verbosity = -1
            i = i + 1
            cycle
         end if
         if (a == "-v") then
            w%verbosity = max(w%verbosity, 1)
            i = i + 1
            cycle
         end if
         if (a == "-vv") then
            w%verbosity = max(w%verbosity, 2)
            i = i + 1
            cycle
         end if

         exit
      end do

      if (present(log_format)) w%log_format = log_format
      if (.not. valid_log_format(w%log_format)) then
         call usage_and_stop("invalid log-format: " // w%log_format // "; use text, json, or logfmt")
      end if
      call configure_logging(w)
      if (i > narg) call usage_and_stop("missing fpm subcommand (build/test/run)")

      a = get_arg(i)
      if (a == "--help" .or. a == "-h") then
         call usage_and_stop_ok()
      end if

      subcmd_pos = i
      subcmd = get_arg(subcmd_pos)

      if (.not. is_supported_subcmd(subcmd)) then
         call usage_and_stop("unsupported subcommand: " // trim(subcmd) // " (supported: build,test,run)")
      end if

      if (subcmd_pos < narg) then
         fpm_cmdline = "fpm " // trim(subcmd) // " " // join_argv(subcmd_pos+1, narg)
      else
         fpm_cmdline = "fpm " // trim(subcmd)
      end if

      i = subcmd_pos + 1
      do while (i <= narg)
         a = get_arg(i)
         if (a == "--") exit

         if (starts_with(a, "--")) then
            eq = index(a, "=")
            if (eq > 0) a = a(:eq-1)
         end if

         select case (a)
          case ("--profile")
            profile = fpm_option_value(i, narg)
          case ("--features")
            call add_features_csv(features, fpm_option_value(i, narg))
          case ("--build-dir")
            build_dir = fpm_option_value(i, narg)
          case ("--compiler")
            compiler = fpm_option_value(i, narg)
          case ("--c-compiler")
            c_compiler = fpm_option_value(i, narg)
          case ("--cxx-compiler")
            cxx_compiler = fpm_option_value(i, narg)
          case ("--archiver")
            archiver = fpm_option_value(i, narg)
          case ("--flag")
            flag = fpm_option_value(i, narg)
          case ("--c-flag")
            cflag = fpm_option_value(i, narg)
          case ("--cxx-flag")
            cxxflag = fpm_option_value(i, narg)
          case ("--link-flag")
            ldflag = fpm_option_value(i, narg)
          case ("--runner")
            runner = fpm_option_value(i, narg)
          case ("--runner-args")
            runner_args = fpm_option_value(i, narg)
          case ("--config-file")
            config_file = fpm_option_value(i, narg)
          case ("--target")
            call add_target_names(names, fpm_option_value(i, narg))
          case ("--all")
            call push_feature(names, "*")
          case ("--tests")
            build_tests = .true.
          case ("--prune")
            prune = .true.
          case ("--no-prune")
            prune = .false.
          case ("--example")
            example = .true.
          case default
            if (trim(subcmd) == "run" .or. trim(subcmd) == "test") then
               if (len(a) > 0) then
                  if (a(1:1) /= "-") call push_feature(names, target_pattern(a))
               end if
            end if
         end select
         i = i + 1
      end do

      select case (trim(subcmd))
       case ("build")
         allocate(fpm_build_settings :: settings)
         call init_build_settings(settings, build_dir, prune, profile, features, compiler, c_compiler, cxx_compiler, archiver, flag, cflag, cxxflag, ldflag)
       case ("test")
         allocate(fpm_test_settings :: settings)
         call init_build_settings(settings, build_dir, prune, profile, features, compiler, c_compiler, cxx_compiler, archiver, flag, cflag, cxxflag, ldflag)
         select type (s => settings)
          type is (fpm_test_settings)
            s%build_tests = .true.
            if (len(runner) > 0) s%runner = runner
            if (len(runner_args) > 0) s%runner_args = runner_args
            call set_names(s, names)
         end select
       case ("run")
         allocate(fpm_run_settings :: settings)
         call init_build_settings(settings, build_dir, prune, profile, features, compiler, c_compiler, cxx_compiler, archiver, flag, cflag, cxxflag, ldflag)
         select type (s => settings)
          type is (fpm_run_settings)
            s%example = example
            if (len(runner) > 0) s%runner = runner
            if (len(runner_args) > 0) s%runner_args = runner_args
            call set_names(s, names)
         end select
       case default
         call usage_and_stop("unsupported subcommand: " // trim(subcmd) // " (supported: build,test,run)")
      end select

      settings%path_to_config = config_file
      if (trim(subcmd) == "build") settings%build_tests = build_tests

      call normalize_watch_opts(w)
   end subroutine parse_cli

   !> Read an fpm option value from `--key=value` or the following argv token.
   function fpm_option_value(i, narg) result(value)
      integer, intent(inout) :: i
      integer, intent(in) :: narg
      character(len=:), allocatable :: value, option
      integer :: eq

      option = get_arg(i)
      eq = index(option, "=")
      if (eq > 0) then
         value = option(eq+1:)
      else
         if (i >= narg) call usage_and_stop(option // " requires a value")
         i = i + 1
         value = get_arg(i)
      end if
   end function fpm_option_value

   !> Expand fpm's target-list delimiters using the dependency's string utility.
   subroutine add_target_names(names, value)
      type(string_t), allocatable, intent(inout) :: names(:)
      character(len=*), intent(in) :: value
      character(len=:), allocatable :: targets(:)
      integer :: j

      call split(value, targets, delimiters=" ,:")
      do j = 1, size(targets)
         call push_feature(names, target_pattern(trim(targets(j))))
      end do
   end subroutine add_target_names

   !> Match fpm's portable `..` spelling of the wildcard target pattern.
   pure function target_pattern(value) result(pattern)
      character(len=*), intent(in) :: value
      character(len=:), allocatable :: pattern
      pattern = value
      if (value == "..") pattern = "*"
   end function target_pattern

   !> Return whether a given subcommand token is supported by `fpm-watch`.
   pure logical function is_supported_subcmd(s) result(ok)
      character(len=*), intent(in) :: s
      ok = (trim(s) == "build") .or. (trim(s) == "test") .or. (trim(s) == "run")
   end function is_supported_subcmd

   !> Detect whether an argv token is a watcher-specific flag.
   !!
   !! Watcher flags are of the form `--watch-*` and are consumed by `fpm-watch`
   !! itself rather than forwarded to `fpm`.
   pure logical function is_watch_flag(a) result(ok)
      character(len=*), intent(in) :: a
      ok = starts_with(a, "--watch-")
   end function is_watch_flag

   !> Apply a single `--watch-*` flag to the watcher options.
   !!
   !! Supports both `--flag=value` and `--flag value` styles for flags that take
   !! a value.
   !!
   !! This routine is called while scanning the argv prefix before the `fpm`
   !! subcommand token.
   subroutine apply_watch_flag(a, i, narg, w)
      character(len=*), intent(in) :: a
      integer, intent(inout) :: i
      integer, intent(in) :: narg
      type(watch_opts_t), intent(inout) :: w

      character(len=:), allocatable :: key, val
      integer :: eq

      key = trim(a)
      val = ""

      eq = index(key, "=")
      if (eq > 0) then
         val = key(eq+1:)
         key = key(1:eq-1)
      else if (watch_option_takes_value(a)) then
         if (i >= narg) call usage_and_stop(key // " requires a value")
         i = i + 1
         val = get_arg(i)
      end if

      select case (key)
       case ("--watch-child")
         continue
       case ("--watch-auto-restart")
         continue
       case ("--watch-restart-delay")
         continue
       case ("--watch-restart-max")
         continue
       case ("--watch-self")
         continue

       case ("--watch-deps")
         if (len_trim(val) > 0) then
            w%watch_deps = parse_bool(val, .true.)
         else
            w%watch_deps = .true.
         end if
       case ("--watch-no-deps")
         w%watch_deps = .false.

       case ("--watch-low-cpu")
         if (len_trim(val) > 0) then
            w%low_cpu = parse_bool(val, .true.)
         else
            w%low_cpu = .true.
         end if
       case ("--watch-no-low-cpu")
         w%low_cpu = .false.

       case ("--watch-quiet")
         w%verbosity = -1
       case ("--watch-log-format")
         if (.not. valid_log_format(val)) call usage_and_stop("--watch-log-format requires text, json, or logfmt")
         w%log_format = val
         call configure_logging(w)
       case ("--watch-verbose")
         if (len_trim(val) > 0) then
            w%verbosity = max(w%verbosity, parse_int(val, 1))
         else
            w%verbosity = max(w%verbosity, 1)
         end if
       case ("--watch-very-verbose")
         w%verbosity = max(w%verbosity, 2)
       case ("--watch-debug")
         w%debug = .true.
       case ("--watch-print-files")
         w%print_files_once = .true.
         w%verbosity = max(w%verbosity, 2)
       case ("--watch-silent-fpm")
         w%silent_fpm = .true.
       case ("--watch-run-on-start")
         w%run_on_start = .true.
       case ("--watch-no-run-on-start")
         w%run_on_start = .false.
       case ("--watch-rescan")
         w%rescan = parse_real(val, w%rescan)
       case ("--watch-no-rescan")
         w%rescan = 0.0
       case ("--watch-ignore")
         call push_feature(w%ignore, trim(val))
       case ("--watch-include")
         call push_feature(w%include, trim(val))
       case ("--watch-feature")
         call push_feature(w%enabled_features, trim(val))
       case ("--watch-poll")
         w%poll = parse_real(val, w%poll)
       case ("--watch-debounce")
         w%debounce = parse_real(val, w%debounce)
       case ("--watch-once")
         if (len_trim(val) > 0) then
            w%once = parse_bool(val, .true.)
         else
            w%once = .true.
         end if
       case ("--watch-help")
         call usage_and_stop_ok()
       case default
         call usage_and_stop("unknown watch flag: " // trim(a))
      end select

      call normalize_watch_opts(w)

   contains

      pure logical function parse_bool(s, default) result(v)
         character(len=*), intent(in) :: s
         logical, intent(in) :: default
         character(len=:), allocatable :: t
         t = trim(adjustl(lower_ascii(s)))
         select case (t)
          case ("1","true","on","yes","y","t")
            v = .true.
          case ("0","false","off","no","n","f")
            v = .false.
          case default
            v = default
         end select
      end function parse_bool

      pure function lower_ascii(s) result(r)
         character(len=*), intent(in) :: s
         character(len=len(s)) :: r
         integer :: k, c, da
         da = iachar("a") - iachar("A")
         do k = 1, len(s)
            c = iachar(s(k:k))
            if (c >= iachar("A") .and. c <= iachar("Z")) then
               r(k:k) = achar(c + da)
            else
               r(k:k) = s(k:k)
            end if
         end do
      end function lower_ascii

   end subroutine apply_watch_flag

   !> Print a usage message and terminate with a non-zero exit status.
   subroutine usage_and_stop(msg)
      character(len=*), intent(in) :: msg
      call log_event("error", "cli.error", msg)
      if (.not. structured_logging()) write(error_unit,"(a)") "Use fpm-watch --watch-help for usage."
      stop 2
   end subroutine usage_and_stop

   !> Print a usage message and terminate successfully.
   subroutine usage_and_stop_ok()
      call usage_print_only()
      stop 0
   end subroutine usage_and_stop_ok

   !> Print the `fpm-watch` help text.
   !!
   !! The help includes:
   !! - Watcher options (`--watch-*`),
   !! - Project configuration (`[extra.fpm-watch]`),
   !! - Examples and behavioral notes.
   subroutine usage_print_only()
      write(error_unit,"(a)") "fpm-watch — Smart file watcher plugin for fpm projects"
      write(error_unit,"(a)") ""
      write(error_unit,"(a)") "USAGE"
      write(error_unit,"(a)") "  fpm-watch [watch options] <build|test|run> [fpm options]"
      write(error_unit,"(a)") ""
      write(error_unit,"(a)") "  Everything after <build|test|run> is passed to fpm unchanged."
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "WATCH OPTIONS"
      write(error_unit,"(a)") "  --watch-help"
      write(error_unit,"(a)") "      Show this help and exit."
      write(error_unit,"(a)") ""
      write(error_unit,"(a)") "  --watch-quiet | -q"
      write(error_unit,"(a)") "      Quiet mode (errors only)."
      write(error_unit,"(a)") "  --watch-log-format <text|json|logfmt>"
      write(error_unit,"(a)") "      Log format (default: text). Structured logs use stdout; fpm output uses stderr."
      write(error_unit,"(a)") "  --watch-verbose[=N] | -v"
      write(error_unit,"(a)") "      Increase verbosity. N: 0..2 (default: 1)."
      write(error_unit,"(a)") "  --watch-very-verbose | -vv"
      write(error_unit,"(a)") "      Same as verbosity=2 (prints watch list, more details)."
      write(error_unit,"(a)") "  --watch-debug"
      write(error_unit,"(a)") "      Extra debug logs (watch list rebuilds, counts, keys)."
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "  --watch-deps"
      write(error_unit,"(a)") "      Also watch dependency source trees (including deps under build/)."
      write(error_unit,"(a)") "  --watch-no-deps"
      write(error_unit,"(a)") "      Disable dependency watching (default)."
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "  --watch-low-cpu"
      write(error_unit,"(a)") "      Use OS sleep instead of busy-waiting (near 0% idle CPU)."
      write(error_unit,"(a)") "  --watch-no-low-cpu"
      write(error_unit,"(a)") "      Disable low CPU mode (default)."
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "  --watch-poll <sec>"
      write(error_unit,"(a)") "      Poll interval for file checks. Default: 0.5"
      write(error_unit,"(a)") "  --watch-debounce <sec>"
      write(error_unit,"(a)") "      Wait after first change to coalesce edits. Default: 0.2"
      write(error_unit,"(a)") "  --watch-rescan <sec>"
      write(error_unit,"(a)") "      Rebuild the fpm model periodically (slow on big projects). Default: 0 (off)."
      write(error_unit,"(a)") "  --watch-no-rescan"
      write(error_unit,"(a)") "      Disable periodic rebuild (recommended)."
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "  --watch-run-on-start"
      write(error_unit,"(a)") "      Run the command once immediately on startup. Default: on."
      write(error_unit,"(a)") "  --watch-no-run-on-start"
      write(error_unit,"(a)") "      Only run after a change is detected."
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "  --watch-silent-fpm"
      write(error_unit,"(a)") "      Suppress fpm output; keep only fpm-watch status lines."
      write(error_unit,"(a)") "  --watch-print-files"
      write(error_unit,"(a)") "      Print the watched file list (limited) at startup."
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "  --watch-ignore <glob>"
      write(error_unit,"(a)") "      Ignore files matching glob (repeatable)."
      write(error_unit,"(a)") "  --watch-include <glob>"
      write(error_unit,"(a)") "      Only watch files matching glob (repeatable). Empty means include all."
      write(error_unit,"(a)") "  --watch-feature <name>"
      write(error_unit,"(a)") "      Enable optional feature plugin (repeatable)."
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "  --watch-auto-restart"
      write(error_unit,"(a)") "      Restart fpm-watch automatically if it crashes."
      write(error_unit,"(a)") "  --watch-restart-delay <sec>"
      write(error_unit,"(a)") "      Delay before restart. Default: 1.0"
      write(error_unit,"(a)") "  --watch-restart-max <N>"
      write(error_unit,"(a)") "      Maximum restart attempts (0 = unlimited). Default: 0"
      write(error_unit,"(a)") "  --watch-self <path>"
      write(error_unit,"(a)") "      Explicit path to fpm-watch executable (advanced)."
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "  --watch-once"
      write(error_unit,"(a)") "      Initialize (and optionally run once) then exit. Good for CI."
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "CONFIGURATION (fpm.toml)"
      write(error_unit,"(a)") "  [extra.fpm-watch]  (same names as flags, without the --watch- prefix)"
      write(error_unit,"(a)") "  poll         = 0.5"
      write(error_unit,"(a)") "  debounce     = 0.2"
      write(error_unit,"(a)") "  rescan       = 0.0"
      write(error_unit,"(a)") "  run-on-start = true"
      write(error_unit,"(a)") "  silent-fpm   = false"
      write(error_unit,"(a)") "  print-files  = false"
      write(error_unit,"(a)") "  deps         = false"
      write(error_unit,"(a)") "  low-cpu      = false"
      write(error_unit,"(a)") "  auto-restart = false"
      write(error_unit,"(a)") "  restart-delay= 1.0"
      write(error_unit,"(a)") "  restart-max  = 0"
      write(error_unit,"(a)") "  self         = """""
      write(error_unit,"(a)") "  debug        = false"
      write(error_unit,"(a)") "  verbosity    = 0"
      write(error_unit,"(a)") '  log-format   = "text"'
      write(error_unit,"(a)") "  ignore       = []"
      write(error_unit,"(a)") "  include      = []"
      write(error_unit,"(a)") "  features     = []"
      write(error_unit,"(a)") "  once         = false"
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "EXAMPLES"
      write(error_unit,"(a)") "  fpm-watch build"
      write(error_unit,"(a)") "  fpm-watch --watch-low-cpu test"
      write(error_unit,"(a)") "  fpm-watch --watch-deps test -- --list"
      write(error_unit,"(a)") "  fpm-watch --watch-auto-restart test"
      write(error_unit,"(a)") ""

      write(error_unit,"(a)") "NOTES"
      write(error_unit,"(a)") "  fpm-watch ignores build/ to avoid infinite rebuild loops."
      write(error_unit,"(a)") "  Enable deps only if you want dependency source changes to trigger runs."
      write(error_unit,"(a)") "  Enable low-cpu to avoid busy-waiting and reduce idle CPU usage."
      write(error_unit,"(a)") "  To quit, press Ctrl+C (or create an empty file named .fpm-watch.stop)."
      write(error_unit,"(a)") ""

   end subroutine usage_print_only

   !> Populate `settings` with argv overrides of fpm environment defaults.
   !!
   !! This routine centralizes:
   !! - `build_dir`, `profile`, `features`,
   !! - Fortran/C/C++ toolchain selection (`FPM_FC`, `FPM_CC`, `FPM_CXX`, `FPM_AR`),
   !! - Common compile and link flags (`--flag`, `--c-flag`, `--cxx-flag`, `--link-flag`).
   !!
   !! Defaults are read with `fpm_command_line:get_fpm_env` before argv parsing,
   !! so explicit option values always override the environment.
   subroutine init_build_settings(settings, build_dir, prune, profile, features, compiler_in, c_compiler_in, cxx_compiler_in, archiver_in, flag_in, cflag_in, cxxflag_in, ldflag_in)
      class(fpm_build_settings), intent(inout) :: settings
      character(len=*), intent(in) :: build_dir
      logical, intent(in) :: prune
      character(len=*), intent(in) :: profile
      type(string_t), allocatable, intent(in) :: features(:)
      character(len=*), intent(in) :: compiler_in, c_compiler_in, cxx_compiler_in, archiver_in
      character(len=*), intent(in) :: flag_in, cflag_in, cxxflag_in, ldflag_in

      settings%verbose = .false.
      settings%prune = prune
      settings%build_dir = build_dir
      if (len_trim(profile) > 0) settings%profile = trim(profile)

      if (allocated(settings%features)) deallocate(settings%features)
      if (allocated(features)) then
         if (size(features) > 0) then
            allocate(settings%features(size(features)))
            settings%features = features
         end if
      end if

      settings%compiler = compiler_in
      settings%c_compiler = c_compiler_in
      settings%cxx_compiler = cxx_compiler_in
      settings%archiver = archiver_in
      settings%flag = flag_in
      settings%cflag = cflag_in
      settings%cxxflag = cxxflag_in
      settings%ldflag = ldflag_in
   end subroutine init_build_settings

   !> Return the default build directory used by `fpm-watch`.
   pure function default_build_dir() result(dir)
      character(len=:), allocatable :: dir
      dir = "build"
   end function default_build_dir

   !> Transfer collected name patterns into a run/test settings object.
   !!
   !! The `names` array contains tokens collected from positional arguments in
   !! `fpm-watch run/test` invocations. Only non-empty entries are copied.
   pure subroutine set_names(s, names)
      class(fpm_run_settings), intent(inout) :: s
      type(string_t), allocatable, intent(in) :: names(:)
      integer :: i, k, n

      if (.not. allocated(names)) return

      n = 0
      do i = 1, size(names)
         if (len_trim(names(i)%s) > 0) n = n + 1
      end do
      if (n == 0) return

      allocate(s%name(n))
      k = 0
      do i = 1, size(names)
         if (len_trim(names(i)%s) == 0) cycle
         k = k + 1
         s%name(k) = trim(names(i)%s)
      end do
   end subroutine set_names

   !> Parse a comma-separated `--features` value into the features array.
   !!
   !! Empty tokens are ignored; whitespace around tokens is stripped.
   subroutine add_features_csv(features, csv)
      type(string_t), allocatable, intent(inout) :: features(:)
      character(len=*), intent(in) :: csv

      character(len=:), allocatable :: tokens(:)
      integer :: j

      call split(csv, tokens, delimiters=",")
      do j = 1, size(tokens)
         call push_feature(features, trim(adjustl(tokens(j))))
      end do
   end subroutine add_features_csv

end module watch_cli
