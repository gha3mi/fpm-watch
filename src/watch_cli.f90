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
!! execution via `execute_command_line` / `fpm_filesystem:run`.
module watch_cli
   use, intrinsic :: iso_fortran_env, only: error_unit
   use watch_types,  only: watch_config_t, watch_opts_t
   use watch_config, only: set_watch_defaults, apply_watch_from_manifest, normalize_watch_opts, push_feature
   use watch_cmdline, only: get_arg, join_argv, parse_int, parse_real
   use fpm_strings,  only: string_t
   use fpm_command_line, only: fpm_build_settings, fpm_run_settings, fpm_test_settings
   use face, only: colorize
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
   subroutine parse_cli_config(cfg)
      use watch_cmdsplice, only: split_cmd_after_subcmd
      type(watch_config_t), intent(out) :: cfg

      character(len=:), allocatable :: fpm_cmdline
      class(fpm_build_settings), allocatable :: settings
      character(len=:), allocatable :: subcmd

      call parse_cli(cfg%w, settings, fpm_cmdline, subcmd)

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
   subroutine parse_cli(w, settings, fpm_cmdline, subcmd)
      type(watch_opts_t), intent(out) :: w
      class(fpm_build_settings), allocatable, intent(out) :: settings
      character(len=:), allocatable, intent(out) :: fpm_cmdline
      character(len=:), allocatable, intent(out) :: subcmd

      integer :: narg, i, subcmd_pos
      character(len=:), allocatable :: a
      character(len=:), allocatable :: profile, build_dir
      character(len=:), allocatable :: compiler, c_compiler, cxx_compiler, archiver
      character(len=:), allocatable :: flag, cflag, cxxflag, ldflag
      character(len=:), allocatable :: runner
      type(string_t), allocatable :: features(:)
      type(string_t), allocatable :: names(:)
      logical :: prune, example

      call set_watch_defaults(w)
      call apply_watch_from_manifest(w)

      prune   = .true.
      example = .false.

      profile = ""
      build_dir = default_build_dir()
      compiler = ""
      c_compiler = ""
      cxx_compiler = ""
      archiver = ""
      flag = ""
      cflag = ""
      cxxflag = ""
      ldflag = ""
      runner = ""

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

         select case (a)
          case ("--profile")
            if (i+1 > narg) call usage_and_stop("--profile requires a value")
            profile = get_arg(i+1)
            i = i + 2
            cycle
          case ("--features")
            if (i+1 > narg) call usage_and_stop("--features requires a value")
            call add_features_csv(features, get_arg(i+1))
            i = i + 2
            cycle
          case ("--build-dir")
            if (i+1 > narg) call usage_and_stop("--build-dir requires a value")
            build_dir = get_arg(i+1)
            i = i + 2
            cycle
          case ("--compiler")
            if (i+1 > narg) call usage_and_stop("--compiler requires a value")
            compiler = get_arg(i+1)
            i = i + 2
            cycle
          case ("--c-compiler")
            if (i+1 > narg) call usage_and_stop("--c-compiler requires a value")
            c_compiler = get_arg(i+1)
            i = i + 2
            cycle
          case ("--cxx-compiler")
            if (i+1 > narg) call usage_and_stop("--cxx-compiler requires a value")
            cxx_compiler = get_arg(i+1)
            i = i + 2
            cycle
          case ("--archiver")
            if (i+1 > narg) call usage_and_stop("--archiver requires a value")
            archiver = get_arg(i+1)
            i = i + 2
            cycle
          case ("--flag")
            if (i+1 > narg) call usage_and_stop("--flag requires a value")
            flag = get_arg(i+1)
            i = i + 2
            cycle
          case ("--c-flag")
            if (i+1 > narg) call usage_and_stop("--c-flag requires a value")
            cflag = get_arg(i+1)
            i = i + 2
            cycle
          case ("--cxx-flag")
            if (i+1 > narg) call usage_and_stop("--cxx-flag requires a value")
            cxxflag = get_arg(i+1)
            i = i + 2
            cycle
          case ("--link-flag")
            if (i+1 > narg) call usage_and_stop("--link-flag requires a value")
            ldflag = get_arg(i+1)
            i = i + 2
            cycle
          case ("--runner")
            if (i+1 > narg) call usage_and_stop("--runner requires a value")
            runner = get_arg(i+1)
            i = i + 2
            cycle
          case ("--prune")
            prune = .true.
            i = i + 1
            cycle
          case ("--no-prune")
            prune = .false.
            i = i + 1
            cycle
          case ("--example")
            example = .true.
            i = i + 1
            cycle
          case default
            if (trim(subcmd) == "run" .or. trim(subcmd) == "test") then
               if (len_trim(a) > 0) then
                  if (a(1:1) /= "-") call push_feature(names, a)
               end if
            end if
            i = i + 1
            cycle
         end select
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
            if (len_trim(runner) > 0) s%runner = trim(runner)
            call set_names(s, names)
         end select
       case ("run")
         allocate(fpm_run_settings :: settings)
         call init_build_settings(settings, build_dir, prune, profile, features, compiler, c_compiler, cxx_compiler, archiver, flag, cflag, cxxflag, ldflag)
         select type (s => settings)
          type is (fpm_run_settings)
            s%example = example
            if (len_trim(runner) > 0) s%runner = trim(runner)
            call set_names(s, names)
         end select
       case default
         call usage_and_stop("unsupported subcommand: " // trim(subcmd) // " (supported: build,test,run)")
      end select

      call normalize_watch_opts(w)
   end subroutine parse_cli

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
      ok = (len_trim(a) >= 8 .and. a(1:8) == "--watch-")
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
      end if

      select case (key)
       case ("--watch-child")
         continue
       case ("--watch-auto-restart")
         continue
       case ("--watch-restart-delay")
         if (len_trim(val) == 0) then
            if (i+1 <= narg) i = i + 1
         end if
       case ("--watch-restart-max")
         if (len_trim(val) == 0) then
            if (i+1 <= narg) i = i + 1
         end if
       case ("--watch-self")
         if (len_trim(val) == 0) then
            if (i+1 <= narg) i = i + 1
         end if

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
         if (len_trim(val) == 0) then
            if (i+1 > narg) call usage_and_stop("--watch-rescan requires a value")
            val = get_arg(i+1)
            i = i + 1
         end if
         w%rescan = parse_real(val, w%rescan)
       case ("--watch-no-rescan")
         w%rescan = 0.0
       case ("--watch-ignore")
         if (len_trim(val) == 0) then
            if (i+1 > narg) call usage_and_stop("--watch-ignore requires a value")
            val = get_arg(i+1)
            i = i + 1
         end if
         call push_feature(w%ignore, trim(val))
       case ("--watch-include")
         if (len_trim(val) == 0) then
            if (i+1 > narg) call usage_and_stop("--watch-include requires a value")
            val = get_arg(i+1)
            i = i + 1
         end if
         call push_feature(w%include, trim(val))
       case ("--watch-feature")
         if (len_trim(val) == 0) then
            if (i+1 > narg) call usage_and_stop("--watch-feature requires a value")
            val = get_arg(i+1)
            i = i + 1
         end if
         call push_feature(w%enabled_features, trim(val))
       case ("--watch-poll")
         if (len_trim(val) == 0) then
            if (i+1 > narg) call usage_and_stop("--watch-poll requires a value")
            val = get_arg(i+1)
            i = i + 1
         end if
         w%poll = parse_real(val, w%poll)
       case ("--watch-debounce")
         if (len_trim(val) == 0) then
            if (i+1 > narg) call usage_and_stop("--watch-debounce requires a value")
            val = get_arg(i+1)
            i = i + 1
         end if
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
         da = iachar('a') - iachar('A')
         do k = 1, len(s)
            c = iachar(s(k:k))
            if (c >= iachar('A') .and. c <= iachar('Z')) then
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
      write(error_unit,'(a)') &
         colorize("fpm-watch: error:", color_fg='red_intense', style='bold_on') // " " // trim(msg)
      write(error_unit,'(a)') ""
      call usage_print_only()
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
      character(len=:), allocatable :: title

      title = colorize( &
         colorize("fpm-watch — Smart file watcher plugin for fpm projects", &
         color_fg='cyan_intense', style='bold_on'), &
         style='underline_on')

      write(error_unit,'(a)') title
      write(error_unit,'(a)') ""
      write(error_unit,'(a)') sec("USAGE")
      write(error_unit,'(a)') "  " // cmd("fpm-watch") // " " // arg("[watch options]") // " " // arg("<build|test|run>") // " " // arg("[fpm options]")
      write(error_unit,'(a)') ""
      write(error_unit,'(a)') "  Everything after " // arg("<build|test|run>") // " is passed to " // cmd("fpm") // " unchanged."
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') sec("WATCH OPTIONS")
      write(error_unit,'(a)') "  --watch-help"
      write(error_unit,'(a)') "      Show this help and exit."
      write(error_unit,'(a)') ""
      write(error_unit,'(a)') "  --watch-quiet | -q"
      write(error_unit,'(a)') "      Quiet mode (errors only)."
      write(error_unit,'(a)') "  --watch-verbose[=N] | -v"
      write(error_unit,'(a)') "      Increase verbosity. N: 0..2 (default: 1)."
      write(error_unit,'(a)') "  --watch-very-verbose | -vv"
      write(error_unit,'(a)') "      Same as verbosity=2 (prints watch list, more details)."
      write(error_unit,'(a)') "  --watch-debug"
      write(error_unit,'(a)') "      Extra debug logs (watch list rebuilds, counts, keys)."
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') "  --watch-deps"
      write(error_unit,'(a)') "      Also watch dependency source trees (including deps under build/)."
      write(error_unit,'(a)') "  --watch-no-deps"
      write(error_unit,'(a)') "      Disable dependency watching (default)."
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') "  --watch-low-cpu"
      write(error_unit,'(a)') "      Use OS sleep instead of busy-waiting (near 0% idle CPU)."
      write(error_unit,'(a)') "  --watch-no-low-cpu"
      write(error_unit,'(a)') "      Disable low CPU mode (default)."
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') "  --watch-poll <sec>"
      write(error_unit,'(a)') "      Poll interval for file checks. Default: 0.5"
      write(error_unit,'(a)') "  --watch-debounce <sec>"
      write(error_unit,'(a)') "      Wait after first change to coalesce edits. Default: 0.2"
      write(error_unit,'(a)') "  --watch-rescan <sec>"
      write(error_unit,'(a)') "      Rebuild the fpm model periodically (slow on big projects). Default: 0 (off)."
      write(error_unit,'(a)') "  --watch-no-rescan"
      write(error_unit,'(a)') "      Disable periodic rebuild (recommended)."
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') "  --watch-run-on-start"
      write(error_unit,'(a)') "      Run the command once immediately on startup. Default: on."
      write(error_unit,'(a)') "  --watch-no-run-on-start"
      write(error_unit,'(a)') "      Only run after a change is detected."
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') "  --watch-silent-fpm"
      write(error_unit,'(a)') "      Suppress fpm output; keep only fpm-watch status lines."
      write(error_unit,'(a)') "  --watch-print-files"
      write(error_unit,'(a)') "      Print the watched file list (limited) at startup."
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') "  --watch-ignore <glob>"
      write(error_unit,'(a)') "      Ignore files matching glob (repeatable)."
      write(error_unit,'(a)') "  --watch-include <glob>"
      write(error_unit,'(a)') "      Only watch files matching glob (repeatable). Empty means include all."
      write(error_unit,'(a)') "  --watch-feature <name>"
      write(error_unit,'(a)') "      Enable optional feature plugin (repeatable)."
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') "  --watch-auto-restart"
      write(error_unit,'(a)') "      Restart fpm-watch automatically if it crashes."
      write(error_unit,'(a)') "  --watch-restart-delay <sec>"
      write(error_unit,'(a)') "      Delay before restart. Default: 1.0"
      write(error_unit,'(a)') "  --watch-restart-max <N>"
      write(error_unit,'(a)') "      Maximum restart attempts (0 = unlimited). Default: 0"
      write(error_unit,'(a)') "  --watch-self <path>"
      write(error_unit,'(a)') "      Explicit path to fpm-watch executable (advanced)."
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') "  --watch-once"
      write(error_unit,'(a)') "      Initialize (and optionally run once) then exit. Good for CI."
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') sec("CONFIGURATION (fpm.toml)")
      write(error_unit,'(a)') "  " // arg("[extra.fpm-watch]") // "  (same names as flags, without the --watch- prefix)"
      write(error_unit,'(a)') "  poll         = 0.5"
      write(error_unit,'(a)') "  debounce     = 0.2"
      write(error_unit,'(a)') "  rescan       = 0.0"
      write(error_unit,'(a)') "  run-on-start = true"
      write(error_unit,'(a)') "  silent-fpm   = false"
      write(error_unit,'(a)') "  print-files  = false"
      write(error_unit,'(a)') "  deps         = false"
      write(error_unit,'(a)') "  low-cpu      = false"
      write(error_unit,'(a)') "  auto-restart = false"
      write(error_unit,'(a)') "  restart-delay= 1.0"
      write(error_unit,'(a)') "  restart-max  = 0"
      write(error_unit,'(a)') "  self         = """""
      write(error_unit,'(a)') "  debug        = false"
      write(error_unit,'(a)') "  verbosity    = 0"
      write(error_unit,'(a)') "  ignore       = []"
      write(error_unit,'(a)') "  include      = []"
      write(error_unit,'(a)') "  features     = []"
      write(error_unit,'(a)') "  once         = false"
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') sec("EXAMPLES")
      write(error_unit,'(a)') "  " // cmd("fpm-watch build")
      write(error_unit,'(a)') "  " // cmd("fpm-watch --watch-low-cpu test")
      write(error_unit,'(a)') "  " // cmd("fpm-watch --watch-deps test") // " " // arg("--") // " " // arg("--list")
      write(error_unit,'(a)') "  " // cmd("fpm-watch --watch-auto-restart test")
      write(error_unit,'(a)') ""

      write(error_unit,'(a)') sec("NOTES")
      write(error_unit,'(a)') "  fpm-watch ignores " // arg("build/") // " to avoid infinite rebuild loops."
      write(error_unit,'(a)') "  Enable " // arg("deps") // " only if you want dependency source changes to trigger runs."
      write(error_unit,'(a)') "  Enable " // arg("low-cpu") // " to avoid busy-waiting and reduce idle CPU usage."
      write(error_unit,'(a)') "  To quit, press Ctrl+C (or create an empty file named " // arg(".fpm-watch.stop") // ")."
      write(error_unit,'(a)') ""

   contains

      function sec(s) result(r)
         character(len=*), intent(in) :: s
         character(len=:), allocatable :: r
         r = colorize(s, color_fg='blue_intense', style='bold_on')
      end function sec

      function cmd(s) result(r)
         character(len=*), intent(in) :: s
         character(len=:), allocatable :: r
         r = colorize(s, color_fg='cyan_intense', style='bold_on')
      end function cmd

      function arg(s) result(r)
         character(len=*), intent(in) :: s
         character(len=:), allocatable :: r
         r = colorize(s, color_fg='yellow_intense')
      end function arg

   end subroutine usage_print_only

   !> Populate `settings` with parsed values and environment fallbacks.
   !!
   !! This routine centralizes:
   !! - `build_dir`, `profile`, `features`,
   !! - Fortran/C/C++ toolchain selection (`FC`, `CC`, `CXX`, `AR`),
   !! - Common compile and link flags (`--flag`, `--c-flag`, `--cxx-flag`, `--link-flag`).
   !!
   !! Environment variables are consulted only when the corresponding CLI option
   !! is empty.
   subroutine init_build_settings(settings, build_dir, prune, profile, features, compiler_in, c_compiler_in, cxx_compiler_in, archiver_in, flag_in, cflag_in, cxxflag_in, ldflag_in)
      class(fpm_build_settings), intent(inout) :: settings
      character(len=*), intent(in) :: build_dir
      logical, intent(in) :: prune
      character(len=*), intent(in) :: profile
      type(string_t), allocatable, intent(in) :: features(:)
      character(len=*), intent(in) :: compiler_in, c_compiler_in, cxx_compiler_in, archiver_in
      character(len=*), intent(in) :: flag_in, cflag_in, cxxflag_in, ldflag_in

      character(len=:), allocatable :: v
      character(len=2048) :: buf
      integer :: n, stat

      settings%verbose = .false.
      settings%prune = prune

      if (len_trim(build_dir) > 0) then
         settings%build_dir = trim(build_dir)
      end if

      if (len_trim(profile) > 0) settings%profile = trim(profile)

      if (allocated(settings%features)) deallocate(settings%features)
      if (allocated(features)) then
         if (size(features) > 0) then
            allocate(settings%features(size(features)))
            settings%features = features
         end if
      end if

      v = trim(compiler_in)
      if (len_trim(v) == 0) then
         buf = ""
         n = 0
         stat = 0
         call get_environment_variable("FC", buf, length=n, status=stat)
         if (stat == 0 .and. n > 0) v = trim(buf(1:n))
      end if
      if (len_trim(v) == 0) v = "gfortran"
      settings%compiler = trim(v)

      v = trim(c_compiler_in)
      if (len_trim(v) == 0) then
         buf = ""
         n = 0
         stat = 0
         call get_environment_variable("CC", buf, length=n, status=stat)
         if (stat == 0 .and. n > 0) v = trim(buf(1:n))
      end if
      if (len_trim(v) > 0) settings%c_compiler = trim(v)

      v = trim(cxx_compiler_in)
      if (len_trim(v) == 0) then
         buf = ""
         n = 0
         stat = 0
         call get_environment_variable("CXX", buf, length=n, status=stat)
         if (stat == 0 .and. n > 0) v = trim(buf(1:n))
      end if
      if (len_trim(v) > 0) settings%cxx_compiler = trim(v)

      v = trim(archiver_in)
      if (len_trim(v) == 0) then
         buf = ""
         n = 0
         stat = 0
         call get_environment_variable("AR", buf, length=n, status=stat)
         if (stat == 0 .and. n > 0) v = trim(buf(1:n))
      end if
      if (len_trim(v) > 0) settings%archiver = trim(v)

      v = trim(flag_in)
      if (len_trim(v) > 0) settings%flag = trim(v)

      v = trim(cflag_in)
      if (len_trim(v) > 0) settings%cflag = trim(v)

      v = trim(cxxflag_in)
      if (len_trim(v) > 0) settings%cxxflag = trim(v)

      v = trim(ldflag_in)
      if (len_trim(v) > 0) settings%ldflag = trim(v)
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

      integer :: n, p, q
      character(len=:), allocatable :: s, tok

      s = trim(csv)
      if (len_trim(s) == 0) return

      p = 1
      n = len_trim(s)

      do
         if (p > n) exit
         q = index(s(p:), ",")
         if (q == 0) then
            tok = adjustl(s(p:n))
            call push_feature(features, trim(tok))
            exit
         else
            tok = adjustl(s(p:p+q-2))
            call push_feature(features, trim(tok))
            p = p + q
         end if
      end do
   end subroutine add_features_csv

end module watch_cli
