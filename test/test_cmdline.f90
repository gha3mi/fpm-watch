!> Regression checks for argv serialization, target parsing, and name insertion.
program test_cmdline
   use, intrinsic :: iso_fortran_env, only: error_unit
   use fpm_strings, only: string_t
   use fpm_command_line, only: fpm_run_settings
   use watch_types, only: watch_config_t
   use watch_cli, only: parse_cli_config
   use watch_cmdline, only: get_arg, join_argv, quote_arg, is_windows_os, watch_option_takes_value, &
                            parse_bool, parse_int, parse_real
   use watch_platform, only: execute_command
   use watch_cmdsplice, only: split_cmd_after_subcmd, inject_names_into_cmd
   implicit none

   type(string_t), allocatable :: values(:)
   type(watch_config_t) :: cfg
   character(len=:), allocatable :: mode, cmd, actual, expected, prefix, rest, runner
   integer :: i, code, number
   real :: seconds
   logical :: ok, enabled
   character(len=24), parameter :: invalid_numbers(*) = [character(len=24) :: &
      "", "/", ",", "1,2", "1 2", "2*3", "NaN", "Infinity", "+", ".", "1e", "1e+", "1/", "1;2"]

   values = [string_t(""), string_t(" "), string_t("tail  "), string_t("*.toml"), &
      string_t("hello;true"), string_t("cash$USER"), string_t("single'quote"), string_t('double"quote'), &
      string_t("C:" // achar(92) // "dir" // achar(92)), string_t("slash" // achar(92) // '"quote'), &
      string_t("%PATH%"), string_t("!PATH!")]

   if (command_argument_count() > 0) then
      mode = get_arg(1)
      if (mode == "--check-argv") then
         call require(command_argument_count() == size(values) + 1, "forwarded argument count")
         expected = ""
         do i = 1, size(values)
            actual = get_arg(i + 1)
            if (len(actual) /= len(values(i)%s) .or. actual /= values(i)%s) then
               write(error_unit, "(a,i0,a,i0,a,i0)") "argument ", i, ": expected length ", &
                  len(values(i)%s), ", actual length ", len(actual)
               write(error_unit, "(a)") "expected: <" // values(i)%s // ">; actual: <" // actual // ">"
            end if
            call require(len(actual) == len(values(i)%s), "forwarded argument length")
            call require(actual == values(i)%s, "forwarded argument contents")
            if (i > 1) expected = expected // " "
            expected = expected // quote_arg(values(i)%s)
         end do
         call require(join_argv(2, command_argument_count()) == expected, "joined argv preserves all tokens")
      else if (mode == "--parse-watch") then
         call parse_cli_config(cfg, start_arg=2)
         call require(.not. cfg%w%run_on_start, "explicit false startup setting")
         call require(.not. cfg%w%watch_deps, "explicit false dependency setting")
         call require(cfg%w%verbosity == 2, "verbosity 2 replaces diagnostic options")
         call require(cfg%w%restart_max == 3, "strict nonnegative restart limit")
         call require(abs(cfg%w%poll - 0.125) < 1.0e-6, "scientific duration value")
         call require(size(cfg%w%ignore) == 1, "space-valued command-name glob retained")
         call require(cfg%w%ignore(1)%s == "run", "command-name glob is not a subcommand")
      else if (mode == "--watch-fpm" .or. mode == "--watch-fpm=custom fpm") then
         call parse_cli_config(cfg)
         call require(cfg%w%fpm_executable == "custom fpm", "custom executable preserved")
         call require(.not. cfg%w%silent_fpm, "CLI overrides silent manifest default")
         call require(cfg%cmd_prefix == quote_arg("custom fpm") // " run", "quoted executable prefix")
         call require(cfg%fpm_cmdline == cfg%cmd_prefix // " " // cfg%cmd_rest, "custom command rebuilt")
      else if (mode == "run") then
         call parse_cli_config(cfg)
         select type (s => cfg%settings)
          type is (fpm_run_settings)
            call require(allocated(s%name), "target list allocated")
            call require(size(s%name) == 4, "fpm target-list delimiters")
            call require(trim(s%name(1)) == "one" .and. trim(s%name(2)) == "two", "comma-separated targets")
            call require(trim(s%name(3)) == "three" .and. trim(s%name(4)) == "four", "colon and space targets")
            call require(s%profile == "release", "equals-style profile")
            call require(s%runner_args == "120", "runner arguments are not target names")
          class default
            call require(.false., "run settings type")
         end select
      else
         call require(.false., "unknown child-test mode")
      end if
      stop
   end if

   call require(quote_arg("", windows=.true.) == '""', "Windows empty argument")
   call require(quote_arg("tail  ", windows=.true.) == '"tail  "', "Windows trailing spaces")
   call require(quote_arg('double"quote', windows=.true.) == '"double' // achar(92) // '"quote"', &
      "Windows embedded quote")
   call require(quote_arg("dir" // achar(92), windows=.true.) == '"dir' // repeat(achar(92), 2) // '"', &
         "Windows trailing backslash")
   call require(quote_arg("slash" // achar(92) // '"quote', windows=.true.) == &
      '"slash' // repeat(achar(92), 3) // '"quote"', "Windows backslash before embedded quote")
   if (.not. is_windows_os()) then
      call require(quote_arg("") == "''", "POSIX empty argument")
      call require(quote_arg("tail  ") == "'tail  '", "POSIX trailing spaces")
   end if
   call require(watch_option_takes_value("--watch-ignore"), "watch option consumes value")
   call require(.not. watch_option_takes_value("--watch-ignore=run"), "equals-style watch option arity")
   call parse_bool(" YES ", enabled, ok)
   call require(ok .and. enabled, "case-insensitive true value")
   call parse_bool("OFF", enabled, ok)
   call require(ok .and. .not. enabled, "case-insensitive false value")
   call parse_bool("invalid", enabled, ok)
   call require(.not. ok, "invalid boolean rejected")
   call parse_bool("", enabled, ok)
   call require(.not. ok, "explicit empty boolean rejected")
   call parse_bool("true,false", enabled, ok)
   call require(.not. ok, "multiple boolean values rejected")
   call parse_int(" +12 ", number, ok)
   call require(ok .and. number == 12, "signed integer accepted")
   call parse_int("999999999999999999999999", number, ok)
   call require(.not. ok, "integer overflow rejected")
   call parse_int("1.0", number, ok)
   call require(.not. ok, "integer rejects real syntax")
   call parse_real(" +.125D+1 ", seconds, ok)
   call require(ok .and. abs(seconds - 1.25) < 1.0e-6, "decimal and exponent real accepted")
   call parse_real("1e9999", seconds, ok)
   call require(.not. ok, "real overflow rejected")
   do i = 1, size(invalid_numbers)
      call parse_int(trim(invalid_numbers(i)), number, ok)
      call require(.not. ok, "invalid integer rejected: " // trim(invalid_numbers(i)))
      call parse_real(trim(invalid_numbers(i)), seconds, ok)
      call require(.not. ok, "invalid real rejected: " // trim(invalid_numbers(i)))
   end do

   cmd = quote_arg(get_arg(0)) // " --check-argv"
   do i = 1, size(values)
      cmd = cmd // " " // quote_arg(values(i)%s)
   end do
   call run_child(cmd, "argv roundtrip")

   cmd = quote_arg(get_arg(0)) // " run --target " // quote_arg("one,two:three four") // &
      " --profile=release --runner-args=120"
   call run_child(cmd, "separate target value")
   cmd = quote_arg(get_arg(0)) // " run --target=" // quote_arg("one,two:three four") // &
      " --profile=release --runner-args=120"
   call run_child(cmd, "equals-style target value")

   cmd = quote_arg(get_arg(0)) // " --watch-fpm " // quote_arg("custom fpm") // &
      " --watch-silent-fpm --watch-silent-fpm=false run --profile=release"
   call run_child(cmd, "custom executable and output override")
   cmd = quote_arg(get_arg(0)) // " " // quote_arg("--watch-fpm=custom fpm") // &
      " --watch-silent-fpm --watch-silent-fpm=false run --profile=release"
   call run_child(cmd, "equals-style custom executable")

   cmd = quote_arg(get_arg(0)) // " --parse-watch --watch-run-on-start=false --watch-deps=false" // &
      " --watch-verbose=2 --watch-restart-max=3 --watch-poll=1.25e-1 --watch-ignore run build"
   call run_child(cmd, "consolidated watcher options")

   runner = quote_arg("echo -- marker")
   rest = quote_arg("--runner") // " " // runner
   expected = "fpm run " // rest // " first"
   call require(inject_names_into_cmd("fpm run", rest, "first") == expected, "quoted runner delimiter ignored")
   rest = rest // " " // quote_arg("--") // " " // quote_arg(" tail ")
   expected = "fpm run " // quote_arg("--runner") // " " // runner // " first " // &
      quote_arg("--") // " " // quote_arg(" tail ")
   call require(inject_names_into_cmd("fpm run", rest, "first") == expected, "serialized argv delimiter located")

   runner = quote_arg("echo 'quoted' -- marker")
   rest = "--runner " // runner // " -- " // quote_arg("--program-flag")
   expected = "fpm run --runner " // runner // " first -- " // quote_arg("--program-flag")
   call require(inject_names_into_cmd("fpm run", rest, "first") == expected, "escaped quotes keep runner intact")
   call split_cmd_after_subcmd("fpm run " // rest, prefix, actual)
   call require(prefix == "fpm run" .and. actual == rest, "command prefix and remainder")

   runner = quote_arg('echo "quoted -- marker"', windows=.true.)
   rest = "--runner " // runner // ' "--" "tail"'
   expected = "fpm run --runner " // runner // ' first "--" "tail"'
   call require(inject_names_into_cmd("fpm run", rest, "first", windows=.true.) == expected, &
      "Windows escaped quotes preserve runner and delimiter")

contains

   !> Assert a regression condition with an actionable failure message.
   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message
      if (condition) return
      write(error_unit, "(a)") "test_cmdline: " // message
      error stop 1
   end subroutine require

   !> Launch this test as a child to verify actual argv reconstruction.
   subroutine run_child(command, message)
      character(len=*), intent(in) :: command, message
      call execute_command(command, exitstat=code)
      call require(code == 0, message // ": child result")
   end subroutine run_child

end program test_cmdline
