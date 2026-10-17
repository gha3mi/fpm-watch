!> Keep global changes global when selecting affected run/test targets.
program check_exec
   use, intrinsic :: iso_fortran_env, only: int64
   use fpm_command_line, only: fpm_run_settings, fpm_test_settings
   use watch_types, only: root_info_t
   use watch_exec, only: build_run_command
   use watch_cmdline, only: quote_arg
   implicit none
   type(fpm_run_settings) :: run
   type(fpm_test_settings) :: tests
   type(root_info_t), allocatable :: roots(:)
   integer(int64), allocatable :: masks(:)
   character(len=:), allocatable :: command

   allocate(run%name(0), tests%name(0), roots(2))
   roots(1)%name = "first"
   roots(1)%mask = 1_int64
   roots(2)%name = "second"
   roots(2)%mask = shiftl(1_int64, 63)
   masks = [roots(1)%mask, roots(2)%mask, 0_int64]

   command = build_run_command(run, "fpm run", "fpm run", "", roots, masks, [1], 1)
   if (command /= "fpm run " // quote_arg("first")) error stop "source edit did not select its root"
   command = build_run_command(run, "fpm run", "fpm run", "", roots, masks, [2], 1)
   if (command /= "fpm run " // quote_arg("second")) error stop "highest root bit was not selected"
   command = build_run_command(run, "fpm run", "fpm run", "", roots, masks, [1, 3], 2)
   if (command /= "fpm run") error stop "global metadata narrowed a coalesced run"
   command = build_run_command(run, "fpm run", "fpm run", "", roots, masks, [3, 2], 2)
   if (command /= "fpm run") error stop "global change depended on changed-file order"
   command = build_run_command(tests, "fpm test", "fpm test", "", roots, masks, [1, 3], 2)
   if (command /= "fpm test") error stop "global metadata narrowed coalesced tests"

   deallocate(run%name)
   run%name = [character(len=5) :: "first"]
   command = build_run_command(run, "fpm run first", "fpm run", "first", roots, masks, [2, 3], 2)
   if (command /= "fpm run first") error stop "global metadata changed an explicit target selection"
end program check_exec
