!> Regression checks for fpm graph dependencies and quoted include files.
program graph_check
   use, intrinsic :: iso_fortran_env, only: int64, error_unit
   use watch_fpm_graph, only: compute_watch_files_from_settings
   use watch_types, only: watch_opts_t, root_info_t
   use watch_filter, only: normalize_path, is_ignored_path
   use fpm_command_line, only: fpm_build_settings, fpm_run_settings, fpm_test_settings, get_fpm_env
   use fpm_filesystem, only: mkdir, join_path
   use fpm_os, only: get_current_directory, change_directory
   use fpm_strings, only: string_t
   use fpm_error, only: error_t
   implicit none

   type(fpm_run_settings) :: settings
   type(watch_opts_t) :: options
   type(string_t), allocatable :: files(:)
   type(root_info_t), allocatable :: roots(:)
   integer(int64), allocatable :: masks(:)
   type(error_t), allocatable :: error
   character(len=:), allocatable :: initial_directory
   real :: build_secs, model_secs, target_secs, watch_secs
   integer :: include_index, iteration

   call get_current_directory(initial_directory, error)
   call check_error()
   call mkdir("build/watch-graph-tests", echo=.false.)
   call change_directory(join_path(initial_directory, "build/watch-graph-tests"), error)
   call check_error()
   call create_fixture()

   settings%build_dir = "build"
   settings%compiler = get_fpm_env("FC", "gfortran")
   settings%c_compiler = get_fpm_env("CC", " ")
   settings%cxx_compiler = get_fpm_env("CXX", " ")
   settings%archiver = get_fpm_env("AR", " ")
   settings%verbose = .false.
   settings%example = .false.
   allocate(settings%name(0))
   options%verbosity = -1

   call compute_graph()
   call expect_file("VERSION", .true.)
   call expect_file("deps/local/metadata/release.version", .false.)
   call expect_file("app/local.inc", .true.)
   call expect_file("app/nested.inc", .true.)
   call expect_file("app/nested.h", .true.)
   call expect_file("include/shared.inc", .true.)
   call expect_file("app/local.h", .true.)
   call expect_file("include/shared.h", .true.)
   call expect_file("deps/local/src/local_dep.f90", .false.)
   call expect_file("deps/local/fpm.toml", .false.)
   if (size(roots) /= 1) error stop "expected one runnable graph root"
   include_index = find_file("include/shared.inc")
   if (masks(include_index) /= roots(1)%mask) error stop "include lost its target mask"

   settings%name = [character(len=7) :: "graph*?"]
   call compute_graph()
   call expect_file("app/main.f90", .true.)
   if (size(roots) /= 1) error stop "wildcard target did not select the runnable root"
   deallocate(settings%name)
   allocate(settings%name(0))

   options%include = [string_t("app/*.f90"), string_t("include/*.inc")]
   options%ignore = [string_t("*/shared.inc"), string_t("*VERSION*")]
   call compute_graph()
   call expect_file("app/main.f90", .true.)
   call expect_file("app/helper.c", .false.)
   call expect_file("app/local.inc", .false.)
   call expect_file("include/shared.inc", .false.)
   call expect_file("fpm.toml", .true.)
   call expect_file("VERSION", .true.)
   deallocate(options%include, options%ignore)

   options%watch_deps = .true.
   call compute_graph()
   call expect_file("deps/local/src/local_dep.f90", .true.)
   call expect_file("deps/local/src/local.inc", .true.)
   call expect_file("deps/local/fpm.toml", .true.)
   options%include = [string_t("app/*.f90")]
   options%ignore = [string_t("*VERSION*"), string_t("*/release.version")]
   call compute_graph()
   call expect_file("VERSION", .true.)
   call expect_file("deps/local/metadata/release.version", .true.)
   deallocate(options%include, options%ignore)
   do iteration = 1, 25
      call compute_graph()
      call expect_file("deps/local/src/local_dep.f90", .true.)
      call expect_file("include/shared.inc", .true.)
   end do

   call write_lines("app/main.f90", [character(len=60) :: &
      "program main", "use missing_graph_module", "implicit none", "end program main"])
   do iteration = 1, 10
      call compute_watch_files_from_settings(settings, options, files, masks, roots, &
         build_secs, model_secs, target_secs, watch_secs, error)
      if (.not. allocated(error)) error stop "missing module did not return a target error"
   end do
   call write_main(with_feature=.false.)
   call compute_graph()
   call expect_file("deps/local/src/local_dep.f90", .true.)

   settings%features = [string_t("with_dep")]
   call write_main(with_feature=.true.)
   call compute_graph()
   call expect_file("build/featuredep/src/feature_dep.f90", .true.)
   call expect_file("build/featuredep/include/feature.inc", .true.)
   call expect_file("build/featuredep/fpm.toml", .true.)

   options%watch_deps = .false.
   call compute_graph()
   call expect_file("deps/local/src/local_dep.f90", .false.)
   call expect_file("build/featuredep/src/feature_dep.f90", .false.)
   call expect_file("build/featuredep/fpm.toml", .false.)

   deallocate(settings%features)
   settings%profile = "with_dep_profile"
   options%watch_deps = .true.
   call compute_graph()
   call expect_file("build/featuredep/src/feature_dep.f90", .true.)
   call expect_file("build/featuredep/fpm.toml", .true.)

   call write_lines("fpm.toml", [character(len=20) :: "name = ["])
   call compute_watch_files_from_settings(settings, options, files, masks, roots, &
      build_secs, model_secs, target_secs, watch_secs, error)
   if (.not. allocated(error)) error stop "invalid manifest did not return an error"
   call write_manifest()

   ! CPP model parsing expands INCLUDE paths; collection must recover originals.
   call write_lines("fpm.toml", [character(len=100) :: &
      'name = "graph_fixture"', "[dependencies]", 'local_dep = { path = "deps/local" }', &
      "[preprocess.cpp]"])
   if (allocated(settings%profile)) deallocate(settings%profile)
   call compute_graph()
   call expect_file("app/local.inc", .true.)
   call expect_file("app/nested.inc", .true.)
   call expect_file("include/shared.inc", .true.)
   call write_lines("app/main.f90", [character(len=100) :: &
      "program main", "implicit none", '#include "cpp.inc"', "print *, cpp_value", "end program main"])
   call write_lines("app/cpp.inc", [character(len=80) :: '#include "cpp_nested.inc"'])
   call write_lines("app/cpp_nested.inc", [character(len=80) :: "integer, parameter :: cpp_value = 5"])
   call compute_graph()
   call expect_file("app/cpp.inc", .true.)
   call expect_file("app/cpp_nested.inc", .true.)
   include_index = find_file("app/cpp_nested.inc")
   if (masks(include_index) /= roots(1)%mask) error stop "nested CPP include lost its root mask"

   call write_lines("fpm.toml", [character(len=60) :: 'name = "graph_fixture"', 'version = "0.1.0"'])
   call write_lines("0.1.0", [character(len=20) :: "0.2.0"])
   call compute_graph()
   call expect_file("0.1.0", .false.)

   call check_source_closures()
   call check_link_input_masks()
   call check_shared_includes()
   call check_global_metadata_overlap()
   call check_empty_library_graph()
   call check_empty_source_units()
   call check_build_directory_boundaries()

   call change_directory(initial_directory, error)
   call check_error()
   print "(a)", "graph regression checks passed"

contains

   !> Build the graph without invoking an external fpm command.
   subroutine compute_graph()
      call compute_watch_files_from_settings(settings, options, files, masks, roots, &
         build_secs, model_secs, target_secs, watch_secs, error)
      call check_error()
   end subroutine compute_graph

   !> Stop with the fpm diagnostic when fixture setup or graph construction fails.
   subroutine check_error()
      if (.not. allocated(error)) return
      write(error_unit, "(a)") error%message
      error stop 1
   end subroutine check_error

   !> Verify that a path is present or excluded as requested.
   subroutine expect_file(path, expected)
      character(len=*), intent(in) :: path
      logical, intent(in) :: expected
      if ((find_file(path) > 0) .eqv. expected) return
      write(error_unit, "(a,l1)") "unexpected watch membership for " // path // ": expected ", expected
      error stop 1
   end subroutine expect_file

   !> Find a normalized fixture path in the watch list.
   integer function find_file(path) result(index)
      character(len=*), intent(in) :: path
      integer :: i
      index = 0
      do i = 1, size(files)
         if (normalize_path(files(i)%s) /= path) cycle
         index = i
         return
      end do
   end function find_file

   !> Replace a fixture text file, including when the regression is rerun.
   subroutine write_lines(path, lines)
      character(len=*), intent(in) :: path, lines(:)
      integer :: unit, ios, i
      open(newunit=unit, file=path, status="replace", action="write", iostat=ios)
      if (ios /= 0) error stop "cannot write graph fixture"
      do i = 1, size(lines)
         write(unit, "(a)") trim(lines(i))
      end do
      close(unit)
   end subroutine write_lines

   !> Create local, C/Fortran include, and feature dependency fixtures.
   subroutine create_fixture()
      call mkdir("app", echo=.false.)
      call mkdir("include", echo=.false.)
      call mkdir("deps/local/src", echo=.false.)
      call mkdir("deps/local/metadata", echo=.false.)
      call mkdir("build/featuredep/src", echo=.false.)
      call mkdir("build/featuredep/include", echo=.false.)
      call write_manifest()
      call write_lines("VERSION", [character(len=20) :: "0.1.0"])
      call write_lines("deps/local/metadata/release.version", [character(len=20) :: "0.2.0"])
      call write_main(with_feature=.false.)
      call write_lines("app/local.inc", [character(len=60) :: "include 'nested.inc'"])
      call write_lines("app/nested.inc", [character(len=60) :: "integer, parameter :: local_value = 1"])
      call write_lines("include/shared.inc", [character(len=60) :: "integer, parameter :: shared_value = 2"])
      call write_lines("app/helper.c", [character(len=80) :: &
         '#include "local.h"', '#include "shared.h"', &
         "int helper(void) { return LOCAL_VALUE + SHARED_VALUE; }"])
      call write_lines("app/local.h", [character(len=60) :: &
         "#ifndef LOCAL_H", "#define LOCAL_H", '#include "nested.h"', "#define LOCAL_VALUE 1", "#endif"])
      call write_lines("app/nested.h", [character(len=60) :: '#include "local.h"'])
      call write_lines("include/shared.h", [character(len=40) :: "#define SHARED_VALUE 2"])
      call write_lines("deps/local/fpm.toml", [character(len=60) :: &
         'name = "local_dep"', 'version = "metadata/release.version"'])
      call write_lines("deps/local/src/local_dep.f90", [character(len=60) :: &
         "module local_dep", "implicit none", "include 'local.inc'", "end module local_dep"])
      call write_lines("deps/local/src/local.inc", [character(len=60) :: "integer, parameter :: dep_value = 3"])
      call write_lines("build/featuredep/fpm.toml", [character(len=40) :: 'name = "feature_dep"'])
      call write_lines("build/featuredep/src/feature_dep.f90", [character(len=60) :: &
         "module feature_dep", "implicit none", "include 'feature.inc'", "end module feature_dep"])
      call write_lines("build/featuredep/include/feature.inc", &
         [character(len=60) :: "integer, parameter :: feature_value = 4"])
   end subroutine create_fixture

   !> Validate owned source graphs, link inputs, submodules, and sibling isolation.
   subroutine check_source_closures()
      type(fpm_build_settings) :: build_settings
      type(fpm_test_settings) :: test_settings
      integer :: unit

      call mkdir("closure/app/first", echo=.false.)
      call mkdir("closure/app/second", echo=.false.)
      call mkdir("closure/src", echo=.false.)
      call mkdir("closure/include", echo=.false.)
      call change_directory(join_path(initial_directory, "build/watch-graph-tests/closure"), error)
      call check_error()
      call write_lines("fpm.toml", [character(len=60) :: 'name = "source_closure"'])
      call write_lines("src/parent.f90", [character(len=80) :: &
         "module parent_module", "implicit none", "interface", "module integer function value()", &
         "end function value", "module integer function nested_value()", "end function nested_value", &
         "end interface", "end module parent_module"])
      call write_lines("src/child.f90", [character(len=80) :: &
         "submodule(parent_module) child_module", "implicit none", "contains", &
         "module procedure value", "value = 9", "end procedure value", "end submodule child_module"])
      call write_lines("src/grandchild.f90", [character(len=80) :: &
         "submodule(parent_module:child_module) grandchild_module", "implicit none", "contains", &
         "module procedure nested_value", "nested_value = 11", "end procedure nested_value", &
         "end submodule grandchild_module"])
      call write_lines("src/unused.f90", [character(len=80) :: &
         "module unused_module", "implicit none", "end module unused_module"])
      call write_lines("src/global.c", [character(len=80) :: "int global_helper(void) { return 1; }"])
      call write_lines("app/first/first.f90", [character(len=80) :: &
         "program first", "use parent_module", "implicit none", "include 'config.inc'", &
         "include 'settings.inc'", "include 'future.inc'", "print *, value()", "end program first"])
      call write_lines("app/first/config.inc", [character(len=1) :: ])
      call write_lines("include/settings.inc", [character(len=1) :: ])
      call write_lines("app/second/second.f90", [character(len=80) :: &
         "program second", "implicit none", "print *, 2", "end program second"])
      call write_lines("app/first/helper.c", [character(len=80) :: "int first_helper(void) { return 1; }"])
      call write_lines("app/second/helper.c", [character(len=80) :: "int second_helper(void) { return 2; }"])
      settings%name = [character(len=6) :: "first"]
      call compute_graph()
      call expect_file("src/parent.f90", .true.)
      call expect_file("src/child.f90", .true.)
      call expect_file("src/grandchild.f90", .true.)
      call expect_file("src/global.c", .true.)
      call expect_file("src/unused.f90", .false.)
      call expect_file("app/first/helper.c", .true.)
      call expect_file("app/second/helper.c", .false.)
      call expect_file("app/second/second.f90", .false.)
      call expect_file("app/first/config.inc", .true.)
      call expect_file("include/settings.inc", .true.)
      call expect_file("app/first/future.inc", .true.)
      open(newunit=unit, file="app/first/config.inc", status="old")
      close(unit, status="delete")
      open(newunit=unit, file="include/settings.inc", status="old")
      close(unit, status="delete")
      call compute_graph()
      call expect_file("app/first/config.inc", .true.)
      call expect_file("include/settings.inc", .true.)
      include_index = find_file("include/settings.inc")
      if (masks(include_index) /= roots(1)%mask) error stop "missing include lost its root mask"
      call write_lines("app/first/config.inc", [character(len=1) :: ])
      call write_lines("include/settings.inc", [character(len=1) :: ])
      call write_lines("app/first/future.inc", [character(len=1) :: ])
      call compute_graph()
      call expect_file("app/first/config.inc", .true.)
      call expect_file("include/settings.inc", .true.)
      call expect_file("app/first/future.inc", .true.)
      do iteration = 1, 100
         call compute_graph()
         call expect_file("src/unused.f90", .false.)
      end do
      settings%prune = .false.
      call compute_graph()
      call expect_file("src/unused.f90", .true.)
      settings%prune = .true.
      settings%name = [character(len=6) :: "second"]
      call compute_graph()
      call expect_file("src/parent.f90", .false.)
      call expect_file("src/child.f90", .false.)
      call expect_file("src/grandchild.f90", .false.)
      call expect_file("src/global.c", .true.)
      call expect_file("app/first/first.f90", .false.)
      call expect_file("app/first/helper.c", .false.)
      call expect_file("app/second/helper.c", .true.)

      build_settings = settings%fpm_build_settings
      call compute_watch_files_from_settings(build_settings, options, files, masks, roots, &
         build_secs, model_secs, target_secs, watch_secs, error)
      call check_error()
      call expect_file("src/unused.f90", .true.)
      if (size(roots) /= 0) error stop "build mode unexpectedly maps runnable roots"

      call mkdir("test/first", echo=.false.)
      call mkdir("test/second", echo=.false.)
      call write_lines("test/first/first_test.f90", [character(len=80) :: &
         "program first_test", "use parent_module", "implicit none", "print *, value()", "end program first_test"])
      call write_lines("test/second/second_test.f90", [character(len=80) :: &
         "program second_test", "implicit none", "print *, 2", "end program second_test"])
      test_settings%fpm_run_settings%fpm_build_settings = build_settings
      test_settings%build_tests = .true.
      test_settings%name = [character(len=10) :: "first_test"]
      call compute_watch_files_from_settings(test_settings, options, files, masks, roots, &
         build_secs, model_secs, target_secs, watch_secs, error)
      call check_error()
      call expect_file("test/first/first_test.f90", .true.)
      call expect_file("test/second/second_test.f90", .false.)
      call expect_file("src/child.f90", .true.)
      call expect_file("src/grandchild.f90", .true.)
      if (size(roots) /= 1) error stop "named test selection did not restrict roots"
   end subroutine check_source_closures

   !> Retain shared link-input closures across all root bits and the unmapped fallback.
   subroutine check_link_input_masks()
      character(len=:), allocatable :: fixture
      character(len=2) :: number
      character(len=80) :: program_lines(3)
      integer :: i

      fixture = join_path(initial_directory, "build/watch-graph-tests/link-inputs")
      call mkdir(join_path(fixture, "src"), echo=.false.)
      call mkdir(join_path(fixture, "app"), echo=.false.)
      call change_directory(fixture, error)
      call check_error()
      call write_lines("fpm.toml", [character(len=60) :: 'name = "link_input_graph"'])
      call write_lines("src/common.f90", [character(len=80) :: &
         "module common_module", "implicit none", "end module common_module"])
      call write_lines("src/external.f90", [character(len=80) :: &
         "subroutine external_helper", "use common_module", "implicit none", "end subroutine external_helper"])
      call write_lines("src/global.c", [character(len=80) :: "int global_helper(void) { return 1; }"])
      call write_lines("src/global.cpp", [character(len=80) :: "int cpp_helper() { return 2; }"])
      call write_lines("src/unused.f90", [character(len=80) :: &
         "module unused_module", "implicit none", "end module unused_module"])
      do i = 1, 65
         write(number, "(i2.2)") i
         program_lines(1) = "program root" // number
         program_lines(2) = "implicit none"
         program_lines(3) = "end program root" // number
         call write_lines("app/root" // number // ".f90", program_lines)
      end do
      if (allocated(settings%name)) deallocate(settings%name)
      allocate(settings%name(64))
      do i = 1, 64
         write(number, "(i2.2)") i
         settings%name(i) = "root" // number
      end do
      call compute_graph()
      if (size(roots) /= 64) error stop "link-input graph did not retain 64 selected roots"
      call expect_mask("src/common.f90", not(0_int64))
      call expect_mask("src/external.f90", not(0_int64))
      call expect_mask("src/global.c", not(0_int64))
      call expect_mask("src/global.cpp", not(0_int64))
      call expect_file("src/unused.f90", .false.)
      call expect_file("app/root65.f90", .false.)

      deallocate(settings%name)
      allocate(settings%name(0))
      call compute_graph()
      if (size(roots) /= 0) error stop "more than 64 link-input roots unexpectedly produced masks"
      if (any(masks /= 0_int64)) error stop "unmapped link-input graph returned a nonzero mask"
      call expect_file("src/common.f90", .true.)
      call expect_file("src/external.f90", .true.)
      call expect_file("src/global.c", .true.)
      call expect_file("src/global.cpp", .true.)
      call expect_file("src/unused.f90", .false.)
      call expect_file("app/root65.f90", .true.)

      settings%name = [character(len=6) :: "root01"]
      settings%prune = .false.
      call compute_graph()
      call expect_mask("src/unused.f90", roots(1)%mask)
      settings%prune = .true.
      call write_lines("fpm.toml", [character(len=60) :: &
         'name = "link_input_graph"', "[library]", 'type = ["static"]'])
      call compute_graph()
      call expect_mask("src/unused.f90", roots(1)%mask)
   end subroutine check_link_input_masks

   !> Keep all 64 root bits across cached nested includes, cycles, and storage growth.
   subroutine check_shared_includes()
      type(string_t), allocatable :: metadata_files(:)
      integer(int64) :: all_roots
      integer :: i, unit, source_index, private_index
      character(len=2) :: number
      character(len=80) :: program_lines(5)
      character(len=:), allocatable :: source_file, private_file, fixture

      fixture = join_path(initial_directory, "build/watch-graph-tests/shared-includes")
      call mkdir(join_path(fixture, "app"), echo=.false.)
      call mkdir(join_path(fixture, "include"), echo=.false.)
      call change_directory(fixture, error)
      call check_error()
      call write_lines("fpm.toml", [character(len=60) :: 'name = "shared_include_graph"'])
      if (allocated(settings%name)) deallocate(settings%name)
      allocate(settings%name(0))

      do i = 1, 64
         write(number, "(i2.2)") i
         source_file = "app/root" // number // ".f90"
         if (i == 1) source_file = "app/main.f90"
         private_file = "private" // number // ".inc"
         program_lines(1) = "program root" // number
         program_lines(2) = "implicit none"
         program_lines(3) = "include 'shared.inc'"
         program_lines(4) = "include '" // private_file // "'"
         program_lines(5) = "end program root" // number
         call write_lines(source_file, program_lines)
         call write_lines("app/" // private_file, [character(len=1) :: ])
      end do
      call write_lines("include/shared.inc", [character(len=60) :: "include 'nested.inc'"])
      call write_lines("include/nested.inc", [character(len=60) :: '#include "cycle.h"'])
      call write_lines("include/cycle.h", [character(len=60) :: '#include "shared.inc"'])

      call compute_graph()
      if (size(roots) /= 64) error stop "shared include graph did not retain all 64 roots"
      all_roots = 0_int64
      do i = 1, size(roots)
         all_roots = ior(all_roots, roots(i)%mask)
      end do
      if (popcnt(all_roots) /= 64) error stop "shared include graph lost the high root bit"
      call expect_mask("app/shared.inc", all_roots)
      call expect_mask("include/shared.inc", all_roots)
      call expect_mask("include/nested.inc", all_roots)
      call expect_mask("include/cycle.h", all_roots)
      do i = 1, 64
         write(number, "(i2.2)") i
         source_file = "app/root" // number // ".f90"
         if (i == 1) source_file = "app/main.f90"
         source_index = find_file(source_file)
         private_index = find_file("app/private" // number // ".inc")
         if (source_index == 0 .or. private_index == 0) error stop "private include disappeared during cache growth"
         if (popcnt(masks(source_index)) /= 1) error stop "source root mask was broadened by shared includes"
         if (masks(private_index) /= masks(source_index)) error stop "private include acquired another root's mask"
      end do

      ! A new graph must resolve a newly created earlier candidate instead of reusing old edges.
      call write_lines("app/shared.inc", [character(len=60) :: "include 'shadow.inc'"])
      call write_lines("app/shadow.inc", [character(len=1) :: ])
      call compute_graph()
      call expect_mask("app/shared.inc", all_roots)
      call expect_mask("app/shadow.inc", all_roots)
      call expect_file("include/shared.inc", .false.)
      call expect_file("include/nested.inc", .false.)
      open(newunit=unit, file="app/shared.inc", status="old")
      close(unit, status="delete")
      open(newunit=unit, file="app/shadow.inc", status="old")
      close(unit, status="delete")
      call compute_graph()
      call expect_mask("include/shared.inc", all_roots)
      call expect_mask("include/cycle.h", all_roots)

      ! Additional metadata survives filters, build exclusion, and absent lookup candidates.
      call write_lines("build/args.rsp", [character(len=60) :: "run"])
      metadata_files = [string_t("build/args.rsp"), string_t("build/future.rsp"), &
         string_t("include/shared.inc"), string_t("build/args.rsp")]
      options%include = [string_t("app/*.f90")]
      options%ignore = [string_t("*.rsp"), string_t("*shared.inc")]
      call compute_watch_files_from_settings(settings, options, files, masks, roots, &
         build_secs, model_secs, target_secs, watch_secs, error, metadata_files)
      call check_error()
      call expect_mask("build/args.rsp", 0_int64)
      call expect_mask("build/future.rsp", 0_int64)
      call expect_mask("include/shared.inc", 0_int64)
      call expect_file("include/nested.inc", .false.)
      if (count([(normalize_path(files(i)%s) == "build/args.rsp", i=1,size(files))]) /= 1) then
         error stop "duplicate metadata path was not merged"
      end if
      deallocate(options%include, options%ignore)
   end subroutine check_shared_includes

   !> Global version/response metadata dominates include membership without losing descendant bits.
   subroutine check_global_metadata_overlap()
      type(string_t), allocatable :: metadata_files(:)
      character(len=:), allocatable :: fixture
      integer(int64) :: all_roots
      integer :: i, first_index, second_index

      fixture = join_path(initial_directory, "build/watch-graph-tests/metadata-overlap")
      call mkdir(join_path(fixture, "app"), echo=.false.)
      call change_directory(fixture, error)
      call check_error()
      call write_lines("fpm.toml", [character(len=80) :: &
         'name = "metadata_overlap_graph"', 'version = "VERSION"', "[preprocess.cpp]"])
      call write_lines("VERSION", [character(len=60) :: "0.1.0", '#include "nested.inc"'])
      call write_lines("nested.inc", [character(len=60) :: "integer, parameter :: nested_value = 1"])
      call write_lines("options.rsp", [character(len=60) :: "options run"])
      call write_lines("app/main.f90", [character(len=80) :: &
         "program main", "implicit none", "#if 0", '#include "../VERSION"', &
         '#include "../options.rsp"', "#endif", "print *, 1", "end program main"])
      call write_lines("app/second.f90", [character(len=80) :: &
         "program second", "implicit none", "#if 0", '#include "../VERSION"', &
         "#endif", "print *, 2", "end program second"])
      if (allocated(settings%name)) deallocate(settings%name)
      allocate(settings%name(0))
      metadata_files = [string_t("options.rsp"), string_t("options.rsp")]
      call compute_watch_files_from_settings(settings, options, files, masks, roots, &
         build_secs, model_secs, target_secs, watch_secs, error, metadata_files)
      call check_error()
      if (size(roots) /= 2) error stop "metadata overlap fixture did not retain both roots"
      all_roots = 0_int64
      do i = 1, size(roots)
         all_roots = ior(all_roots, roots(i)%mask)
      end do
      call expect_mask("VERSION", 0_int64)
      call expect_mask("options.rsp", 0_int64)
      call expect_mask("nested.inc", all_roots)
      first_index = find_file("app/main.f90")
      second_index = find_file("app/second.f90")
      if (popcnt(masks(first_index)) /= 1 .or. popcnt(masks(second_index)) /= 1) then
         error stop "global metadata broadened program membership"
      end if
      if (count([(normalize_path(files(i)%s) == "options.rsp", i=1,size(files))]) /= 1) then
         error stop "duplicate response metadata path was not merged"
      end if

      options%include = [string_t("app/*.f90")]
      options%ignore = [string_t("*VERSION*"), string_t("*.rsp"), string_t("*.inc")]
      call compute_watch_files_from_settings(settings, options, files, masks, roots, &
         build_secs, model_secs, target_secs, watch_secs, error, metadata_files)
      call check_error()
      call expect_mask("VERSION", 0_int64)
      call expect_mask("options.rsp", 0_int64)
      call expect_file("nested.inc", .false.)
      deallocate(options%include, options%ignore)
   end subroutine check_global_metadata_overlap

   !> Rebuild a two-program graph whose default library directory contains no sources.
   subroutine check_empty_library_graph()
      type(fpm_run_settings) :: empty_settings
      type(watch_opts_t) :: empty_options
      character(len=:), allocatable :: fixture
      integer :: pass, first_index, second_index

      fixture = join_path(initial_directory, "build/watch-graph-tests/empty-library")
      call mkdir(join_path(fixture, "src"), echo=.false.)
      call mkdir(join_path(fixture, "app"), echo=.false.)
      call change_directory(fixture, error)
      call check_error()
      call write_lines("fpm.toml", [character(len=60) :: 'name = "empty_library_graph"', 'version = "0.1.0"'])
      call write_lines("app/first.f90", [character(len=60) :: &
         "program first", "implicit none", "print *, 1", "end program first"])
      call write_lines("app/second.f90", [character(len=60) :: &
         "program second", "implicit none", "print *, 2", "end program second"])
      empty_settings%fpm_build_settings = settings%fpm_build_settings
      empty_settings%example = .false.
      allocate(empty_settings%name(0))
      empty_options%verbosity = -1

      do pass = 1, 2
         call compute_watch_files_from_settings(empty_settings, empty_options, files, masks, roots, &
            build_secs, model_secs, target_secs, watch_secs, error)
         call check_error()
         call expect_file("app/first.f90", .true.)
         call expect_file("app/second.f90", .true.)
         call expect_mask("fpm.toml", 0_int64)
         if (size(roots) /= 2) error stop "empty-library graph did not retain both programs"
         first_index = find_file("app/first.f90")
         second_index = find_file("app/second.f90")
         if (popcnt(masks(first_index)) /= 1 .or. popcnt(masks(second_index)) /= 1) then
            error stop "empty-library graph lost program root membership"
         end if
         if (iand(masks(first_index), masks(second_index)) /= 0_int64) then
            error stop "independent empty-library programs share a root mask"
         end if
      end do
   end subroutine check_empty_library_graph

   !> Empty model sources release their units without closing caller-owned files.
   subroutine check_empty_source_units()
      type(fpm_build_settings) :: empty_settings
      type(watch_opts_t) :: empty_options
      character(len=:), allocatable :: fixture
      character(len=32), parameter :: empty_paths(3) = [character(len=32) :: &
         "src/empty.f90", "src/empty.c", "deps/empty/src/empty.f90"]
      integer :: pass, path, caller_unit
      logical :: opened

      fixture = join_path(initial_directory, "build/watch-graph-tests/empty-sources")
      call mkdir(join_path(fixture, "src"), echo=.false.)
      call mkdir(join_path(fixture, "app"), echo=.false.)
      call mkdir(join_path(fixture, "deps/empty/src"), echo=.false.)
      call change_directory(fixture, error)
      call check_error()
      call write_lines("fpm.toml", [character(len=80) :: &
         'name = "empty_source_graph"', "[dependencies]", 'empty_dep = { path = "deps/empty" }'])
      call write_lines("deps/empty/fpm.toml", [character(len=60) :: 'name = "empty_dep"'])
      call write_lines("app/main.f90", [character(len=60) :: "program main", "end program main"])
      do path = 1, size(empty_paths)
         call write_lines(trim(empty_paths(path)), [character(len=1) :: ])
      end do

      empty_settings = settings%fpm_build_settings
      empty_options%verbosity = -1
      open(newunit=caller_unit, file="caller-owned.txt", status="replace", action="readwrite")
      do pass = 1, 20
         call compute_watch_files_from_settings(empty_settings, empty_options, files, masks, roots, &
            build_secs, model_secs, target_secs, watch_secs, error)
         call check_error()
         call expect_file("src/empty.f90", .true.)
         call expect_file("src/empty.c", .true.)
         call expect_file("deps/empty/src/empty.f90", .false.)
         do path = 1, size(empty_paths)
            inquire(file=trim(empty_paths(path)), opened=opened)
            if (opened) error stop "empty model source left a file unit open"
         end do
         inquire(unit=caller_unit, opened=opened)
         if (.not. opened) error stop "graph construction closed a caller-owned file"
      end do
      close(caller_unit, status="delete")
   end subroutine check_empty_source_units

   !> Exclude the configured build tree without dropping same-named source directories.
   subroutine check_build_directory_boundaries()
      type(fpm_build_settings) :: build_settings
      type(watch_opts_t) :: build_options
      character(len=:), allocatable :: fixture
      integer :: pass, dependencies

      fixture = join_path(initial_directory, "build/watch-graph-tests/build-boundaries")
      call mkdir(join_path(fixture, "src/build"), echo=.false.)
      call mkdir(join_path(fixture, "include/build"), echo=.false.)
      call mkdir(join_path(fixture, "build/localdep/src"), echo=.false.)
      call mkdir(join_path(fixture, "build-sibling"), echo=.false.)
      call mkdir(join_path(fixture, "scratch"), echo=.false.)
      call change_directory(fixture, error)
      call check_error()
      call write_lines("fpm.toml", [character(len=100) :: &
         'name = "build_boundaries_graph"', "[dependencies]", 'localdep = { path = "build/localdep" }'])
      call write_lines("src/build/nested.f90", [character(len=60) :: &
         "module nested_build_source", "end module nested_build_source"])
      call write_lines("src/root.f90", [character(len=512) :: &
         "module build_boundaries_root", "include '../build/relative.inc'", &
         "include '" // join_path(fixture, "build/absolute.inc") // "'", &
         "include '../build-sibling/keep.inc'", "include '../include/build/keep.inc'", &
         "include '" // join_path(fixture, "include/build/absolute.inc") // "'", &
         "end module build_boundaries_root"])
      call write_lines("build/relative.inc", [character(len=60) :: "integer :: generated_relative"])
      call write_lines("build/absolute.inc", [character(len=60) :: "integer :: generated_absolute"])
      call write_lines("build-sibling/keep.inc", [character(len=60) :: "integer :: sibling_value"])
      call write_lines("include/build/keep.inc", [character(len=60) :: "integer :: nested_value"])
      call write_lines("include/build/absolute.inc", [character(len=60) :: "integer :: absolute_value"])
      call write_lines("build/localdep/fpm.toml", [character(len=60) :: 'name = "localdep"'])
      call write_lines("build/localdep/src/local.f90", [character(len=60) :: &
         "module localdep", "end module localdep"])
      build_settings = settings%fpm_build_settings
      build_options%verbosity = -1

      do pass = 1, 4
         select case (pass)
          case (1)
            build_settings%build_dir = "build"
          case (2)
            build_settings%build_dir = join_path(fixture, "build")
          case (3)
            build_settings%build_dir = "./scratch/../build/"
          case default
            build_settings%build_dir = join_path(fixture, "scratch/../build/")
         end select
         do dependencies = 0, 1
            build_options%watch_deps = dependencies == 1
            call compute_watch_files_from_settings(build_settings, build_options, files, masks, roots, &
               build_secs, model_secs, target_secs, watch_secs, error)
            call check_error()
            call expect_file("src/build/nested.f90", .true.)
            call expect_file("src/root.f90", .true.)
            call expect_file("build-sibling/keep.inc", .true.)
            call expect_file("include/build/keep.inc", .true.)
            call expect_file(join_path(fixture, "include/build/absolute.inc"), .true.)
            call expect_file("build/relative.inc", .false.)
            call expect_file(join_path(fixture, "build/absolute.inc"), .false.)
            call expect_file("build/localdep/src/local.f90", build_options%watch_deps)
            call expect_file("build/localdep/fpm.toml", build_options%watch_deps)
            call expect_mask("fpm.toml", 0_int64)
         end do
      end do

      if (.not. is_ignored_path("C:\Project\build\generated.inc", "c:/project/BUILD", windows=.true.)) then
         error stop "Windows build directory comparison lost separator or case equivalence"
      end if
      if (is_ignored_path("C:\Project\src\build\keep.f90", "C:/Project/build", windows=.true.)) then
         error stop "Windows build directory comparison excluded a nested source directory"
      end if
      if (is_ignored_path("/project/BUILD/source.f90", "/project/build")) then
         error stop "case-sensitive build directory comparison folded source paths"
      end if
   end subroutine check_build_directory_boundaries

   !> Verify that a watched path retains exactly the expected root membership.
   subroutine expect_mask(path, expected)
      character(len=*), intent(in) :: path
      integer(int64), intent(in) :: expected
      integer :: index
      call expect_file(path, .true.)
      index = find_file(path)
      if (masks(index) /= expected) then
         write(error_unit, "(a)") "unexpected root mask for " // path
         error stop 1
      end if
   end subroutine expect_mask

   !> Write the main manifest with a local dependency and a feature-specific dependency.
   subroutine write_manifest()
      call write_lines("fpm.toml", [character(len=100) :: &
         'name = "graph_fixture"', 'version = "VERSION"', "[dependencies]", 'local_dep = { path = "deps/local" }', &
         "[features]", 'with_dep.dependencies.feature_dep = { path = "build/featuredep" }', &
         "[profiles]", 'with_dep_profile = ["with_dep"]'])
   end subroutine write_manifest

   !> Select whether the executable uses the optional dependency.
   subroutine write_main(with_feature)
      logical, intent(in) :: with_feature
      character(len=40) :: optional_use
      optional_use = ""
      if (with_feature) optional_use = "use feature_dep"
      call write_lines("app/main.f90", [character(len=80) :: &
         "program main", "use local_dep", optional_use, "implicit none", &
         "include 'local.inc'", "include 'shared.inc'", &
         "print *, local_value + shared_value + dep_value", "end program main"])
   end subroutine write_main

end program graph_check
