!> Regression checks for fpm graph dependencies and quoted include files.
program graph_check
   use, intrinsic :: iso_fortran_env, only: int64, error_unit
   use watch_fpm_graph, only: compute_watch_files_from_settings
   use watch_types, only: watch_opts_t, root_info_t
   use watch_filter, only: normalize_path
   use fpm_command_line, only: fpm_run_settings
   use fpm_filesystem, only: mkdir, join_path
   use fpm_os, only: get_current_directory, change_directory
   use fpm_strings, only: string_t
   use fpm_error, only: error_t
   use fpm_environment, only: get_env
   implicit none

   type(fpm_run_settings) :: settings
   type(watch_opts_t) :: options
   type(string_t), allocatable :: files(:)
   type(root_info_t), allocatable :: roots(:)
   integer(int64), allocatable :: masks(:)
   type(error_t), allocatable :: error
   character(len=:), allocatable :: initial_directory
   integer(int64) :: manifest_key
   real :: build_secs, model_secs, target_secs, watch_secs
   integer :: include_index, iteration

   call get_current_directory(initial_directory, error)
   call check_error()
   call mkdir('build/watch-graph-tests', echo=.false.)
   call change_directory(join_path(initial_directory, 'build/watch-graph-tests'), error)
   call check_error()
   call create_fixture()

   settings%build_dir = 'build'
   settings%compiler = get_env('FPM_FC', default='gfortran')
   settings%verbose = .false.
   settings%example = .false.
   allocate(settings%name(0))
   options%verbosity = -1

   call compute_graph()
   call expect_file('app/local.inc', .true.)
   call expect_file('include/shared.inc', .true.)
   call expect_file('app/local.h', .true.)
   call expect_file('include/shared.h', .true.)
   call expect_file('deps/local/src/local_dep.f90', .false.)
   call expect_file('deps/local/fpm.toml', .false.)
   if (size(roots) /= 1) error stop 'expected one runnable graph root'
   include_index = find_file('include/shared.inc')
   if (masks(include_index) /= roots(1)%mask) error stop 'include lost its target mask'

   options%watch_deps = .true.
   call compute_graph()
   call expect_file('deps/local/src/local_dep.f90', .true.)
   call expect_file('deps/local/src/local.inc', .true.)
   call expect_file('deps/local/fpm.toml', .true.)
   do iteration = 1, 25
      call compute_graph()
      call expect_file('deps/local/src/local_dep.f90', .true.)
      call expect_file('include/shared.inc', .true.)
   end do

   call write_lines('app/main.f90', [character(len=60) :: &
      'program main', 'use missing_graph_module', 'implicit none', 'end program main'])
   do iteration = 1, 10
      call compute_watch_files_from_settings(settings, options, files, masks, roots, &
         manifest_key, build_secs, model_secs, target_secs, watch_secs, error)
      if (.not. allocated(error)) error stop 'missing module did not return a target error'
   end do
   call write_main(with_feature=.false.)
   call compute_graph()
   call expect_file('deps/local/src/local_dep.f90', .true.)

   settings%features = [string_t('with_dep')]
   call write_main(with_feature=.true.)
   call compute_graph()
   call expect_file('build/featuredep/src/feature_dep.f90', .true.)
   call expect_file('build/featuredep/include/feature.inc', .true.)
   call expect_file('build/featuredep/fpm.toml', .true.)

   options%watch_deps = .false.
   call compute_graph()
   call expect_file('deps/local/src/local_dep.f90', .false.)
   call expect_file('build/featuredep/src/feature_dep.f90', .false.)
   call expect_file('build/featuredep/fpm.toml', .false.)

   deallocate(settings%features)
   settings%profile = 'with_dep_profile'
   options%watch_deps = .true.
   call compute_graph()
   call expect_file('build/featuredep/src/feature_dep.f90', .true.)
   call expect_file('build/featuredep/fpm.toml', .true.)

   call write_lines('fpm.toml', [character(len=20) :: 'name = ['])
   call compute_watch_files_from_settings(settings, options, files, masks, roots, &
      manifest_key, build_secs, model_secs, target_secs, watch_secs, error)
   if (.not. allocated(error)) error stop 'invalid manifest did not return an error'
   call write_manifest()

   call change_directory(initial_directory, error)
   call check_error()
   print '(a)', 'graph regression checks passed'

contains

   !> Build the graph without invoking an external fpm command.
   subroutine compute_graph()
      call compute_watch_files_from_settings(settings, options, files, masks, roots, &
         manifest_key, build_secs, model_secs, target_secs, watch_secs, error)
      call check_error()
   end subroutine compute_graph

   !> Stop with the fpm diagnostic when fixture setup or graph construction fails.
   subroutine check_error()
      if (.not. allocated(error)) return
      write(error_unit, '(a)') error%message
      error stop 1
   end subroutine check_error

   !> Verify that a path is present or excluded as requested.
   subroutine expect_file(path, expected)
      character(len=*), intent(in) :: path
      logical, intent(in) :: expected
      if ((find_file(path) > 0) .eqv. expected) return
      write(error_unit, '(a,l1)') 'unexpected watch membership for ' // path // ': expected ', expected
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
      open(newunit=unit, file=path, status='replace', action='write', iostat=ios)
      if (ios /= 0) error stop 'cannot write graph fixture'
      do i = 1, size(lines)
         write(unit, '(a)') trim(lines(i))
      end do
      close(unit)
   end subroutine write_lines

   !> Create local, C/Fortran include, and feature dependency fixtures.
   subroutine create_fixture()
      call mkdir('app', echo=.false.)
      call mkdir('include', echo=.false.)
      call mkdir('deps/local/src', echo=.false.)
      call mkdir('build/featuredep/src', echo=.false.)
      call mkdir('build/featuredep/include', echo=.false.)
      call write_manifest()
      call write_main(with_feature=.false.)
      call write_lines('app/local.inc', [character(len=60) :: 'integer, parameter :: local_value = 1'])
      call write_lines('include/shared.inc', [character(len=60) :: 'integer, parameter :: shared_value = 2'])
      call write_lines('app/helper.c', [character(len=80) :: &
         '#include "local.h"', '#include "shared.h"', &
         'int helper(void) { return LOCAL_VALUE + SHARED_VALUE; }'])
      call write_lines('app/local.h', [character(len=40) :: '#define LOCAL_VALUE 1'])
      call write_lines('include/shared.h', [character(len=40) :: '#define SHARED_VALUE 2'])
      call write_lines('deps/local/fpm.toml', [character(len=40) :: 'name = "local_dep"'])
      call write_lines('deps/local/src/local_dep.f90', [character(len=60) :: &
         'module local_dep', 'implicit none', "include 'local.inc'", 'end module local_dep'])
      call write_lines('deps/local/src/local.inc', [character(len=60) :: 'integer, parameter :: dep_value = 3'])
      call write_lines('build/featuredep/fpm.toml', [character(len=40) :: 'name = "feature_dep"'])
      call write_lines('build/featuredep/src/feature_dep.f90', [character(len=60) :: &
         'module feature_dep', 'implicit none', "include 'feature.inc'", 'end module feature_dep'])
      call write_lines('build/featuredep/include/feature.inc', &
         [character(len=60) :: 'integer, parameter :: feature_value = 4'])
   end subroutine create_fixture

   !> Write the main manifest with a local dependency and a feature-specific dependency.
   subroutine write_manifest()
      call write_lines('fpm.toml', [character(len=100) :: &
         'name = "graph_fixture"', '[dependencies]', 'local_dep = { path = "deps/local" }', &
         '[features]', 'with_dep.dependencies.feature_dep = { path = "build/featuredep" }', &
         '[profiles]', 'with_dep_profile = ["with_dep"]'])
   end subroutine write_manifest

   !> Select whether the executable uses the optional dependency.
   subroutine write_main(with_feature)
      logical, intent(in) :: with_feature
      character(len=40) :: optional_use
      optional_use = ''
      if (with_feature) optional_use = 'use feature_dep'
      call write_lines('app/main.f90', [character(len=80) :: &
         'program main', 'use local_dep', optional_use, 'implicit none', &
         "include 'local.inc'", "include 'shared.inc'", &
         'print *, local_value + shared_value + dep_value', 'end program main'])
   end subroutine write_main

end program graph_check
