!> Compute watch lists from the `fpm` dependency graph.
!!
!! Builds the `fpm` model and expands targets into a set of source files to
!! watch. Optional dependency watching can include dependency project trees
!! (including those located under the build directory).
!!
!! This module is responsible for connecting `fpm-watch` to the `fpm` internal
!! model/target structures. It selects the relevant closure of targets for the
!! configured command (build/test/run), and produces:
!! - `files`: unique normalized file paths
!! - `file_mask`: bitmasks mapping each file to root targets
!! - `roots`: the root targets corresponding to runnable/testable executables
module watch_fpm_graph
   use, intrinsic :: iso_fortran_env, only: int64, error_unit
   use watch_types, only: watch_opts_t, root_info_t
   use watch_log, only: log_info
   use watch_filter, only: normalize_path, is_ignored_path, filter_watch_files, starts_with
   use watch_util, only: ftoa, trim_or_empty
   use fpm,              only: build_model
   use fpm_manifest,     only: package_config_t, get_package_data
   use fpm_model,        only: fpm_model_t, FPM_SCOPE_APP, FPM_SCOPE_TEST, FPM_SCOPE_EXAMPLE
   use fpm_targets,      only: build_target_ptr, targets_from_sources
   use fpm_filesystem,   only: basename, exists, join_path
   use fpm_strings,      only: string_t, glob, str
   use fpm_error,        only: error_t
   use fpm_dependency,   only: dependency_tree_t, new_dependency_tree
   use fpm_command_line, only: fpm_build_settings, fpm_run_settings, fpm_test_settings
   implicit none
   private
   public compute_watch_files_from_settings

contains

   !> Compute watched files and masks from current `fpm` settings.
   !!
   !! This is the primary "graph to watchlist" entry point.
   !!
   !! ### Outputs
   !! - `files`: normalized, unique file paths to watch.
   !! - `file_mask`: a bitmask per file that encodes which root target(s) the
   !!   file belongs to (used for name injection on run/test).
   !! - `roots`: root executable/test targets. `roots(r)%mask` is a 1-bit mask.
   !! - `manifest_key`: placeholder output (computed elsewhere); set to `0` here.
   !! - `build_secs`: sum of `secs_model + secs_targets + secs_watch`.
   !! - `secs_model`: time spent building the `fpm` model.
   !! - `secs_targets`: time spent expanding sources into targets.
   !! - `secs_watch`: time spent collecting and filtering file paths.
   !!
   !! ### Notes
   !! - Root bitmasks are limited to `bit_size(0_int64)` (typically 64 roots).
   !! - Build directory paths are excluded from the watch list unless dependency
   !!   watching is enabled and the path is identified as belonging to a dep.
   subroutine compute_watch_files_from_settings(settings, w, files, file_mask, roots, manifest_key, build_secs, secs_model, secs_targets, secs_watch)
      class(fpm_build_settings), intent(inout) :: settings
      type(watch_opts_t), intent(in) :: w
      type(string_t), allocatable, intent(out) :: files(:)
      integer(int64), allocatable, intent(out) :: file_mask(:)
      type(root_info_t), allocatable, intent(out) :: roots(:)
      integer(int64), intent(out) :: manifest_key
      real, intent(out) :: build_secs
      real, intent(out) :: secs_model, secs_targets, secs_watch

      type(package_config_t), allocatable :: package
      type(fpm_model_t),       allocatable :: model
      type(build_target_ptr),  allocatable :: targets(:)
      type(error_t), allocatable :: err
      type(dependency_tree_t) :: tree

      logical, allocatable :: keep(:)
      logical, allocatable :: visited(:)
      integer(int64), allocatable :: target_mask(:)

      type(string_t), allocatable :: dep_dirs(:)

      integer :: i, r, nroots
      integer(int64) :: rate, t0, t1, tw0, tw1
      integer :: n_before, n_after

      secs_model   = 0.0
      secs_targets = 0.0
      secs_watch   = 0.0
      build_secs   = 0.0

      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64

      allocate(package, model)

      call get_package_data(package, "fpm.toml", err, apply_defaults=.true.)
      if (allocated(err)) then
         write(error_unit,'(a)') "fpm-watch: manifest error: " // err%message
         stop 1
      end if

      call new_dependency_tree(tree, build_dir=trim_or_empty(settings%build_dir), path_to_config=trim_or_empty(settings%path_to_config))

      call tree%add(package, err)
      if (allocated(err)) then
         write(error_unit,'(a)') "fpm-watch: dependency error: " // err%message
         stop 1
      end if

      if (w%watch_deps) then
         call collect_dep_dirs(tree, dep_dirs)
         if (w%verbosity >= 1) then
            call log_info(w, "deps: watching dependency sources (deps=" // str(size(dep_dirs)) // ")")
         end if
      else
         if (allocated(dep_dirs)) deallocate(dep_dirs)
         allocate(dep_dirs(0))
      end if

      call system_clock(t0)
      call build_model(model, settings, package, err)
      call system_clock(t1)
      secs_model = real(t1 - t0) / real(rate)

      if (allocated(err)) then
         write(error_unit,'(a)') "fpm-watch: model error: " // err%message
         stop 1
      end if

      call system_clock(t0)
      call targets_from_sources(targets, model, settings%prune, package%library, err)
      call system_clock(t1)
      secs_targets = real(t1 - t0) / real(rate)

      if (allocated(err)) then
         write(error_unit,'(a)') "fpm-watch: targets error: " // err%message
         stop 1
      end if

      call system_clock(tw0)

      allocate(keep(size(targets)))
      keep = .false.

      select type (s => settings)
       type is (fpm_test_settings)
         do i = 1, size(targets)
            if (targets(i)%ptr%is_executable_target(FPM_SCOPE_TEST)) keep(i) = .true.
         end do
       type is (fpm_run_settings)
         if (s%example) then
            call select_names_or_all(targets, FPM_SCOPE_EXAMPLE, s%name, keep)
         else
            call select_names_or_all(targets, FPM_SCOPE_APP, s%name, keep)
         end if
       class default
         keep = .true.
      end select

      nroots = count(keep)

      allocate(visited(size(targets)))
      visited = .false.

      if (is_run_or_test(settings) .and. nroots > 0 .and. nroots <= bit_size(0_int64)) then
         allocate(roots(nroots))
         allocate(target_mask(size(targets)))
         target_mask = 0_int64

         r = 0
         do i = 1, size(targets)
            if (.not. keep(i)) cycle
            r = r + 1
            roots(r)%name = basename(targets(i)%ptr%output_file, suffix=.false.)
            roots(r)%mask = shiftl(1_int64, r-1)
            call dfs_mark_mask(targets(i)%ptr, targets, target_mask, roots(r)%mask)
         end do

         visited = (target_mask /= 0_int64)
      else
         if (allocated(roots)) deallocate(roots)
         allocate(roots(0))
         if (allocated(target_mask)) deallocate(target_mask)
         if (all(keep)) then
            visited = .true.
         else
            do i = 1, size(targets)
               if (keep(i)) call dfs_mark_bool(targets(i)%ptr, targets, visited)
            end do
         end if
      end if

      if (w%watch_deps) then
         call gather_files_with_mask(targets, visited, target_mask, files, file_mask, trim_or_empty(settings%build_dir), dep_dirs)
      else
         call gather_files_with_mask(targets, visited, target_mask, files, file_mask, trim_or_empty(settings%build_dir))
      end if

      if (w%watch_deps) then
         call push_file_with_mask(files, file_mask, "fpm.toml", trim_or_empty(settings%build_dir), 0_int64, dep_dirs)
      else
         call push_file_with_mask(files, file_mask, "fpm.toml", trim_or_empty(settings%build_dir), 0_int64)
      end if

      do i = 1, tree%ndep
         if (.not. allocated(tree%dep(i)%proj_dir)) cycle
         if (len_trim(tree%dep(i)%proj_dir) == 0) cycle

         if (w%watch_deps) then
            call push_file_with_mask(files, file_mask, join_path(tree%dep(i)%proj_dir, "fpm.toml"), trim_or_empty(settings%build_dir), 0_int64, dep_dirs)
         else
            call push_file_with_mask(files, file_mask, join_path(tree%dep(i)%proj_dir, "fpm.toml"), trim_or_empty(settings%build_dir), 0_int64)
         end if
      end do

      n_before = 0
      if (allocated(files)) n_before = size(files)

      call filter_watch_files(files, w%include, w%ignore, file_mask)

      n_after = 0
      if (allocated(files)) n_after = size(files)

      if (w%debug) then
         call log_info(w, "debug: watch files filtered: " // str(n_before) // " -> " // str(n_after))
      end if

      call system_clock(tw1)
      secs_watch = real(tw1 - tw0) / real(rate)

      build_secs = secs_model + secs_targets + secs_watch
      manifest_key = 0_int64

      if (w%verbosity >= 1) then
         call log_info(w, "timings: model=" // ftoa(secs_model) // "s targets=" // ftoa(secs_targets) // "s watchlist=" // ftoa(secs_watch) // "s")
         call log_info(w, "targets: total=" // str(size(targets)) // " closure=" // str(count(visited)))
      end if

      deallocate(package, model)
      if (allocated(targets)) deallocate(targets)
      if (allocated(keep)) deallocate(keep)
      if (allocated(visited)) deallocate(visited)
      if (allocated(target_mask)) deallocate(target_mask)
      if (allocated(dep_dirs)) deallocate(dep_dirs)

   contains

      subroutine collect_dep_dirs(tree, dep_dirs)
         type(dependency_tree_t), intent(in) :: tree
         type(string_t), allocatable, intent(out) :: dep_dirs(:)
         integer :: i

         if (tree%ndep <= 0) then
            allocate(dep_dirs(0))
            return
         end if

         allocate(dep_dirs(tree%ndep))
         do i = 1, tree%ndep
            if (allocated(tree%dep(i)%proj_dir)) then
               dep_dirs(i)%s = normalize_path(trim(tree%dep(i)%proj_dir))
            else
               dep_dirs(i)%s = ""
            end if
         end do
      end subroutine collect_dep_dirs

   end subroutine compute_watch_files_from_settings

   !> Return whether the settings correspond to `fpm run` or `fpm test`.
   logical function is_run_or_test(settings)
      class(fpm_build_settings), intent(in) :: settings
      is_run_or_test = .false.
      select type (settings)
       type is (fpm_test_settings)
         is_run_or_test = .true.
       type is (fpm_run_settings)
         is_run_or_test = .true.
      end select
   end function is_run_or_test

   subroutine gather_files_with_mask(targets, visited, target_mask, files, file_mask, build_dir, dep_dirs)
      type(build_target_ptr), intent(in) :: targets(:)
      logical, intent(in) :: visited(:)
      integer(int64), allocatable, intent(in), optional :: target_mask(:)
      type(string_t), allocatable, intent(out) :: files(:)
      integer(int64), allocatable, intent(out) :: file_mask(:)
      character(len=*), intent(in) :: build_dir
      type(string_t), allocatable, intent(in), optional :: dep_dirs(:)

      type(string_t), allocatable :: buf(:)
      integer(int64), allocatable :: mbuf(:)
      integer :: n, cap
      integer :: i, j
      integer(int64) :: m

      n = 0
      cap = 256
      allocate(buf(cap), mbuf(cap))

      do i = 1, size(targets)
         if (.not. visited(i)) cycle
         if (.not. allocated(targets(i)%ptr%source)) cycle

         m = 0_int64
         if (present(target_mask)) then
            if (allocated(target_mask)) m = target_mask(i)
         end if

         if (present(dep_dirs)) then
            call vec_push_unique(buf, mbuf, n, cap, targets(i)%ptr%source%file_name, build_dir, m, dep_dirs)
         else
            call vec_push_unique(buf, mbuf, n, cap, targets(i)%ptr%source%file_name, build_dir, m)
         end if

         if (allocated(targets(i)%ptr%source%include_dependencies)) then
            do j = 1, size(targets(i)%ptr%source%include_dependencies)
               if (present(dep_dirs)) then
                  call vec_push_unique(buf, mbuf, n, cap, targets(i)%ptr%source%include_dependencies(j)%s, build_dir, m, dep_dirs)
               else
                  call vec_push_unique(buf, mbuf, n, cap, targets(i)%ptr%source%include_dependencies(j)%s, build_dir, m)
               end if
            end do
         end if
      end do

      call finalize_vec(buf, mbuf, n, files, file_mask)

   contains

      subroutine finalize_vec(buf, mbuf, n, files, file_mask)
         type(string_t), allocatable, intent(inout) :: buf(:)
         integer(int64), allocatable, intent(inout) :: mbuf(:)
         integer, intent(in) :: n
         type(string_t), allocatable, intent(out) :: files(:)
         integer(int64), allocatable, intent(out) :: file_mask(:)
         integer :: k

         if (n <= 0) then
            allocate(files(0), file_mask(0))
            deallocate(buf, mbuf)
            return
         end if

         allocate(files(n), file_mask(n))
         do k = 1, n
            call move_alloc(buf(k)%s, files(k)%s)
            file_mask(k) = mbuf(k)
         end do
         deallocate(buf, mbuf)
      end subroutine finalize_vec

   end subroutine gather_files_with_mask

   subroutine push_file_with_mask(files, file_mask, path, build_dir, mask, dep_dirs)
      type(string_t), allocatable, intent(inout) :: files(:)
      integer(int64), allocatable, intent(inout) :: file_mask(:)
      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: build_dir
      integer(int64), intent(in) :: mask
      type(string_t), allocatable, intent(in), optional :: dep_dirs(:)

      type(string_t), allocatable :: buf(:)
      integer(int64), allocatable :: mbuf(:)
      integer :: n, cap, i

      if (.not. allocated(files)) then
         n = 0
         cap = 8
         allocate(buf(cap), mbuf(cap))
      else
         n = size(files)
         cap = max(8, n)
         allocate(buf(cap), mbuf(cap))
         do i = 1, n
            buf(i)%s = files(i)%s
            mbuf(i) = file_mask(i)
         end do
      end if

      if (present(dep_dirs)) then
         call vec_push_unique(buf, mbuf, n, cap, path, build_dir, mask, dep_dirs)
      else
         call vec_push_unique(buf, mbuf, n, cap, path, build_dir, mask)
      end if

      call finalize_vec(buf, mbuf, n, files, file_mask)

   contains

      subroutine finalize_vec(buf, mbuf, n, files, file_mask)
         type(string_t), allocatable, intent(inout) :: buf(:)
         integer(int64), allocatable, intent(inout) :: mbuf(:)
         integer, intent(in) :: n
         type(string_t), allocatable, intent(inout) :: files(:)
         integer(int64), allocatable, intent(inout) :: file_mask(:)
         type(string_t), allocatable :: out(:)
         integer(int64), allocatable :: outm(:)
         integer :: k

         if (allocated(files)) deallocate(files)
         if (allocated(file_mask)) deallocate(file_mask)

         if (n <= 0) then
            allocate(files(0), file_mask(0))
            deallocate(buf, mbuf)
            return
         end if

         allocate(out(n), outm(n))
         do k = 1, n
            call move_alloc(buf(k)%s, out(k)%s)
            outm(k) = mbuf(k)
         end do
         deallocate(buf, mbuf)
         call move_alloc(out, files)
         call move_alloc(outm, file_mask)
      end subroutine finalize_vec

   end subroutine push_file_with_mask

   subroutine vec_push_unique(buf, mbuf, n, cap, path, build_dir, mask, dep_dirs)
      type(string_t), allocatable, intent(inout) :: buf(:)
      integer(int64), allocatable, intent(inout) :: mbuf(:)
      integer, intent(inout) :: n, cap
      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: build_dir
      integer(int64), intent(in) :: mask
      type(string_t), allocatable, intent(in), optional :: dep_dirs(:)

      character(len=:), allocatable :: p
      integer :: i

      p = normalize_path(path)
      if (len_trim(p) == 0) return

      if (is_ignored_path(p, build_dir)) then
         if (.not. present(dep_dirs)) return
         if (.not. is_in_dep_dirs(p, dep_dirs)) return
      end if

      if (.not. exists(p)) return

      do i = 1, n
         if (buf(i)%s == p) then
            mbuf(i) = ior(mbuf(i), mask)
            return
         end if
      end do

      if (n >= cap) call vec_grow(buf, mbuf, cap)

      n = n + 1
      buf(n)%s = p
      mbuf(n) = mask
   end subroutine vec_push_unique

   pure logical function is_in_dep_dirs(p, dep_dirs) result(ok)
      character(len=*), intent(in) :: p
      type(string_t), allocatable, intent(in) :: dep_dirs(:)
      integer :: j
      character(len=:), allocatable :: d
      ok = .false.
      if (.not. allocated(dep_dirs)) return
      if (size(dep_dirs) == 0) return

      do j = 1, size(dep_dirs)
         if (len_trim(dep_dirs(j)%s) == 0) cycle
         d = normalize_path(trim(dep_dirs(j)%s))
         if (len_trim(d) == 0) cycle
         if (p == d) then
            ok = .true.
            return
         end if
         if (starts_with(p, d // "/")) then
            ok = .true.
            return
         end if
      end do
   end function is_in_dep_dirs

   subroutine vec_grow(buf, mbuf, cap)
      type(string_t), allocatable, intent(inout) :: buf(:)
      integer(int64), allocatable, intent(inout) :: mbuf(:)
      integer, intent(inout) :: cap

      type(string_t), allocatable :: nb(:)
      integer(int64), allocatable :: nm(:)
      integer :: newcap, i

      newcap = max(16, cap + cap/2)
      allocate(nb(newcap), nm(newcap))
      do i = 1, cap
         nb(i)%s = buf(i)%s
         nm(i) = mbuf(i)
      end do
      call move_alloc(nb, buf)
      call move_alloc(nm, mbuf)
      cap = newcap
   end subroutine vec_grow

   subroutine select_names_or_all(targets, scope, names, keep)
      type(build_target_ptr), intent(in) :: targets(:)
      integer, intent(in) :: scope
      character(len=*), intent(in), optional :: names(:)
      logical, intent(inout) :: keep(:)

      integer :: i, j
      character(len=:), allocatable :: outbase_full, outbase_stem
      logical :: any_name

      any_name = .false.
      if (present(names)) then
         if (size(names) > 0) then
            do j = 1, size(names)
               if (len_trim(names(j)) > 0) then
                  any_name = .true.
                  exit
               end if
            end do
         end if
      end if

      if (.not. any_name) then
         do i = 1, size(targets)
            if (targets(i)%ptr%is_executable_target(scope)) keep(i) = .true.
         end do
         return
      end if

      do i = 1, size(targets)
         if (.not. targets(i)%ptr%is_executable_target(scope)) cycle
         outbase_full = basename(targets(i)%ptr%output_file)
         outbase_stem = basename(targets(i)%ptr%output_file, suffix=.false.)
         do j = 1, size(names)
            if (len_trim(names(j)) == 0) cycle
            if (glob(trim(names(j)), outbase_full) .or. glob(trim(names(j)), outbase_stem)) then
               keep(i) = .true.
               exit
            end if
         end do
      end do
   end subroutine select_names_or_all

   recursive subroutine dfs_mark_mask(node, all, tmask, mask)
      use fpm_targets, only: build_target_t
      type(build_target_t), pointer, intent(in) :: node
      type(build_target_ptr), intent(in) :: all(:)
      integer(int64), intent(inout) :: tmask(:)
      integer(int64), intent(in) :: mask
      integer :: idx, i

      idx = find_ptr_index(node, all)
      if (idx <= 0) return
      if (iand(tmask(idx), mask) /= 0_int64) return

      tmask(idx) = ior(tmask(idx), mask)

      if (allocated(node%dependencies)) then
         do i = 1, size(node%dependencies)
            if (associated(node%dependencies(i)%ptr)) call dfs_mark_mask(node%dependencies(i)%ptr, all, tmask, mask)
         end do
      end if
   end subroutine dfs_mark_mask

   recursive subroutine dfs_mark_bool(node, all, visited)
      use fpm_targets, only: build_target_t
      type(build_target_t), pointer, intent(in) :: node
      type(build_target_ptr), intent(in) :: all(:)
      logical, intent(inout) :: visited(:)
      integer :: idx, i

      idx = find_ptr_index(node, all)
      if (idx <= 0) return
      if (visited(idx)) return
      visited(idx) = .true.

      if (allocated(node%dependencies)) then
         do i = 1, size(node%dependencies)
            if (associated(node%dependencies(i)%ptr)) call dfs_mark_bool(node%dependencies(i)%ptr, all, visited)
         end do
      end if
   end subroutine dfs_mark_bool

   integer function find_ptr_index(p, all) result(idx)
      use fpm_targets, only: build_target_t
      type(build_target_t), pointer, intent(in) :: p
      type(build_target_ptr), intent(in) :: all(:)
      integer :: i
      idx = 0
      do i = 1, size(all)
         if (associated(all(i)%ptr, p)) then
            idx = i
            return
         end if
      end do
   end function find_ptr_index

end module watch_fpm_graph
