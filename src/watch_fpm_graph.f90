!> Build watch lists from fpm's manifest, model, and target graph.
!!
!! Build mode visits all returned targets. Run mode selects application or
!! example closures by name; test mode visits all test executable closures.
!! Sources, resolved includes, and manifests form a unique normalized path list.
!! Optional dependency watching includes dependency paths under the build tree.
!! Up to 64 run/test roots receive bitmasks for later affected-target selection.
module watch_fpm_graph
   use, intrinsic :: iso_fortran_env, only: int64
   use, intrinsic :: iso_c_binding, only: c_int
   use watch_types, only: watch_opts_t, root_info_t
   use watch_log, only: log_event, log_int, log_real, structured_logging
   use watch_platform, only: begin_raw_output, end_raw_output
   use watch_filter, only: normalize_path, is_ignored_path, filter_watch_files, starts_with
   use watch_util, only: ftoa, trim_or_empty
   use fpm,              only: build_model
   use fpm_manifest,     only: package_config_t, get_package_data
   use fpm_model,        only: fpm_model_t, FPM_SCOPE_APP, FPM_SCOPE_TEST, FPM_SCOPE_EXAMPLE
   use fpm_targets,      only: build_target_ptr, targets_from_sources, add_target_ptr
   use fpm_filesystem,   only: basename, exists, join_path, dirname, canon_path, is_absolute_path
   use fpm_strings,      only: string_t, glob, str
   use fpm_error,        only: error_t
   use fpm_dependency,   only: dependency_tree_t
   use fpm_command_line, only: fpm_build_settings, fpm_run_settings, fpm_test_settings
   implicit none
   private
   public compute_watch_files_from_settings

contains

   !> Build fpm's model and return the filtered source/include/manifest closure.
   !!
   !! `settings` may be updated by fpm model construction. `files` contains
   !! unique normalized paths; aligned `file_mask` entries map files to `roots`.
   !! Run selects named applications/examples; test collects all test roots.
   !! Mapping is disabled when there are no roots or more than 64, yielding
   !! empty `roots` and zero file masks while retaining the selected closure.
   !!
   !! `manifest_key` is always zero here; the engine computes it after refresh.
   !! Timing outputs are seconds: `build_secs` sums model, target, and collection
   !! time, excluding manifest reading. On manifest/model/target failure, return
   !! allocated `error` if supplied, otherwise log the stage and exit with status 1.
   !! Watch-list outputs are only usable after a successful return.
   subroutine compute_watch_files_from_settings(settings, w, files, file_mask, roots, &
      manifest_key, build_secs, secs_model, secs_targets, secs_watch, error)
      class(fpm_build_settings), intent(inout) :: settings
      !! Typed build/run/test settings; fpm may update these while constructing its model.
      type(watch_opts_t), intent(in) :: w
      !! Watcher dependency/filter/logging options; unallocated filters impose no restriction.
      type(string_t), allocatable, intent(out) :: files(:)
      !! Unique normalized watched paths, allocated on success (possibly empty).
      integer(int64), allocatable, intent(out) :: file_mask(:)
      !! Root-membership bits aligned with files; zero when root mapping is disabled.
      type(root_info_t), allocatable, intent(out) :: roots(:)
      !! Executable names and distinct mask bits; empty for build mode or unsupported root counts.
      integer(int64), intent(out) :: manifest_key
      !! Reserved output, always zero; computed from accepted files by the engine.
      real, intent(out) :: build_secs
      !! Sum of model, target-construction, and watch-list collection times in seconds.
      real, intent(out) :: secs_model, secs_targets, secs_watch
      !! Separate stage durations in seconds, respectively model, targets, and file collection.
      type(error_t), allocatable, intent(out), optional :: error
      !! Allocated for manifest/model/target failure; omitted error output makes those failures fatal.

      type(package_config_t), allocatable :: package
      type(fpm_model_t),       allocatable :: model
      type(build_target_ptr),  allocatable :: targets(:)
      type(error_t), allocatable :: err

      logical, allocatable :: keep(:)
      logical, allocatable :: visited(:)
      integer(int64), allocatable :: target_mask(:)

      type(string_t), allocatable :: dep_dirs(:)

      integer :: i, r, nroots
      integer(int64) :: rate, t0, t1, tw0, tw1
      integer :: n_before, n_after
      integer(c_int) :: saved_output

      secs_model   = 0.0
      secs_targets = 0.0
      secs_watch   = 0.0
      build_secs   = 0.0
      manifest_key = 0_int64

      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64

      allocate(package, model)

      call begin_raw_output(structured_logging(), saved_output)
      call get_package_data(package, "fpm.toml", err, apply_defaults=.true.)
      call end_raw_output(saved_output)
      if (allocated(err)) then
         call handle_graph_error(err, error, "manifest")
         return
      end if

      call system_clock(t0)
      call begin_raw_output(structured_logging(), saved_output)
      call build_model(model, settings, package, err)
      call end_raw_output(saved_output)
      call system_clock(t1)
      secs_model = real(t1 - t0) / real(rate)

      if (allocated(err)) then
         call handle_graph_error(err, error, "model")
         return
      end if

      call collect_dep_dirs(model%deps, dep_dirs)
      if (w%watch_deps .and. w%verbosity >= 1) then
         call log_event("info", "graph.dependencies", "Watching " // str(size(dep_dirs)) // " dependencies", &
                       [log_int("dependency_count", size(dep_dirs))], verbosity=1)
      end if

      call system_clock(t0)
      call begin_raw_output(structured_logging(), saved_output)
      call targets_from_sources(targets, model, settings%prune, package%library, err)
      call end_raw_output(saved_output)
      call system_clock(t1)
      secs_targets = real(t1 - t0) / real(rate)

      if (allocated(err)) then
         call release_targets(targets)
         call handle_graph_error(err, error, "targets")
         return
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

      call gather_files_with_mask(targets, visited, target_mask, files, file_mask, &
         trim_or_empty(settings%build_dir), model, w%watch_deps, dep_dirs)

      call push_file_with_mask(files, file_mask, "fpm.toml", trim_or_empty(settings%build_dir), 0_int64, dep_dirs)

      if (w%watch_deps) then
         do i = 1, size(dep_dirs)
            if (len_trim(dep_dirs(i)%s) == 0) cycle
            call push_file_with_mask(files, file_mask, join_path(dep_dirs(i)%s, "fpm.toml"), &
               trim_or_empty(settings%build_dir), 0_int64, dep_dirs)
         end do
      end if

      n_before = 0
      if (allocated(files)) n_before = size(files)

      call filter_watch_files(files, w%include, w%ignore, file_mask)

      n_after = 0
      if (allocated(files)) n_after = size(files)

      if (w%debug) then
         call log_event("debug", "graph.filtered", "Filtered watch list", &
                        [log_int("files_before", n_before), log_int("files_after", n_after)])
      end if

      call system_clock(tw1)
      secs_watch = real(tw1 - tw0) / real(rate)

      build_secs = secs_model + secs_targets + secs_watch

      if (w%verbosity >= 1) then
         call log_event("info", "graph.ready", "Computed watch graph in " // ftoa(build_secs) // "s", &
                       [log_real("model_ms", secs_model * 1000.0), log_real("targets_ms", secs_targets * 1000.0), &
                        log_real("watch_list_ms", secs_watch * 1000.0), log_int("target_count", size(targets)), &
                        log_int("closure_count", count(visited))], verbosity=1)
      end if

      deallocate(package, model)
      call release_targets(targets)
      if (allocated(keep)) deallocate(keep)
      if (allocated(visited)) deallocate(visited)
      if (allocated(target_mask)) deallocate(target_mask)
      if (allocated(dep_dirs)) deallocate(dep_dirs)

   contains

      !> Return canonicalized dependency project directories, excluding the main-project node.
      subroutine collect_dep_dirs(tree, dep_dirs)
         type(dependency_tree_t), intent(in) :: tree
         type(string_t), allocatable, intent(out) :: dep_dirs(:)
         integer :: i

         if (tree%ndep <= 1) then
            allocate(dep_dirs(0))
            return
         end if

         ! The first node belongs to the main project, not a dependency.
         allocate(dep_dirs(tree%ndep - 1))
         do i = 2, tree%ndep
            if (allocated(tree%dep(i)%proj_dir)) then
               dep_dirs(i-1)%s = normalize_path(canon_path(tree%dep(i)%proj_dir))
            else
               dep_dirs(i-1)%s = ""
            end if
         end do
      end subroutine collect_dep_dirs

   end subroutine compute_watch_files_from_settings

   !> Transfer `err` to an optional error output, or log its stage and exit with status 1.
   subroutine handle_graph_error(err, error, stage)
      type(error_t), allocatable, intent(inout) :: err
      type(error_t), allocatable, intent(out), optional :: error
      character(len=*), intent(in) :: stage

      if (present(error)) then
         call move_alloc(err, error)
      else
         call log_event("error", "graph.failed", stage // " error: " // err%message)
         stop 1
      end if
   end subroutine handle_graph_error

   !> Release the uniquely owned target objects and their shared dependency references.
   !!
   !! fpm returns pointer wrappers without finalizers. Collect every reachable node
   !! while all pointers remain valid, then remove the dependency references before
   !! deallocating each target and its allocatable source/command data exactly once.
   !! Nodes already discarded without remaining references by fpm's pruning routine
   !! cannot be recovered through the returned target graph.
   subroutine release_targets(targets)
      type(build_target_ptr), allocatable, intent(inout) :: targets(:)
      type(build_target_ptr), allocatable :: owned(:)
      integer :: i, j

      if (.not. allocated(targets)) return
      allocate(owned(0))
      do i = 1, size(targets)
         if (.not. associated(targets(i)%ptr)) cycle
         if (find_ptr_index(targets(i)%ptr, owned) > 0) cycle
         call add_target_ptr(owned, targets(i))
      end do

      i = 1
      do while (i <= size(owned))
         if (allocated(owned(i)%ptr%dependencies)) then
            do j = 1, size(owned(i)%ptr%dependencies)
               if (.not. associated(owned(i)%ptr%dependencies(j)%ptr)) cycle
               if (find_ptr_index(owned(i)%ptr%dependencies(j)%ptr, owned) > 0) cycle
               call add_target_ptr(owned, owned(i)%ptr%dependencies(j))
            end do
         end if
         i = i + 1
      end do

      deallocate(targets)
      do i = 1, size(owned)
         if (allocated(owned(i)%ptr%dependencies)) deallocate(owned(i)%ptr%dependencies)
      end do
      do i = 1, size(owned)
         deallocate(owned(i)%ptr)
      end do
   end subroutine release_targets

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

   !> Collect visited source targets and resolved includes with aligned root masks.
   !!
   !! `visited` aligns with `targets`; an absent/unallocated `target_mask` yields
   !! zero masks. When dependency watching is disabled, skip foreign-package
   !! sources and includes inside dependency directories. Duplicate paths merge
   !! masks; only existing paths passing build-directory checks are retained.
   subroutine gather_files_with_mask(targets, visited, target_mask, files, file_mask, build_dir, model, watch_deps, dep_dirs)
      type(build_target_ptr), intent(in) :: targets(:)
      logical, intent(in) :: visited(:)
      integer(int64), allocatable, intent(in), optional :: target_mask(:)
      type(string_t), allocatable, intent(out) :: files(:)
      integer(int64), allocatable, intent(out) :: file_mask(:)
      character(len=*), intent(in) :: build_dir
      type(fpm_model_t), intent(in) :: model
      logical, intent(in) :: watch_deps
      type(string_t), allocatable, intent(in) :: dep_dirs(:)

      type(string_t), allocatable :: buf(:)
      integer(int64), allocatable :: mbuf(:)
      integer :: n, cap
      integer :: i, j
      integer(int64) :: m
      character(len=:), allocatable :: include_file

      n = 0
      cap = 256
      allocate(buf(cap), mbuf(cap))

      do i = 1, size(targets)
         if (.not. visited(i)) cycle
         if (.not. allocated(targets(i)%ptr%source)) cycle
         if (.not. watch_deps) then
            if (allocated(targets(i)%ptr%package_name)) then
               if (targets(i)%ptr%package_name /= model%package_name) cycle
            end if
         end if

         m = 0_int64
         if (present(target_mask)) then
            if (allocated(target_mask)) m = target_mask(i)
         end if

         call vec_push_unique(buf, mbuf, n, cap, targets(i)%ptr%source%file_name, build_dir, m, dep_dirs)

         if (allocated(targets(i)%ptr%source%include_dependencies)) then
            do j = 1, size(targets(i)%ptr%source%include_dependencies)
               include_file = resolve_include_file(targets(i)%ptr%source%file_name, &
                  targets(i)%ptr%source%include_dependencies(j)%s, model%include_dirs)
               if (len_trim(include_file) == 0) cycle
               if (.not. watch_deps) then
                  if (is_in_dep_dirs(include_file, dep_dirs)) cycle
               end if
               call vec_push_unique(buf, mbuf, n, cap, include_file, build_dir, m, dep_dirs)
            end do
         end if
      end do

      call finalize_vec(buf, mbuf, n, files, file_mask)

   contains

      !> Transfer the populated buffer prefix into exact-size aligned file/mask arrays and release buffers.
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

   !> Resolve an include path, returning a normalized canonical path or an empty string.
   !!
   !! Existing absolute paths are accepted directly. Relative names are searched
   !! beside the source, then in fpm include directories in order, then relative
   !! to the current working directory. The first existing candidate wins.
   function resolve_include_file(source_file, include_name, include_dirs) result(path)
      character(len=*), intent(in) :: source_file, include_name
      type(string_t), allocatable, intent(in) :: include_dirs(:)
      character(len=:), allocatable :: path, candidate
      integer :: i

      path = ""
      if (len_trim(include_name) == 0) return

      if (is_absolute_path(include_name)) then
         if (exists(include_name)) path = normalize_path(canon_path(include_name))
         return
      end if

      candidate = normalize_path(canon_path(join_path(dirname(source_file), include_name)))
      if (exists(candidate)) then
         path = candidate
         return
      end if

      if (allocated(include_dirs)) then
         do i = 1, size(include_dirs)
            candidate = normalize_path(canon_path(join_path(include_dirs(i)%s, include_name)))
            if (.not. exists(candidate)) cycle
            path = candidate
            return
         end do
      end if

      ! Some compilers also search the invocation directory for Fortran INCLUDE.
      if (exists(include_name)) path = normalize_path(canon_path(include_name))
   end function resolve_include_file

   !> Append an existing eligible path to aligned arrays, merging mask bits for duplicates.
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

      !> Transfer the populated buffer prefix into exact-size aligned file/mask arrays and release buffers.
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

   !> Add a normalized existing path to a growable buffer, merging duplicate masks.
   !!
   !! Exclude build-directory paths unless supplied dependency directories contain
   !! the path. `n` counts populated entries; `cap` and both buffers grow together.
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

   !> Test a normalized path against nonempty normalized dependency-directory prefixes.
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

   !> Grow both file/mask buffers to at least 16 or 1.5 times the current capacity.
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

   !> Mark executable targets in `scope` matching names without clearing existing marks.
   !!
   !! Absent/empty name patterns select all executables in scope. Otherwise match
   !! fpm globs against each output basename with and without its suffix.
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
            if (glob(outbase_full, trim(names(j))) .or. glob(outbase_stem, trim(names(j)))) then
               keep(i) = .true.
               exit
            end if
         end do
      end do
   end subroutine select_names_or_all

   !> Recursively OR one root bit into reachable target masks, skipping already marked nodes.
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

   !> Recursively mark reachable targets in `visited`, skipping already visited nodes.
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

   !> Return the first 1-based index associated with target pointer `p`, or zero if absent.
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
