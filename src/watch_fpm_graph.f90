!> Build watch lists from fpm's manifest, model, and target graph.
!!
!! Build mode watches every model source. Run/test mode selects named executable
!! source closures. Link-only sources are conservatively retained because fpm
!! does not expose its linker graph independently of its pruning allocator.
!! Sources, include candidates, manifests, and version files form one normalized path list.
!! Optional dependency watching includes dependency paths under the build tree.
!! Up to 64 run/test roots receive bitmasks for later affected-target selection.
module watch_fpm_graph
   use, intrinsic :: iso_fortran_env, only: int64
   use, intrinsic :: iso_c_binding, only: c_int
   use watch_types, only: watch_opts_t, root_info_t
   use watch_log, only: log_event, log_int, log_real, structured_logging
   use watch_platform, only: begin_raw_output, end_raw_output, is_windows
   use watch_filter, only: normalize_path, is_ignored_path, filter_watch_files, starts_with
   use watch_util, only: ftoa, trim_or_empty
   use fpm,              only: build_model
   use fpm_manifest,     only: package_config_t, get_package_data
   use fpm_model,        only: fpm_model_t, srcfile_t, FPM_SCOPE_APP, FPM_SCOPE_TEST, FPM_SCOPE_EXAMPLE, &
                              FPM_SCOPE_LIB, FPM_SCOPE_DEP, FPM_UNIT_PROGRAM, FPM_UNIT_SUBMODULE, &
                              FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, FPM_UNIT_CPPSOURCE
   use fpm_targets,      only: build_target_ptr, new_target, resolve_module_dependencies, FPM_TARGET_OBJECT
   use fpm_sources,      only: get_exe_name_with_suffix
   use fpm_source_parsing, only: parse_f_source, parse_c_source
   use fpm_filesystem,   only: basename, exists, join_path, dirname, canon_path, is_absolute_path
   use fpm_strings,      only: string_t, glob, str, fnv_1a
   use fpm_error,        only: error_t
   use fpm_toml,         only: read_package_file, get_value
   use fpm_versioning,   only: version_t, new_version
   use tomlf,            only: toml_table
   use fpm_dependency,   only: dependency_tree_t
   use fpm_command_line, only: fpm_build_settings, fpm_run_settings, fpm_test_settings
   use fpm_os,           only: get_current_directory
   implicit none
   private
   public compute_watch_files_from_settings

   !> Include edges parsed once per unique path during one graph construction.
   type include_edges_t
      integer, allocatable :: children(:)
      integer :: count = 0
      logical :: parsed = .false.
   end type include_edges_t

   !> Shared growable path/mask collection with indexed, cached include edges.
   type file_collection_t
      !> Absolute project base shared by path comparisons without changing exported paths.
      character(len=:), allocatable :: project_dir
      type(string_t), allocatable :: paths(:)
      !> Internal root bits also drive include propagation and cycle detection.
      integer(int64), allocatable :: masks(:)
      !> Global metadata bypasses filters and exports a zero mask regardless of internal root bits.
      logical, allocatable :: protected(:)
      type(include_edges_t), allocatable :: includes(:)
      integer, allocatable :: slots(:)
      integer :: count = 0
   contains
      procedure :: add => collection_add
      procedure :: finish => collection_finish
   end type file_collection_t

contains

   !> Build fpm's model and return the filtered source/include/manifest closure.
   !!
   !! `settings` may be updated by fpm model construction. `files` contains
   !! unique normalized paths; aligned `file_mask` entries map files to `roots`.
   !! Run/test select named roots; unselected library modules are omitted unless
   !! reached through fpm module dependencies or submodule implementations. With
   !! pruning disabled or separate library outputs, retain whole library inputs.
   !! Mapping is disabled when there are no roots or more than 64, yielding
   !! empty `roots` and zero file masks while retaining the selected closure.
   !!
   !! Timing outputs are seconds: `build_secs` sums model, target, and collection
   !! time, excluding manifest reading. Build watches all model files conservatively;
   !! it does not reproduce fpm pruning. On manifest/model/target failure, return
   !! allocated `error` if supplied, otherwise log the stage and exit with status 1.
   !! Watch-list outputs are only usable after a successful return.
   subroutine compute_watch_files_from_settings(settings, w, files, file_mask, roots, &
      build_secs, secs_model, secs_targets, secs_watch, error, metadata_files)
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
      real, intent(out) :: build_secs
      !! Sum of model, target-construction, and watch-list collection times in seconds.
      real, intent(out) :: secs_model, secs_targets, secs_watch
      !! Separate stage durations in seconds, respectively model, targets, and file collection.
      type(error_t), allocatable, intent(out), optional :: error
      !! Allocated for manifest/model/target failure; omitted error output makes those failures fatal.
      type(string_t), allocatable, intent(in), optional :: metadata_files(:)
      !! Additional protected metadata paths, including missing lookup candidates.

      type(package_config_t), allocatable :: package
      type(fpm_model_t),       allocatable :: model
      type(build_target_ptr),  allocatable :: targets(:)
      type(error_t), allocatable :: err

      logical, allocatable :: keep(:)
      logical, allocatable :: visited(:), metadata(:)
      integer(int64), allocatable :: target_mask(:)

      type(string_t), allocatable :: dep_dirs(:)
      type(file_collection_t) :: collected
      character(len=:), allocatable :: build_dir

      integer :: i, r, nroots
      logical :: whole_libraries
      integer(int64) :: rate, t0, t1, tw0, tw1
      integer :: n_before, n_after
      integer(c_int) :: saved_output

      secs_model   = 0.0
      secs_targets = 0.0
      secs_watch   = 0.0
      build_secs   = 0.0

      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64

      call get_current_directory(collected%project_dir, err)
      if (allocated(err)) then
         call handle_graph_error(err, error, "directory")
         return
      end if
      collected%project_dir = normalize_path(canon_path(collected%project_dir))

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
      whole_libraries = .not. settings%prune
      if (allocated(package%library)) whole_libraries = whole_libraries .or. .not. package%library%monolithic()
      call build_watch_targets(model, is_run_or_test(settings), targets, err)
      call end_raw_output(saved_output)
      call system_clock(t1)
      secs_targets = real(t1 - t0) / real(rate)

      if (allocated(err)) then
         call release_targets(targets)
         call handle_graph_error(err, error, "targets")
         return
      end if

      call system_clock(tw0)

      build_dir = trim_or_empty(settings%build_dir)
      if (len(build_dir) > 0) build_dir = project_path(build_dir, collected%project_dir)

      allocate(keep(size(targets)))
      keep = .false.

      select type (s => settings)
       type is (fpm_test_settings)
         call select_names_or_all(targets, FPM_SCOPE_TEST, s%name, keep)
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
         call mark_link_inputs(targets, keep, whole_libraries, target_mask, visited)
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
            call mark_link_inputs(targets, keep, whole_libraries, target_mask, visited)
         end if
      end if

      call gather_files_with_mask(targets, visited, target_mask, collected, &
         build_dir, model, w%watch_deps, dep_dirs)

      call collect_manifest_metadata(collected, "fpm.toml", build_dir, dep_dirs)

      if (w%watch_deps) then
         do i = 1, size(dep_dirs)
            if (len_trim(dep_dirs(i)%s) == 0) cycle
            call collect_manifest_metadata(collected, join_path(dep_dirs(i)%s, "fpm.toml"), &
               build_dir, dep_dirs)
         end do
      end if

      if (present(metadata_files)) then
         if (allocated(metadata_files)) then
            do i = 1, size(metadata_files)
               call collected%add(metadata_files(i)%s, build_dir, 0_int64, &
                  dep_dirs, preserve=.true., allow_missing=.true.)
            end do
         end if
      end if

      call collected%finish(files, file_mask, metadata)

      n_before = 0
      if (allocated(files)) n_before = size(files)

      call filter_watch_files(files, w%include, w%ignore, file_mask, preserve=metadata)

      n_after = 0
      if (allocated(files)) n_after = size(files)

      if (w%verbosity >= 2) then
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

   !> Own one fpm target per model source and delegate USE resolution to fpm.
   !!
   !! No fpm pruning routine is called: its discarded pointer allocations cannot
   !! be recovered by callers. These nodes serve watch membership only. Every
   !! allocation stays in `targets`, including on dependency-resolution errors.
   !! Build mode needs no closure edges because it retains every model source.
   subroutine build_watch_targets(model, resolve_dependencies, targets, error)
      type(fpm_model_t), intent(in) :: model
      logical, intent(in) :: resolve_dependencies
      type(build_target_ptr), allocatable, intent(out) :: targets(:)
      type(error_t), allocatable, intent(out) :: error
      integer :: i, j, k, n
      character(len=:), allocatable :: output

      n = 0
      do i = 1, size(model%packages)
         n = n + size(model%packages(i)%sources)
      end do
      allocate(targets(n))
      k = 0
      do i = 1, size(model%packages)
         do j = 1, size(model%packages(i)%sources)
            k = k + 1
            output = model%packages(i)%sources(j)%file_name
            if (model%packages(i)%sources(j)%unit_type == FPM_UNIT_PROGRAM) then
               output = get_exe_name_with_suffix(model%packages(i)%sources(j))
            end if
            targets(k) = new_target(model%packages(i)%name, FPM_TARGET_OBJECT, &
               output_name=output, source=model%packages(i)%sources(j))
            targets(k)%ptr%schedule = k
         end do
      end do
      if (.not. resolve_dependencies) return
      call resolve_module_dependencies(targets, model%external_modules, error)
      if (allocated(error)) return
      call add_submodule_implementations(targets)
   end subroutine build_watch_targets

   !> Add reverse parent-to-implementation edges in one allocation per parent.
   !!
   !! fpm resolves each submodule's forward USE dependencies. Watching a parent
   !! also needs its implementation, including nested submodules. Only these
   !! sparse reverse edges are stored; shared linker inputs are marked directly.
   subroutine add_submodule_implementations(targets)
      type(build_target_ptr), intent(inout) :: targets(:)
      type(build_target_ptr), allocatable :: dependencies(:)
      integer, allocatable :: implementations(:), children(:)
      integer :: i, j, k, l, n, existing

      implementations = pack([(i, i=1,size(targets))], &
         [(targets(i)%ptr%source%unit_type == FPM_UNIT_SUBMODULE, i=1,size(targets))])
      if (size(implementations) == 0) return
      allocate(children(size(implementations)))
      do i = 1, size(targets)
         if (size(targets(i)%ptr%source%modules_provided) == 0) cycle
         n = 0
         do j = 1, size(implementations)
            k = implementations(j)
            if (i == k) cycle
            if (.not. implements_parent(targets(k)%ptr%source, targets(i)%ptr%source)) cycle
            if (any([(associated(targets(i)%ptr%dependencies(l)%ptr, targets(k)%ptr), &
               l=1,size(targets(i)%ptr%dependencies))])) cycle
            n = n + 1
            children(n) = k
         end do
         if (n == 0) cycle
         existing = size(targets(i)%ptr%dependencies)
         allocate(dependencies(existing+n))
         dependencies(:existing) = targets(i)%ptr%dependencies
         do j = 1, n
            dependencies(existing+j)%ptr => targets(children(j))%ptr
         end do
         call move_alloc(dependencies, targets(i)%ptr%dependencies)
      end do
   end subroutine add_submodule_implementations

   !> Test the parent names parsed by fpm without duplicating module resolution.
   pure logical function implements_parent(child, parent) result(matches)
      type(srcfile_t), intent(in) :: child, parent
      integer :: i, j
      matches = .false.
      do i = 1, size(child%parent_modules)
         if (.not. any([(child%parent_modules(i)%s == parent%modules_provided(j)%s, &
            j=1,size(parent%modules_provided))])) cycle
         matches = .true.
         return
      end do
   end function implements_parent

   !> Mark shared and executable-local link inputs without a dense edge matrix.
   !!
   !! Link-only library sources belong to every selected executable; unpruned or
   !! separate library outputs retain all library sources. Propagate their union
   !! of root bits once, then mark scoped inputs beside each selected program.
   !! When more than 64 roots are selected, boolean traversal retains the same
   !! source closure without allocating masks or per-executable adjacency lists.
   subroutine mark_link_inputs(targets, keep, whole_libraries, target_mask, visited)
      type(build_target_ptr), intent(in) :: targets(:)
      logical, intent(in) :: keep(:), whole_libraries
      integer(int64), allocatable, intent(inout) :: target_mask(:)
      logical, intent(inout) :: visited(:)
      integer :: i, j, r
      integer(int64) :: all_roots, mask
      logical :: library_source, link_source, selected
      character(len=:), allocatable :: source_dir

      if (.not. any(keep)) return
      all_roots = 0_int64
      if (allocated(target_mask)) then
         do r = 1, count(keep)
            all_roots = ibset(all_roots, r-1)
         end do
      end if
      do i = 1, size(targets)
         associate(source => targets(i)%ptr%source)
            library_source = any(source%unit_scope == [FPM_SCOPE_LIB, FPM_SCOPE_DEP])
            link_source = any(source%unit_type == [FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, FPM_UNIT_CPPSOURCE])
            if (library_source .and. (whole_libraries .or. link_source)) then
               mask = all_roots
               selected = .true.
            else
               if (.not. link_source) cycle
               mask = 0_int64
               selected = .false.
               source_dir = dirname(source%file_name)
               r = 0
               do j = 1, size(targets)
                  if (.not. keep(j)) cycle
                  r = r + 1
                  if (source%unit_scope /= targets(j)%ptr%source%unit_scope) cycle
                  if (.not. starts_with(source_dir, dirname(targets(j)%ptr%source%file_name))) cycle
                  selected = .true.
                  if (allocated(target_mask)) then
                     mask = ibset(mask, r-1)
                  else
                     exit
                  end if
               end do
            end if
         end associate
         if (.not. selected) cycle
         if (allocated(target_mask)) then
            call dfs_mark_mask(targets(i)%ptr, targets, target_mask, mask)
         else
            call dfs_mark_bool(targets(i)%ptr, targets, visited)
         end if
      end do
   end subroutine mark_link_inputs

   !> Release every owned source node once, independent of shared dependency edges.
   subroutine release_targets(targets)
      type(build_target_ptr), allocatable, intent(inout) :: targets(:)
      integer :: i
      if (.not. allocated(targets)) return
      do i = 1, size(targets)
         if (associated(targets(i)%ptr)) deallocate(targets(i)%ptr)
      end do
      deallocate(targets)
   end subroutine release_targets

   !> Check program membership directly on fpm's source metadata.
   pure logical function is_program_in_scope(target, scope) result(is_program)
      type(build_target_ptr), intent(in) :: target
      integer, intent(in) :: scope
      is_program = .false.
      if (.not. allocated(target%ptr%source)) return
      is_program = target%ptr%source%unit_type == FPM_UNIT_PROGRAM .and. target%ptr%source%unit_scope == scope
   end function is_program_in_scope

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

   !> Collect selected source/include closures into the shared path/mask collection.
   subroutine gather_files_with_mask(targets, visited, target_mask, collected, build_dir, model, watch_deps, dep_dirs)
      type(build_target_ptr), intent(in) :: targets(:)
      logical, intent(in) :: visited(:)
      integer(int64), allocatable, intent(in) :: target_mask(:)
      type(file_collection_t), intent(inout) :: collected
      character(len=*), intent(in) :: build_dir
      type(fpm_model_t), intent(in) :: model
      logical, intent(in) :: watch_deps
      type(string_t), allocatable, intent(in) :: dep_dirs(:)
      integer :: i
      integer(int64) :: mask

      do i = 1, size(targets)
         if (.not. visited(i)) cycle
         if (.not. watch_deps) then
            if (targets(i)%ptr%package_name /= model%package_name) cycle
         end if
         mask = 0_int64
         if (allocated(target_mask)) mask = target_mask(i)
         call gather_include_closure(targets(i)%ptr%source%file_name, mask, collected, &
            build_dir, model%include_dirs, watch_deps, dep_dirs)
      end do
   end subroutine gather_files_with_mask

   !> Recover original and nested includes through fpm's public source parsers.
   !!
   !! The Fortran parser is deliberately called without preprocessing: fpm's
   !! CPP-enabled model parser expands and discards the original include paths.
   !! The C parser supplies quoted CPP directives, including in Fortran sources.
   !! Parse both languages once per path so mixed include trees remain complete.
   !! Later roots propagate their bits through cached edges without rereading files.
   !! Merging bits before recursion terminates cycles and shared subtrees.
   recursive subroutine gather_include_closure(path, mask, collected, build_dir, include_dirs, watch_deps, dep_dirs, path_index)
      character(len=*), intent(in) :: path, build_dir
      integer(int64), intent(in) :: mask
      type(file_collection_t), intent(inout) :: collected
      type(string_t), allocatable, intent(in) :: include_dirs(:), dep_dirs(:)
      logical, intent(in) :: watch_deps
      integer, intent(out), optional :: path_index
      type(srcfile_t) :: parsed
      type(error_t), allocatable :: parse_error
      logical :: changed
      integer :: language, i, io_status, index
      integer(int64) :: file_size

      if (present(path_index)) path_index = 0
      if (.not. watch_deps) then
         if (is_in_dep_dirs(normalize_path(path), dep_dirs)) return
      end if
      call collected%add(path, build_dir, mask, dep_dirs, changed, allow_missing=.true., path_index=index)
      if (present(path_index)) path_index = index
      if (.not. changed) return
      if (collected%includes(index)%parsed) then
         do i = 1, collected%includes(index)%count
            call propagate_include_mask(collected%includes(index)%children(i), mask, collected)
         end do
         return
      end if
      collected%includes(index)%parsed = .true.
      ! Empty files have no includes; retain them without invoking either parser.
      inquire(file=path, size=file_size, iostat=io_status)
      if (io_status /= 0) return
      if (file_size <= 0_int64) return
      do language = 1, 2
         if (language == 1) then
            parsed = parse_f_source(path, parse_error)
         else
            parsed = parse_c_source(path, parse_error)
         end if
         if (.not. allocated(parsed%include_dependencies)) cycle
         do i = 1, size(parsed%include_dependencies)
            call gather_include_candidates(index, path, parsed%include_dependencies(i)%s, mask, collected, &
               build_dir, include_dirs, watch_deps, dep_dirs)
         end do
      end do
   end subroutine gather_include_closure

   !> Propagate new root bits over cached edges without path lookup or file I/O.
   recursive subroutine propagate_include_mask(index, mask, collected)
      integer, intent(in) :: index
      integer(int64), intent(in) :: mask
      type(file_collection_t), intent(inout) :: collected
      integer :: i

      if (ior(collected%masks(index), mask) == collected%masks(index)) return
      collected%masks(index) = ior(collected%masks(index), mask)
      do i = 1, collected%includes(index)%count
         call propagate_include_mask(collected%includes(index)%children(i), mask, collected)
      end do
   end subroutine propagate_include_mask

   !> Append one resolved candidate edge with amortized linear storage growth.
   subroutine append_include_edge(edges, child)
      type(include_edges_t), intent(inout) :: edges
      integer, intent(in) :: child
      integer, allocatable :: grown(:)
      integer :: capacity

      if (child == 0) return
      if (.not. allocated(edges%children)) allocate(edges%children(4))
      if (edges%count == size(edges%children)) then
         capacity = 2 * size(edges%children)
         allocate(grown(capacity))
         grown(:edges%count) = edges%children(:edges%count)
         call move_alloc(grown, edges%children)
      end if
      edges%count = edges%count + 1
      edges%children(edges%count) = child
   end subroutine append_include_edge

   !> Retain searched include candidates through the first existing match.
   !!
   !! Missing search locations remain watched so creation, deletion, and
   !! recreation preserve the same path and root mask. Search beside the source,
   !! then fpm include directories, then the invocation directory. Candidates
   !! following an existing match cannot currently affect resolution and are omitted.
   recursive subroutine gather_include_candidates(source_index, source_file, include_name, mask, collected, &
      build_dir, include_dirs, watch_deps, dep_dirs)
      integer, intent(in) :: source_index
      character(len=*), intent(in) :: source_file, include_name, build_dir
      integer(int64), intent(in) :: mask
      type(file_collection_t), intent(inout) :: collected
      type(string_t), allocatable, intent(in) :: include_dirs(:), dep_dirs(:)
      logical, intent(in) :: watch_deps
      character(len=:), allocatable :: candidate
      integer :: i, child

      if (len_trim(include_name) == 0) return
      if (is_absolute_path(include_name)) then
         call gather_include_closure(normalize_path(canon_path(include_name)), mask, collected, &
            build_dir, include_dirs, watch_deps, dep_dirs, child)
         call append_include_edge(collected%includes(source_index), child)
         return
      end if

      candidate = normalize_path(canon_path(join_path(dirname(source_file), include_name)))
      call gather_include_closure(candidate, mask, collected, build_dir, include_dirs, watch_deps, dep_dirs, child)
      call append_include_edge(collected%includes(source_index), child)
      if (exists(candidate)) return
      if (allocated(include_dirs)) then
         do i = 1, size(include_dirs)
            candidate = normalize_path(canon_path(join_path(include_dirs(i)%s, include_name)))
            call gather_include_closure(candidate, mask, collected, build_dir, include_dirs, watch_deps, dep_dirs, child)
            call append_include_edge(collected%includes(source_index), child)
            if (exists(candidate)) return
         end do
      end if
      call gather_include_closure(normalize_path(canon_path(include_name)), mask, collected, &
         build_dir, include_dirs, watch_deps, dep_dirs, child)
      call append_include_edge(collected%includes(source_index), child)
   end subroutine gather_include_candidates

   !> Watch each manifest and its fpm-supported version file as protected metadata.
   !!
   !! Let fpm parse both TOML and version strings. A version entry names a file
   !! only if fpm rejects it as a literal version and the relative file exists.
   subroutine collect_manifest_metadata(collected, manifest, build_dir, dep_dirs)
      type(file_collection_t), intent(inout) :: collected
      character(len=*), intent(in) :: manifest, build_dir
      type(string_t), allocatable, intent(in) :: dep_dirs(:)
      type(toml_table), allocatable :: table
      type(version_t) :: version
      type(error_t), allocatable :: error
      character(len=:), allocatable :: value, version_file

      call collected%add(manifest, build_dir, 0_int64, dep_dirs, preserve=.true.)
      call read_package_file(table, manifest, error)
      if (allocated(error)) return
      call get_value(table, "version", value, "0")
      call new_version(version, value, error)
      if (.not. allocated(error)) return
      version_file = join_path(dirname(manifest), value)
      if (exists(version_file)) call collected%add(version_file, build_dir, 0_int64, dep_dirs, preserve=.true.)
   end subroutine collect_manifest_metadata

   !> Resolve a path against the project base using fpm's lexical canonicalization.
   function project_path(path, project_dir) result(absolute)
      character(len=*), intent(in) :: path, project_dir
      character(len=:), allocatable :: absolute

      absolute = normalize_path(path)
      if (.not. is_absolute_path(absolute)) absolute = join_path(project_dir, absolute)
      absolute = normalize_path(canon_path(absolute))
   end function project_path

   !> Insert an eligible path and merge root bits; include candidates may be missing.
   subroutine collection_add(self, path, build_dir, mask, dep_dirs, changed, preserve, allow_missing, path_index)
      class(file_collection_t), intent(inout) :: self
      character(len=*), intent(in) :: path, build_dir
      integer(int64), intent(in) :: mask
      type(string_t), allocatable, intent(in) :: dep_dirs(:)
      logical, intent(out), optional :: changed
      logical, intent(in), optional :: preserve, allow_missing
      integer, intent(out), optional :: path_index
      logical :: protected, keep_missing
      character(len=:), allocatable :: normalized
      integer :: slot, index

      if (present(changed)) changed = .false.
      if (present(path_index)) path_index = 0
      protected = .false.
      if (present(preserve)) protected = preserve
      keep_missing = .false.
      if (present(allow_missing)) keep_missing = allow_missing
      normalized = normalize_path(canon_path(path))
      if (len_trim(normalized) == 0) return
      if (.not. protected .and. is_ignored_path(project_path(normalized, self%project_dir), build_dir, is_windows())) then
         if (.not. is_in_dep_dirs(normalized, dep_dirs)) return
      end if
      if (.not. keep_missing) then
         if (.not. exists(normalized)) return
      end if
      if (.not. allocated(self%paths)) call collection_grow(self)
      if (self%count * 2 >= size(self%slots)) call collection_grow(self)
      slot = collection_slot(self, normalized)
      index = self%slots(slot)
      if (index > 0) then
         if (present(path_index)) path_index = index
         if (present(changed)) changed = ior(self%masks(index), mask) /= self%masks(index)
         self%masks(index) = ior(self%masks(index), mask)
         self%protected(index) = self%protected(index) .or. protected
         return
      end if
      self%count = self%count + 1
      self%slots(slot) = self%count
      self%paths(self%count)%s = normalized
      self%masks(self%count) = mask
      self%protected(self%count) = protected
      if (present(path_index)) path_index = self%count
      if (present(changed)) changed = .true.
   end subroutine collection_add

   !> Find a path's matching or empty open-addressed slot.
   integer function collection_slot(self, path) result(slot)
      class(file_collection_t), intent(in) :: self
      character(len=*), intent(in) :: path
      slot = int(iand(fnv_1a(path), int(size(self%slots)-1, int64))) + 1
      do while (self%slots(slot) /= 0)
         if (self%paths(self%slots(slot))%s == path) return
         slot = modulo(slot, size(self%slots)) + 1
      end do
   end function collection_slot

   !> Double collection storage and rebuild the path index at a load below one half.
   subroutine collection_grow(self)
      class(file_collection_t), intent(inout) :: self
      type(string_t), allocatable :: paths(:)
      integer(int64), allocatable :: masks(:)
      logical, allocatable :: protected(:)
      type(include_edges_t), allocatable :: includes(:)
      integer :: capacity, i, slot
      capacity = 128
      if (allocated(self%paths)) capacity = 2 * size(self%paths)
      allocate(paths(capacity), masks(capacity), protected(capacity), includes(capacity))
      do i = 1, self%count
         call move_alloc(self%paths(i)%s, paths(i)%s)
         masks(i) = self%masks(i)
         protected(i) = self%protected(i)
         includes(i)%count = self%includes(i)%count
         includes(i)%parsed = self%includes(i)%parsed
         call move_alloc(self%includes(i)%children, includes(i)%children)
      end do
      call move_alloc(protected, self%protected)
      call move_alloc(paths, self%paths)
      call move_alloc(masks, self%masks)
      call move_alloc(includes, self%includes)
      if (allocated(self%slots)) deallocate(self%slots)
      allocate(self%slots(2*capacity), source=0)
      do i = 1, self%count
         slot = collection_slot(self, self%paths(i)%s)
         self%slots(slot) = i
      end do
   end subroutine collection_grow

   !> Transfer aligned arrays, exporting global metadata independently of include membership.
   !!
   !! Keep internal root bits until traversal is complete so metadata can also
   !! propagate include dependencies and terminate cycles. Its exported zero mask
   !! makes any metadata change select the full command, even after later propagation.
   subroutine collection_finish(self, files, masks, protected)
      class(file_collection_t), intent(inout) :: self
      type(string_t), allocatable, intent(out) :: files(:)
      integer(int64), allocatable, intent(out) :: masks(:)
      logical, allocatable, intent(out) :: protected(:)
      integer :: i
      allocate(files(self%count), masks(self%count), protected(self%count))
      do i = 1, self%count
         call move_alloc(self%paths(i)%s, files(i)%s)
         masks(i) = self%masks(i)
         if (self%protected(i)) masks(i) = 0_int64
         protected(i) = self%protected(i)
      end do
      if (allocated(self%paths)) deallocate(self%paths, self%masks, self%slots, self%protected, self%includes)
      self%count = 0
   end subroutine collection_finish

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
            if (is_program_in_scope(targets(i), scope)) keep(i) = .true.
         end do
         return
      end if

      do i = 1, size(targets)
         if (.not. is_program_in_scope(targets(i), scope)) cycle
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
      integer(int64) :: new_mask

      idx = find_ptr_index(node, all)
      if (idx <= 0) return
      new_mask = iand(mask, not(tmask(idx)))
      if (new_mask == 0_int64) return
      tmask(idx) = ior(tmask(idx), new_mask)

      if (allocated(node%dependencies)) then
         do i = 1, size(node%dependencies)
            if (associated(node%dependencies(i)%ptr)) call dfs_mark_mask(node%dependencies(i)%ptr, all, tmask, new_mask)
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

   !> Recover a watcher-owned node index from its stable ID in constant time.
   integer function find_ptr_index(p, all) result(idx)
      use fpm_targets, only: build_target_t
      type(build_target_t), pointer, intent(in) :: p
      type(build_target_ptr), intent(in) :: all(:)
      idx = p%schedule
      if (idx < 1 .or. idx > size(all)) then
         idx = 0
      else if (.not. associated(all(idx)%ptr, p)) then
         idx = 0
      end if
   end function find_ptr_index

end module watch_fpm_graph
