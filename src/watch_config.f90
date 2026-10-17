!> Read watcher and supervisor defaults through fpm's TOML interface.
!!
!! Missing or unreadable watcher tables leave existing values unchanged.
!! Scalar keys replace defaults; manifest lists replace existing arrays.
!! CLI list options subsequently append unique entries through `push_feature`.
module watch_config
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   use watch_types, only: watch_opts_t
   use fpm_strings, only: string_t
   use fpm_filesystem, only: exists
   use fpm_error, only: error_t
   use fpm_toml, only: read_package_file, get_list
   use tomlf, only: toml_table, toml_stat, get_value
   implicit none
   private
   public set_watch_defaults, apply_watch_from_manifest, normalize_watch_opts, push_feature

contains

   !> Reset scalar watcher options and ensure list arrays are allocated.
   !!
   !! Existing include/ignore arrays are retained. Fresh options receive empty arrays.
   subroutine set_watch_defaults(w)
      type(watch_opts_t), intent(inout) :: w
      w%verbosity = 0
      w%log_format = "text"
      w%fpm_executable = "fpm"
      w%debug = .false.
      w%poll = 0.5
      w%debounce = 0.2
      w%rescan = 0.0
      w%run_on_start = .true.
      w%print_files_once = .false.
      w%silent_fpm = .false.
      w%output_markers = .false.
      w%restart_count = 0
      w%watch_deps = .false.
      w%once = .false.
      if (.not. allocated(w%ignore)) allocate(w%ignore(0))
      if (.not. allocated(w%include)) allocate(w%include(0))
      w%auto_restart = .false.
      w%is_child = .false.
      w%restart_delay = 1.0
      w%restart_max = 0
      w%self_exe = ""
   end subroutine set_watch_defaults

   !> Normalize timing bounds, verbosity, and the executable name.
   !!
   !! Clamp poll to at least 0.05 s, debounce/rescan to nonnegative values, and
   !! verbosity to -1..2. Restore an empty/unallocated fpm executable to `fpm`.
   !! Log-format validation and list allocation are handled separately.
   subroutine normalize_watch_opts(w)
      type(watch_opts_t), intent(inout) :: w
      if (w%poll < 0.05) w%poll = 0.05
      if (w%debounce < 0.0) w%debounce = 0.0
      if (w%rescan < 0.0) w%rescan = 0.0
      if (w%restart_delay < 0.0) w%restart_delay = 0.0
      if (w%restart_max < 0) w%restart_max = 0
      if (w%verbosity > 2) w%verbosity = 2
      if (w%verbosity < -1) w%verbosity = -1
      if (.not. allocated(w%fpm_executable)) w%fpm_executable = "fpm"
      if (len_trim(w%fpm_executable) == 0) w%fpm_executable = "fpm"
   end subroutine normalize_watch_opts

   !> Replace recognized options from `[extra.fpm-watch]` and normalize them.
   !!
   !! Missing keys or values that TOML getters cannot read leave existing options
   !! unchanged. Successfully read lists replace existing arrays. Log-format validation
   !! belongs to CLI configuration assembly.
   subroutine apply_watch_from_manifest(w, manifest)
      type(watch_opts_t), intent(inout) :: w
      character(len=*), intent(in) :: manifest

      type(toml_table), allocatable :: root
      type(toml_table), pointer :: wt
      logical :: ok

      call load_watch_table(root, wt, ok, manifest)
      if (.not. ok) return

      call toml_get_real(wt,    "poll",     w%poll)
      call toml_get_real(wt,    "debounce", w%debounce)
      call toml_get_real(wt,    "rescan",   w%rescan)

      call toml_get_logical(wt, "debug",        w%debug)
      call toml_get_logical(wt, "print-files",  w%print_files_once)
      call toml_get_logical(wt, "silent-fpm",   w%silent_fpm)
      call toml_get_logical(wt, "output-markers", w%output_markers)
      call toml_get_logical(wt, "run-on-start", w%run_on_start)

      call toml_get_logical(wt, "deps",         w%watch_deps)
      call toml_get_logical(wt, "once",         w%once)

      call toml_get_int(wt,     "verbosity", w%verbosity)
      call toml_get_string(wt, "log-format", w%log_format)
      call toml_get_string(wt, "fpm-executable", w%fpm_executable)

      call toml_get_list_strings(wt, "ignore",   w%ignore)
      call toml_get_list_strings(wt, "include",  w%include)
      call toml_get_logical(wt, "auto-restart", w%auto_restart)
      call toml_get_real(wt, "restart-delay", w%restart_delay)
      call toml_get_int(wt, "restart-max", w%restart_max)
      call toml_get_string(wt, "self", w%self_exe)

      call normalize_watch_opts(w)
   end subroutine apply_watch_from_manifest

   !> Locate `[extra.fpm-watch]` in the selected project manifest.
   !!
   !! `ok` is false and `wt` is null when the file/table is absent or unreadable.
   !! On success, `wt` points inside `root` and remains valid only while `root`
   !! remains allocated.
   subroutine load_watch_table(root, wt, ok, manifest)
      type(toml_table), allocatable, intent(out) :: root
      type(toml_table), pointer, intent(out) :: wt
      logical, intent(out) :: ok
      character(len=*), intent(in) :: manifest

      type(toml_table), pointer :: extra
      type(error_t), allocatable :: err
      integer :: stat

      ok = .false.
      wt => null()
      extra => null()

      if (.not. exists(manifest)) return

      call read_package_file(root, manifest, err)
      if (allocated(err)) return

      call get_value(root, "extra", extra, requested=.false., stat=stat)
      if (stat /= toml_stat%success) return
      if (.not. associated(extra)) return

      call get_value(extra, "fpm-watch", wt, requested=.false., stat=stat)
      if (stat /= toml_stat%success) return
      if (.not. associated(wt)) return

      ok = .true.
   end subroutine load_watch_table

   !> Read a finite TOML real value if present; otherwise retain the existing value.
   subroutine toml_get_real(table, key, v)
      type(toml_table), intent(inout) :: table
      character(len=*), intent(in) :: key
      real, intent(inout) :: v
      real :: tmp
      integer :: stat
      tmp = v
      call get_value(table, key, tmp, stat=stat)
      if (stat == toml_stat%success) then
         if (ieee_is_finite(tmp)) v = tmp
      end if
   end subroutine toml_get_real

   !> Read a TOML integer value if present.
   subroutine toml_get_int(table, key, v)
      type(toml_table), intent(inout) :: table
      character(len=*), intent(in) :: key
      integer, intent(inout) :: v
      integer :: tmp, stat
      tmp = v
      call get_value(table, key, tmp, stat=stat)
      if (stat == toml_stat%success) v = tmp
   end subroutine toml_get_int

   !> Read a TOML logical value if present.
   subroutine toml_get_logical(table, key, v)
      type(toml_table), intent(inout) :: table
      character(len=*), intent(in) :: key
      logical, intent(inout) :: v
      logical :: tmp
      integer :: stat
      tmp = v
      call get_value(table, key, tmp, stat=stat)
      if (stat == toml_stat%success) v = tmp
   end subroutine toml_get_logical

   !> Read a TOML string value if present.
   subroutine toml_get_string(table, key, v)
      type(toml_table), intent(inout) :: table
      character(len=*), intent(in) :: key
      character(len=:), allocatable, intent(inout) :: v
      character(len=:), allocatable :: tmp
      integer :: stat

      call get_value(table, key, tmp, stat=stat)
      if (stat == toml_stat%success) then
         if (allocated(tmp)) then
            v = tmp
         else
            v = ""
         end if
      end if
   end subroutine toml_get_string

   !> Read a TOML string list into an allocatable `string_t` array.
   subroutine toml_get_list_strings(table, key, list)
      type(toml_table), intent(inout) :: table
      character(len=*), intent(in) :: key
      type(string_t), allocatable, intent(inout) :: list(:)
      type(error_t), allocatable :: err
      type(string_t), allocatable :: tmp(:)

      if (.not. allocated(list)) allocate(list(0))

      call get_list(table, key, tmp, err)
      if (allocated(err)) then
         if (allocated(tmp)) deallocate(tmp)
         return
      end if
      if (.not. allocated(tmp)) return

      call move_alloc(tmp, list)
   end subroutine toml_get_list_strings

   !> Append a unique, nonempty, right-trimmed string to an allocatable list.
   !!
   !! Used for fpm features, target patterns, and filters.
   !! Comparisons are case-sensitive; leading whitespace is retained.
   subroutine push_feature(features, name)
      type(string_t), allocatable, intent(inout) :: features(:)
      character(len=*), intent(in) :: name
      integer :: n, i
      type(string_t), allocatable :: tmp(:)

      if (len_trim(name) == 0) return

      if (.not. allocated(features)) then
         allocate(features(1))
         features(1)%s = trim(name)
         return
      end if

      do i = 1, size(features)
         if (trim(features(i)%s) == trim(name)) return
      end do

      n = size(features)
      allocate(tmp(n+1))
      tmp(1:n) = features
      tmp(n+1)%s = trim(name)
      call move_alloc(tmp, features)
   end subroutine push_feature

end module watch_config
