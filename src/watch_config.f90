!> Project configuration and defaults.
!!
!! Reads watcher configuration from:
!! - Built-in defaults (`set_watch_defaults`)
!! - `fpm.toml` under `[extra.fpm-watch]` (`apply_watch_from_manifest`)
!!
!! Also provides helper routines used by the supervisor (`watch_restart`) for
!! auto-restart defaults (delay, max restarts, self path).
module watch_config
   use watch_types, only: watch_opts_t
   use fpm_strings, only: string_t
   use fpm_filesystem, only: exists
   use fpm_error, only: error_t
   use fpm_toml, only: read_package_file, get_list
   use tomlf, only: toml_table, toml_stat, get_value
   implicit none
   private
   public set_watch_defaults, apply_watch_from_manifest, normalize_watch_opts, push_feature, get_restart_defaults

contains

   !> Apply built-in defaults to watcher options.
   !!
   !! This routine sets conservative defaults intended to work well for most
   !! projects, and ensures that list fields are always allocated (possibly
   !! with size zero).
   subroutine set_watch_defaults(w)
      type(watch_opts_t), intent(inout) :: w
      w%verbosity = 0
      w%debug = .false.
      w%poll = 0.5
      w%debounce = 0.2
      w%rescan = 0.0
      w%run_on_start = .true.
      w%print_files_once = .false.
      w%silent_fpm = .false.
      w%watch_deps = .false.
      w%low_cpu = .false.
      if (.not. allocated(w%ignore)) allocate(w%ignore(0))
      if (.not. allocated(w%include)) allocate(w%include(0))
      if (.not. allocated(w%enabled_features)) allocate(w%enabled_features(0))
   end subroutine set_watch_defaults

   !> Normalize watcher options and clamp to safe bounds.
   !!
   !! Clamping prevents pathological values (e.g., negative times, too-small poll
   !! intervals) from degrading performance or usability.
   subroutine normalize_watch_opts(w)
      type(watch_opts_t), intent(inout) :: w
      if (w%poll < 0.05) w%poll = 0.05
      if (w%debounce < 0.0) w%debounce = 0.0
      if (w%rescan < 0.0) w%rescan = 0.0
      if (w%verbosity > 2) w%verbosity = 2
      if (w%verbosity < -1) w%verbosity = -1
   end subroutine normalize_watch_opts

   !> Apply configuration overrides from `fpm.toml` `[extra.fpm-watch]`.
   !!
   !! Missing keys are ignored; successfully read values override existing
   !! defaults.
   subroutine apply_watch_from_manifest(w)
      type(watch_opts_t), intent(inout) :: w

      type(toml_table), allocatable :: root
      type(toml_table), pointer :: wt
      logical :: ok

      call load_watch_table(root, wt, ok)
      if (.not. ok) return

      call toml_get_real(wt,    "poll",     w%poll)
      call toml_get_real(wt,    "debounce", w%debounce)
      call toml_get_real(wt,    "rescan",   w%rescan)

      call toml_get_logical(wt, "debug",        w%debug)
      call toml_get_logical(wt, "print-files",  w%print_files_once)
      call toml_get_logical(wt, "silent-fpm",   w%silent_fpm)
      call toml_get_logical(wt, "run-on-start", w%run_on_start)

      call toml_get_logical(wt, "deps",         w%watch_deps)
      call toml_get_logical(wt, "low-cpu",      w%low_cpu)

      call toml_get_int(wt,     "verbosity", w%verbosity)

      call toml_get_list_strings(wt, "ignore",   w%ignore)
      call toml_get_list_strings(wt, "include",  w%include)
      call toml_get_list_strings(wt, "features", w%enabled_features)

      call normalize_watch_opts(w)
   end subroutine apply_watch_from_manifest

   !> Read supervisor defaults from the manifest (when present).
   !!
   !! These are used by `watch_restart` and can be configured under
   !! `[extra.fpm-watch]`.
   !!
   !! Keys:
   !! - `auto-restart` (logical)
   !! - `restart-delay` (real, seconds)
   !! - `restart-max` (integer, 0 = unlimited)
   !! - `self` or `self-exe` (string, explicit path to `fpm-watch`)
   subroutine get_restart_defaults(auto_restart, restart_delay, restart_max, self_exe)
      logical, intent(out) :: auto_restart
      real, intent(out) :: restart_delay
      integer, intent(out) :: restart_max
      character(len=:), allocatable, intent(out) :: self_exe

      type(toml_table), allocatable :: root
      type(toml_table), pointer :: wt
      logical :: ok
      character(len=:), allocatable :: tmp

      auto_restart  = .false.
      restart_delay = 1.0
      restart_max   = 0
      self_exe      = ""

      call load_watch_table(root, wt, ok)
      if (.not. ok) return

      call toml_get_logical(wt, "auto-restart",  auto_restart)
      call toml_get_real(wt,    "restart-delay", restart_delay)
      call toml_get_int(wt,     "restart-max",   restart_max)

      tmp = ""
      call toml_get_string(wt, "self", tmp)
      if (len_trim(tmp) == 0) call toml_get_string(wt, "self-exe", tmp)
      if (len_trim(tmp) > 0) self_exe = trim(tmp)

      if (restart_delay < 0.0) restart_delay = 0.0
      if (restart_max < 0) restart_max = 0
   end subroutine get_restart_defaults

   !> Locate and return the `[extra.fpm-watch]` table from `fpm.toml`.
   !!
   !! Returns `ok=.false.` when:
   !! - `fpm.toml` is missing,
   !! - the TOML cannot be read,
   !! - the `extra` table is absent,
   !! - the `fpm-watch` table is absent.
   subroutine load_watch_table(root, wt, ok)
      type(toml_table), allocatable, intent(out) :: root
      type(toml_table), pointer, intent(out) :: wt
      logical, intent(out) :: ok

      type(toml_table), pointer :: extra
      type(error_t), allocatable :: err
      integer :: stat

      ok = .false.
      wt => null()
      extra => null()

      if (.not. exists("fpm.toml")) return

      call read_package_file(root, "fpm.toml", err)
      if (allocated(err)) return

      call get_value(root, "extra", extra, requested=.false., stat=stat)
      if (stat /= toml_stat%success) return
      if (.not. associated(extra)) return

      call get_value(extra, "fpm-watch", wt, requested=.false., stat=stat)
      if (stat /= toml_stat%success) return
      if (.not. associated(wt)) return

      ok = .true.
   end subroutine load_watch_table

   !> Read a TOML real value if present.
   subroutine toml_get_real(table, key, v)
      type(toml_table), intent(inout) :: table
      character(len=*), intent(in) :: key
      real, intent(inout) :: v
      real :: tmp
      integer :: stat
      tmp = v
      call get_value(table, key, tmp, stat=stat)
      if (stat == toml_stat%success) v = tmp
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

   !> Append a feature name to a `string_t` array if not already present.
   !!
   !! This is used both for plugin feature names and for `ignore/include` patterns.
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
