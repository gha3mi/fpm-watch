!> Watch list filtering and path normalization.
!!
!! Contains helpers to normalize paths and apply include/ignore globs to the
!! computed set of watched files.
!!
!! Key behaviors:
!! - Paths are normalized to use `/` separators.
!! - Leading `./` segments are removed for consistent matching.
!! - Build directory filtering is handled elsewhere (but helper exists here).
module watch_filter
   use, intrinsic :: iso_fortran_env, only: int64
   use fpm_strings, only: string_t, glob
   use fpm_filesystem, only: basename
   implicit none
   private
   public normalize_path, starts_with, filter_watch_files, is_ignored_path

contains

   !> Normalize a path to a consistent form for matching.
   !!
   !! Transformations:
   !! - Backslashes (`\`) → slashes (`/`)
   !! - Strip any leading `./` prefixes (repeatedly)
   pure function normalize_path(path) result(p)
      character(len=*), intent(in) :: path
      character(len=:), allocatable :: p
      integer :: i
      p = trim(path)
      do i = 1, len(p)
         if (p(i:i) == char(92)) p(i:i) = "/"
      end do
      do while (len(p) >= 2)
         if (p(1:2) == "./") then
            p = p(3:)
         else
            exit
         end if
      end do
   end function normalize_path

   !> Check whether a string begins with a given prefix.
   pure logical function starts_with(s, prefix) result(ok)
      character(len=*), intent(in) :: s, prefix
      integer :: n
      n = len_trim(prefix)
      if (n <= 0) then
         ok = .true.
      else if (len_trim(s) < n) then
         ok = .false.
      else
         ok = (s(1:n) == prefix(1:n))
      end if
   end function starts_with

   pure logical function ends_with(s, suffix) result(ok)
      character(len=*), intent(in) :: s, suffix
      integer :: ns, n
      ns = len_trim(suffix)
      n = len_trim(s)
      if (ns <= 0) then
         ok = .true.
      else if (n < ns) then
         ok = .false.
      else
         ok = (s(n-ns+1:n) == suffix(1:ns))
      end if
   end function ends_with

   pure logical function contains_path_fragment(p, frag) result(ok)
      character(len=*), intent(in) :: p, frag
      character(len=:), allocatable :: needle
      ok = .false.
      if (len_trim(frag) == 0) return
      needle = "/" // trim(frag) // "/"
      if (index(p, needle) /= 0) then
         ok = .true.
         return
      end if
      if (ends_with(p, "/" // trim(frag))) then
         ok = .true.
         return
      end if
   end function contains_path_fragment

   !> Identify paths that should be ignored due to being inside the build directory.
   !!
   !! This is used to prevent watch loops from triggering rebuilds due to build
   !! artifacts changing. When dependency watching is enabled, callers may
   !! selectively override this behavior.
   pure logical function is_ignored_path(path, build_dir) result(ignored)
      character(len=*), intent(in) :: path, build_dir
      character(len=:), allocatable :: p, b

      p = normalize_path(path)
      b = normalize_path(build_dir)

      ignored = .false.

      if (len_trim(b) == 0) return

      if (p == b) then
         ignored = .true.
         return
      end if

      if (starts_with(p, b // "/")) then
         ignored = .true.
         return
      end if

      if (contains_path_fragment(p, b)) then
         ignored = .true.
         return
      end if
   end function is_ignored_path

   !> Apply include/ignore rules to the watch list.
   !!
   !! - If `include` patterns are empty, all files are included by default.
   !! - If `include` patterns are present, only matching files are kept.
   !! - If `ignore` patterns match a file, it is removed even if included.
   !!
   !! The `masks` array (when present) is filtered in sync with `files`.
   subroutine filter_watch_files(files, include, ignore, masks)
      type(string_t), allocatable, intent(inout) :: files(:)
      type(string_t), allocatable, intent(in)    :: include(:), ignore(:)
      integer(int64), allocatable, intent(inout), optional :: masks(:)

      logical, allocatable :: keep(:)
      type(string_t), allocatable :: out(:)
      integer(int64), allocatable :: outm(:)
      integer :: i, k, n, m

      if (.not. allocated(files)) return

      n = size(files)
      allocate(keep(n))
      do i = 1, n
         keep(i) = should_keep(files(i)%s, include, ignore)
      end do

      m = count(keep)
      allocate(out(m))
      if (present(masks)) then
         if (.not. allocated(masks)) then
            allocate(masks(n))
            masks = 0_int64
         end if
         allocate(outm(m))
      end if

      k = 0
      do i = 1, n
         if (.not. keep(i)) cycle
         k = k + 1
         out(k)%s = files(i)%s
         if (present(masks)) outm(k) = masks(i)
      end do

      call move_alloc(out, files)
      if (present(masks)) call move_alloc(outm, masks)
      deallocate(keep)

   contains

      logical function should_keep(path, include_pats, ignore_pats) result(ok)
         character(len=*), intent(in) :: path
         type(string_t), allocatable, intent(in) :: include_pats(:), ignore_pats(:)
         character(len=:), allocatable :: b
         b = basename(trim(path))
         if (b == "fpm.toml") then
            ok = .true.
            return
         end if
         ok = match_include(path, include_pats) .and. (.not. match_any(path, ignore_pats))
      end function should_keep

      logical function match_any(path, pats) result(ok)
         character(len=*), intent(in) :: path
         type(string_t), allocatable, intent(in) :: pats(:)
         integer :: j
         character(len=:), allocatable :: pat
         ok = .false.
         if (.not. allocated(pats)) return
         do j = 1, size(pats)
            if (len_trim(pats(j)%s) == 0) cycle
            pat = normalize_path(trim(pats(j)%s))
            if (glob(pat, trim(path))) then
               ok = .true.
               return
            end if
         end do
      end function match_any

      logical function match_include(path, pats) result(ok)
         character(len=*), intent(in) :: path
         type(string_t), allocatable, intent(in) :: pats(:)
         if (.not. allocated(pats)) then
            ok = .true.
            return
         end if
         if (size(pats) == 0) then
            ok = .true.
            return
         end if
         ok = match_any(path, pats)
      end function match_include

   end subroutine filter_watch_files

end module watch_filter
