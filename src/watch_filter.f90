!> Normalize paths and filter the computed watch list with fpm globs.
!!
!! Normalization changes backslashes to `/` and removes leading `./` segments;
!! it does not canonicalize paths. Build-directory exclusion is applied by the
!! graph collector. Include/ignore filters always retain watched `fpm.toml` files.
module watch_filter
   use watch_util, only: starts_with
   use, intrinsic :: iso_fortran_env, only: int64
   use fpm_strings, only: string_t, glob
   use fpm_filesystem, only: basename
   implicit none
   private
   public normalize_path, starts_with, filter_watch_files, is_ignored_path

contains

   !> Right-trim a path, replace backslashes, and remove repeated leading `./`.
   !!
   !! Preserve case, interior separators, and `..` segments; no filesystem lookup
   !! or canonicalization is performed.
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


   !> Compare right-trimmed suffixes; an empty suffix matches every string.
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

   !> Test for a slash-delimited interior or trailing path fragment.
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

   !> Test normalized paths against a build directory or its slash-delimited fragments.
   !!
   !! Matches the directory itself, its descendants, and interior/trailing path
   !! fragments. An empty build directory matches nothing; dependency exceptions
   !! are applied by the graph collector.
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

   !> Filter normalized paths and optional aligned masks, preserving order.
   !!
   !! Empty/unallocated include lists admit all files; matching ignore globs
   !! remove included paths. A basename of `fpm.toml` bypasses both filters.
   !! An unallocated file list is left unchanged. If masks are supplied but
   !! unallocated, create zero masks before filtering them with the files.
   subroutine filter_watch_files(files, include, ignore, masks)
      type(string_t), allocatable, intent(inout) :: files(:)
      !! Normalized input paths, replaced with the retained subset in the same order.
      type(string_t), allocatable, intent(in)    :: include(:), ignore(:)
      !! Include and ignore glob lists, respectively; empty/unallocated lists impose no restriction.
      integer(int64), allocatable, intent(inout), optional :: masks(:)
      !! Aligned file masks; created as zeros if supplied but unallocated, then filtered with files.

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

      !> Retain manifests unconditionally; otherwise require inclusion and no ignore match.
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

      !> Test fpm globs after normalizing each nonempty pattern; an absent list matches nothing.
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
            if (glob(trim(path), pat)) then
               ok = .true.
               return
            end if
         end do
      end function match_any

      !> Admit every path for an empty/unallocated include list; otherwise require a glob match.
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
