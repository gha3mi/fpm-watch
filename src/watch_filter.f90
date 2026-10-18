!> Normalize paths and filter the computed watch list with fpm globs.
!!
!! Normalization changes backslashes to `/` and removes leading `./` segments;
!! it does not canonicalize paths. Build-directory exclusion is applied by the
!! graph collector. Include/ignore filters retain watched manifests and version metadata.
module watch_filter
   use watch_util, only: starts_with
   use, intrinsic :: iso_fortran_env, only: int64
   use fpm_strings, only: string_t, glob, lower
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

   !> Test canonical paths against only the build directory and its descendants.
   !!
   !! Both paths must use the same base: the graph collector resolves them against
   !! the project directory before comparison. A same-named directory elsewhere
   !! is retained. An empty build directory matches nothing. Optional `windows`
   !! enables case-insensitive comparison; dependency exceptions remain in the collector.
   pure logical function is_ignored_path(path, build_dir, windows) result(ignored)
      character(len=*), intent(in) :: path, build_dir
      logical, intent(in), optional :: windows
      character(len=:), allocatable :: p, b

      p = normalize_path(path)
      b = normalize_path(build_dir)

      ignored = .false.
      if (len_trim(b) == 0) return
      if (present(windows)) then
         if (windows) then
            p = lower(p)
            b = lower(b)
         end if
      end if
      do while (len(b) > 1)
         if (b(len(b):) /= "/") exit
         b = b(:len(b)-1)
      end do
      ignored = p == b
      if (b == "/") then
         ignored = ignored .or. starts_with(p, b)
      else
         ignored = ignored .or. starts_with(p, b // "/")
      end if
   end function is_ignored_path

   !> Filter normalized paths and optional aligned masks, preserving order.
   !!
   !! Empty/unallocated include lists admit all files; matching ignore globs
   !! remove included paths. Manifests and optional marked metadata bypass both filters.
   !! An unallocated file list is left unchanged. If masks are supplied but
   !! unallocated, create zero masks before filtering them with the files.
   subroutine filter_watch_files(files, include, ignore, masks, preserve)
      type(string_t), allocatable, intent(inout) :: files(:)
      !! Normalized input paths, replaced with the retained subset in the same order.
      type(string_t), allocatable, intent(in)    :: include(:), ignore(:)
      !! Include and ignore glob lists, respectively; empty/unallocated lists impose no restriction.
      integer(int64), allocatable, intent(inout), optional :: masks(:)
      !! Aligned file masks; created as zeros if supplied but unallocated, then filtered with files.
      logical, intent(in), optional :: preserve(:)
      !! Aligned metadata markers; true entries bypass include/ignore filters.

      logical, allocatable :: keep(:)
      type(string_t), allocatable :: out(:)
      integer(int64), allocatable :: outm(:)
      integer :: i, k, n, m

      if (.not. allocated(files)) return

      n = size(files)
      allocate(keep(n))
      do i = 1, n
         keep(i) = should_keep(files(i)%s, include, ignore)
         if (present(preserve)) keep(i) = keep(i) .or. preserve(i)
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
