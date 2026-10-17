!> Stream complete file contents and compare watch-list snapshots.
!!
!! Every byte contributes to the fingerprint. Missing, empty, and unreadable files
!! have distinct states; nonempty content uses a 63-bit hash. Hash collisions are
!! still possible, so fingerprints are change detectors, not content identities.
!! File reads share a reusable buffer and are intended for serial use.
module watch_fingerprint
   use, intrinsic :: iso_fortran_env, only: int64
   use fpm_strings, only: string_t
   implicit none
   private
   public init_fingerprints, scan_changes, accept_changes, file_fingerprint, watch_state_changed
   public preserve_fingerprints

   integer(int64), parameter :: FNV_OFFSET = 1469598103934665603_int64
   integer(int64), parameter :: MASK32 = 4294967295_int64
   integer, parameter :: BLOCK_SIZE = 65536
   integer(int64), parameter :: FILE_MISSING = 0_int64, FILE_EMPTY = 1_int64, FILE_UNREADABLE = 2_int64

   !> Shared bounded scratch buffer; file fingerprinting is not reentrant.
   character(len=BLOCK_SIZE), save :: io_buf

contains

   !> Compare unique path/fingerprint snapshots independently of file ordering.
   !!
   !! Unallocated path lists count as empty. Added/removed paths, changed
   !! fingerprints, or missing/mismatched fingerprint arrays for nonempty lists
   !! report a change. Two empty snapshots compare equal. Matching path order
   !! compares in linear time; reordered lists use sorted indices in O(n log n).
   pure logical function watch_state_changed(old_files, old_fp, new_files, new_fp) result(changed)
      type(string_t), allocatable, intent(in) :: old_files(:), new_files(:)
      integer(int64), allocatable, intent(in) :: old_fp(:), new_fp(:)
      integer :: nold, nnew, i
      integer, allocatable :: old_order(:), new_order(:), scratch(:)
      logical :: aligned

      nold = 0
      nnew = 0
      if (allocated(old_files)) nold = size(old_files)
      if (allocated(new_files)) nnew = size(new_files)
      changed = (nold /= nnew)
      if (changed .or. nnew == 0) return

      changed = .true.
      if (.not. allocated(old_fp)) return
      if (.not. allocated(new_fp)) return
      if (size(old_fp) /= nold .or. size(new_fp) /= nnew) return

      aligned = .true.
      do i = 1, nnew
         if (old_files(i)%s == new_files(i)%s) cycle
         aligned = .false.
         exit
      end do
      if (aligned) then
         changed = any(old_fp /= new_fp)
         return
      end if

      allocate(old_order(nold), new_order(nnew), scratch(nnew))
      old_order = [(i, i=1,nold)]
      new_order = old_order
      call sort_path_indices(old_files, old_order, scratch, 1, nold)
      call sort_path_indices(new_files, new_order, scratch, 1, nnew)
      do i = 1, nnew
         if (old_files(old_order(i))%s /= new_files(new_order(i))%s) return
         if (old_fp(old_order(i)) /= new_fp(new_order(i))) return
      end do
      changed = .false.
   end function watch_state_changed

   !> Preserve accepted baselines by path while retaining initial values for new paths.
   !!
   !! Both snapshots contain unique paths. Missing or malformed fingerprint arrays
   !! leave the new baseline unchanged. File ordering and membership may differ;
   !! matching order copies directly, otherwise sorted indices support a linear merge.
   pure subroutine preserve_fingerprints(old_files, old_fp, new_files, new_fp)
      type(string_t), allocatable, intent(in) :: old_files(:), new_files(:)
      integer(int64), allocatable, intent(in) :: old_fp(:)
      integer(int64), allocatable, intent(inout) :: new_fp(:)
      integer, allocatable :: old_order(:), new_order(:), scratch(:)
      integer :: nold, nnew, i, j
      logical :: aligned

      if (.not. allocated(old_files)) return
      if (.not. allocated(new_files)) return
      if (.not. allocated(old_fp)) return
      if (.not. allocated(new_fp)) return
      nold = size(old_files)
      nnew = size(new_files)
      if (nold == 0 .or. nnew == 0) return
      if (size(old_fp) /= nold .or. size(new_fp) /= nnew) return
      aligned = nold == nnew
      if (aligned) then
         do i = 1, nnew
            if (old_files(i)%s == new_files(i)%s) cycle
            aligned = .false.
            exit
         end do
      end if
      if (aligned) then
         new_fp = old_fp
         return
      end if

      allocate(old_order(nold), new_order(nnew), scratch(max(nold, nnew)))
      old_order = [(i, i=1,nold)]
      new_order = [(i, i=1,nnew)]
      call sort_path_indices(old_files, old_order, scratch, 1, nold)
      call sort_path_indices(new_files, new_order, scratch, 1, nnew)
      i = 1
      j = 1
      do while (i <= nold .and. j <= nnew)
         if (old_files(old_order(i))%s < new_files(new_order(j))%s) then
            i = i + 1
         else if (old_files(old_order(i))%s > new_files(new_order(j))%s) then
            j = j + 1
         else
            new_fp(new_order(j)) = old_fp(old_order(i))
            i = i + 1
            j = j + 1
         end if
      end do
   end subroutine preserve_fingerprints

   !> Stable merge sort of path indices; the file storage itself remains unchanged.
   pure recursive subroutine sort_path_indices(files, order, scratch, first, last)
      type(string_t), intent(in) :: files(:)
      integer, intent(inout) :: order(:), scratch(:)
      integer, intent(in) :: first, last
      integer :: middle, left, right, out
      if (first >= last) return
      middle = first + (last - first) / 2
      call sort_path_indices(files, order, scratch, first, middle)
      call sort_path_indices(files, order, scratch, middle + 1, last)
      left = first
      right = middle + 1
      do out = first, last
         if (left > middle) then
            scratch(out) = order(right)
            right = right + 1
         else if (right > last) then
            scratch(out) = order(left)
            left = left + 1
         else if (files(order(left))%s <= files(order(right))%s) then
            scratch(out) = order(left)
            left = left + 1
         else
            scratch(out) = order(right)
            right = right + 1
         end if
      end do
      order(first:last) = scratch(first:last)
   end subroutine sort_path_indices

   !> Initialize fingerprint arrays for a watch list.
   !!
   !! Allocates `fp_prev`, `fp_now`, and `changed_idx` to match the size of
   !! `files`. Initial fingerprints are computed once and copied into `fp_now`.
   subroutine init_fingerprints(files, fp_prev, fp_now, changed_idx)
      type(string_t), allocatable, intent(in) :: files(:)
      integer(int64), allocatable, intent(inout) :: fp_prev(:)
      integer(int64), allocatable, intent(inout) :: fp_now(:)
      integer, allocatable, intent(inout) :: changed_idx(:)
      integer :: n, i

      if (.not. allocated(files)) then
         if (allocated(fp_prev)) deallocate(fp_prev)
         if (allocated(fp_now))  deallocate(fp_now)
         if (allocated(changed_idx)) deallocate(changed_idx)
         allocate(fp_prev(0), fp_now(0), changed_idx(0))
         return
      end if

      n = size(files)

      if (allocated(fp_prev)) deallocate(fp_prev)
      if (allocated(fp_now))  deallocate(fp_now)
      if (allocated(changed_idx)) deallocate(changed_idx)

      allocate(fp_prev(n), fp_now(n), changed_idx(n))

      do i = 1, n
         fp_prev(i) = file_fingerprint(files(i)%s)
      end do
      fp_now = fp_prev
   end subroutine init_fingerprints

   !> Hash files and return 1-based indices differing from the accepted baseline.
   !!
   !! `fp_prev`, `fp_now`, and `changed_idx` must have space for every file.
   !! Only `changed_idx(:changed_count)` is defined by this scan; `fp_prev`
   !! remains unchanged. An empty/unallocated file list reports no changes.
   subroutine scan_changes(files, fp_prev, fp_now, changed_idx, changed_count)
      type(string_t), allocatable, intent(in) :: files(:)
      integer(int64), intent(in) :: fp_prev(:)
      integer(int64), intent(inout) :: fp_now(:)
      integer, intent(inout) :: changed_idx(:)
      integer, intent(out) :: changed_count
      integer :: i, n

      changed_count = 0
      if (.not. allocated(files)) return
      n = size(files)
      if (n == 0) return

      do i = 1, n
         fp_now(i) = file_fingerprint(files(i)%s)
         if (fp_now(i) /= fp_prev(i)) then
            changed_count = changed_count + 1
            changed_idx(changed_count) = i
         end if
      end do
   end subroutine scan_changes

   !> Copy computed fingerprints into the baseline for valid changed indices.
   !!
   !! The engine accepts detected edits before graph refresh and command execution.
   !! Surviving paths retain those baselines through every refresh, so later edits
   !! remain detectable even while commands run. Invalid indices are skipped;
   !! both fingerprint arrays must be aligned.
   subroutine accept_changes(fp_prev, fp_now, changed_idx, changed_count)
      integer(int64), intent(inout) :: fp_prev(:)
      integer(int64), intent(in)    :: fp_now(:)
      integer, intent(in) :: changed_idx(:)
      integer, intent(in) :: changed_count
      integer :: j, i
      do j = 1, changed_count
         i = changed_idx(j)
         if (i >= 1 .and. i <= size(fp_prev)) fp_prev(i) = fp_now(i)
      end do
   end subroutine accept_changes

   !> Hash every byte using bounded reads, reserving distinct non-content states.
   !!
   !! Missing files return zero, empty files one, and inquiry/open/read failures
   !! two. Successful nonempty hashes are negative and cannot equal those states.
   !! A size change during reading is treated as unreadable until the next poll.
   integer(int64) function file_fingerprint(path) result(fp)
      character(len=*), intent(in) :: path
      logical :: ex
      integer(int64) :: sz, final_size, remaining, h
      integer :: u, ios, n, i

      fp = FILE_UNREADABLE
      inquire(file=trim(path), exist=ex, iostat=ios)
      if (ios /= 0) return
      if (.not. ex) then
         fp = FILE_MISSING
         return
      end if
      open(newunit=u, file=trim(path), access="stream", form="unformatted", action="read", status="old", iostat=ios)
      if (ios /= 0) return
      sz = -1_int64
      inquire(unit=u, size=sz, iostat=ios)
      if (ios /= 0 .or. sz < 0_int64) then
         close(u)
         return
      end if
      remaining = sz
      h = FNV_OFFSET
      do while (remaining > 0_int64)
         n = int(min(remaining, int(BLOCK_SIZE, int64)))
         read(u, iostat=ios) io_buf(:n)
         if (ios /= 0) exit
         do i = 1, n
            call fnv1a_mix_i64(h, int(iachar(io_buf(i:i)), int64))
         end do
         remaining = remaining - int(n, int64)
      end do
      if (remaining == 0_int64) then
         inquire(unit=u, size=final_size, iostat=ios)
         if (ios == 0) then
            if (final_size == sz) then
               fp = ibset(h, bit_size(h) - 1)
               if (sz == 0_int64) fp = FILE_EMPTY
            end if
         end if
      end if
      close(u)
   end function file_fingerprint

   !> Mix with the FNV prime modulo 2**64 using bounded 32-bit limbs.
   !!
   !! The prime is 2**40 + 435. Limb products fit signed int64, avoiding
   !! undefined signed overflow while preserving the intended bit pattern.
   pure subroutine fnv1a_mix_i64(h, x)
      integer(int64), intent(inout) :: h
      integer(int64), intent(in)    :: x
      integer(int64) :: low, high, product
      h = ieor(h, x)
      low = iand(h, MASK32)
      high = iand(shiftr(h, 32), MASK32)
      product = low * 435_int64
      high = high * 435_int64 + low * 256_int64 + shiftr(product, 32)
      h = ior(shiftl(iand(high, MASK32), 32), iand(product, MASK32))
   end subroutine fnv1a_mix_i64

end module watch_fingerprint
