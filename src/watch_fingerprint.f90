!> Sample file contents and compare watch-list snapshots.
!!
!! Fingerprints mix file size with FNV-style samples of at most three 4096-byte
!! blocks: head, middle, and tail. Same-size edits outside the sampled regions
!! can be missed. Fingerprints are change-detection heuristics, not content IDs.
!! File reads share a reusable module buffer and are intended for serial use.
module watch_fingerprint
   use, intrinsic :: iso_fortran_env, only: int64
   use fpm_strings, only: string_t
   use fpm_filesystem, only: basename
   use fpm_command_line, only: fpm_build_settings
   implicit none
   private
   public init_fingerprints, scan_changes, accept_changes, manifest_key_from_files, file_fingerprint, watch_state_changed

   integer(int64), parameter :: FNV_OFFSET = 1469598103934665603_int64
   integer(int64), parameter :: FNV_PRIME  = 1099511628211_int64
   integer, parameter :: BLOCK_SIZE = 4096

   !> Shared 4096-byte scratch buffer; file fingerprinting is not reentrant.
   character(len=1), allocatable, save :: io_buf(:)

contains

   !> Compare unique path/fingerprint snapshots independently of file ordering.
   !!
   !! Unallocated path lists count as empty. Added/removed paths, changed
   !! fingerprints, or missing/mismatched fingerprint arrays for nonempty lists
   !! report a change. Two empty snapshots compare equal.
   pure logical function watch_state_changed(old_files, old_fp, new_files, new_fp) result(changed)
      type(string_t), allocatable, intent(in) :: old_files(:), new_files(:)
      integer(int64), allocatable, intent(in) :: old_fp(:), new_fp(:)
      integer :: nold, nnew, i, j
      logical :: found

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

      do i = 1, nnew
         found = .false.
         do j = 1, nold
            if (new_files(i)%s /= old_files(j)%s) cycle
            if (new_fp(i) /= old_fp(j)) return
            found = .true.
            exit
         end do
         if (.not. found) return
      end do
      changed = .false.
   end function watch_state_changed

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

   !> Resample files and return 1-based indices differing from the accepted baseline.
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

   !> Copy sampled fingerprints into the baseline for valid changed indices.
   !!
   !! The engine accepts ordinary edits after command completion even if fpm
   !! failed, and accepts failed manifest-refresh samples while retaining a retry
   !! flag. Invalid indices are skipped; both fingerprint arrays must be aligned.
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

   !> Mix selected fpm settings and watched manifest paths/fingerprints into a key.
   !!
   !! Includes build directory, toolchain, profile, compile/link flags, pruning,
   !! build-tests, and fpm features. File and feature ordering affects the key.
   !! The engine compares keys for plugin notifications after successful manifest
   !! refreshes; this key does not trigger the refresh itself.
   integer(int64) function manifest_key_from_files(settings, files) result(k)
      class(fpm_build_settings), intent(in) :: settings
      type(string_t), allocatable, intent(in) :: files(:)

      integer :: i
      character(len=:), allocatable :: b

      k = FNV_OFFSET

      call mix_alloc(k, settings%build_dir)
      call mix_alloc(k, settings%compiler)
      call mix_alloc(k, settings%c_compiler)
      call mix_alloc(k, settings%cxx_compiler)
      call mix_alloc(k, settings%archiver)
      call mix_alloc(k, settings%profile)
      call mix_alloc(k, settings%flag)
      call mix_alloc(k, settings%cflag)
      call mix_alloc(k, settings%cxxflag)
      call mix_alloc(k, settings%ldflag)

      if (settings%prune) then
         call fnv1a_mix_str(k, "prune=1")
      else
         call fnv1a_mix_str(k, "prune=0")
      end if

      if (settings%build_tests) then
         call fnv1a_mix_str(k, "build_tests=1")
      else
         call fnv1a_mix_str(k, "build_tests=0")
      end if

      if (allocated(settings%features)) then
         do i = 1, size(settings%features)
            call fnv1a_mix_str(k, trim(settings%features(i)%s))
         end do
      end if

      if (.not. allocated(files)) return
      do i = 1, size(files)
         b = basename(files(i)%s)
         if (b == "fpm.toml") then
            call fnv1a_mix_str(k, files(i)%s)
            call fnv1a_mix_i64(k, file_fingerprint(files(i)%s))
         end if
      end do

   contains

      !> Mix a right-trimmed, allocated, nonempty string into the current hash state.
      subroutine mix_alloc(h, s)
         integer(int64), intent(inout) :: h
         character(len=:), allocatable, intent(in) :: s
         if (.not. allocated(s)) return
         if (len_trim(s) == 0) return
         call fnv1a_mix_str(h, trim(s))
      end subroutine mix_alloc

   end function manifest_key_from_files

   !> Mix file size with up to three sampled blocks of at most 4096 bytes each.
   !!
   !! Missing files, failed size inquiries, and nonpositive sizes return zero.
   !! If opening fails, return a size-only value mixed with the initial hash;
   !! failed block reads contribute nothing. Larger files sample head, middle,
   !! and tail, which may overlap. Uses the shared module scratch buffer.
   integer(int64) function file_fingerprint(path) result(fp)
      character(len=*), intent(in) :: path
      !! File path to sample; trailing blanks are removed before inquiry/open.
      logical :: ex
      integer(int64) :: sz, h, n64, pos_mid, pos_end, pos_max
      integer :: u, ios, n

      inquire(file=trim(path), exist=ex, size=sz, iostat=ios)
      if (ios /= 0 .or. .not. ex) then
         fp = 0_int64
         return
      end if
      if (sz <= 0_int64) then
         fp = 0_int64
         return
      end if

      n64 = min(sz, int(BLOCK_SIZE, int64))
      n = int(n64)

      open(newunit=u, file=trim(path), access="stream", form="unformatted", action="read", status="old", iostat=ios)
      if (ios /= 0) then
         fp = ieor(ishft(sz, 1), FNV_OFFSET)
         return
      end if

      if (.not. allocated(io_buf)) allocate(io_buf(BLOCK_SIZE))

      h = FNV_OFFSET
      call hash_block(u, 1_int64, n, h)

      if (sz > n64) then
         pos_max = max(1_int64, sz - n64 + 1_int64)

         pos_mid = sz / 2_int64 - n64 / 2_int64
         if (pos_mid < 1_int64) pos_mid = 1_int64
         if (pos_mid > pos_max) pos_mid = pos_max

         pos_end = pos_max

         call hash_block(u, pos_mid, n, h)
         call hash_block(u, pos_end, n, h)
      end if

      close(u)

      fp = ieor(ishft(sz, 1), h)
   end function file_fingerprint

   !> Mix bytes from a 1-based stream position; skip the block if its read fails.
   subroutine hash_block(u, pos, n, h)
      integer, intent(in) :: u
      integer(int64), intent(in) :: pos
      integer, intent(in) :: n
      integer(int64), intent(inout) :: h

      integer :: ios, i

      if (n <= 0) return
      read(u, pos=pos, iostat=ios) io_buf(1:n)
      if (ios == 0) then
         do i = 1, n
            call fnv1a_mix_i64(h, int(iachar(io_buf(i)), int64))
         end do
      end if
   end subroutine hash_block

   !> Mix each byte of a right-trimmed string using XOR and the FNV prime.
   subroutine fnv1a_mix_str(h, s)
      integer(int64), intent(inout) :: h
      character(len=*), intent(in)  :: s
      integer :: i
      do i = 1, len_trim(s)
         call fnv1a_mix_i64(h, int(iachar(s(i:i)), int64))
      end do
   end subroutine fnv1a_mix_str

   !> XOR one integer into the hash state, then multiply by the FNV prime.
   subroutine fnv1a_mix_i64(h, x)
      integer(int64), intent(inout) :: h
      integer(int64), intent(in)    :: x
      h = ieor(h, x)
      h = h * FNV_PRIME
   end subroutine fnv1a_mix_i64

end module watch_fingerprint
