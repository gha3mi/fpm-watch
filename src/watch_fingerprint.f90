!> Lightweight file fingerprinting for change detection.
!!
!! Uses a size-mixed FNV-1a hash of up to three fixed-size blocks from the file
!! (head, mid, tail) to efficiently detect modifications.
!!
!! This fingerprint is intended as a fast heuristic:
!! - It is not a cryptographic hash.
!! - It trades perfect detection for performance on large trees.
!! - Combining size + multiple sample blocks provides good practical coverage.
module watch_fingerprint
   use, intrinsic :: iso_fortran_env, only: int64
   use fpm_strings, only: string_t
   use fpm_filesystem, only: basename
   use fpm_command_line, only: fpm_build_settings
   implicit none
   private
   public init_fingerprints, scan_changes, accept_changes, manifest_key_from_files, file_fingerprint

   integer(int64), parameter :: FNV_OFFSET = 1469598103934665603_int64
   integer(int64), parameter :: FNV_PRIME  = 1099511628211_int64
   integer, parameter :: BLOCK_SIZE = 4096

   character(len=1), allocatable, save :: io_buf(:)

contains

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

   !> Scan files and return indices of changed entries.
   !!
   !! `changed_idx(1:changed_count)` contains the 1-based indices into `files`
   !! for which the fingerprint differs from `fp_prev`.
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

   !> Accept the new fingerprints for the subset of changed indices.
   !!
   !! After a successful run, this updates `fp_prev` so future scans detect new
   !! changes relative to the latest known state.
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

   !> Compute a key representing the current manifest-related configuration.
   !!
   !! This key is used to detect when changes require rebuilding the watch list.
   !! It includes:
   !! - Selected `fpm_build_settings` fields
   !! - Feature list (`settings%features`)
   !! - The fingerprint of any watched file named `fpm.toml`
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

      subroutine mix_alloc(h, s)
         integer(int64), intent(inout) :: h
         character(len=:), allocatable, intent(in) :: s
         if (.not. allocated(s)) return
         if (len_trim(s) == 0) return
         call fnv1a_mix_str(h, trim(s))
      end subroutine mix_alloc

   end function manifest_key_from_files

   !> Compute a fingerprint for a file path.
   !!
   !! Strategy:
   !! - Read the file size (`inquire(size=...)`).
   !! - Hash up to three blocks (head/mid/tail) using FNV-1a mixing.
   !! - Combine with file size to reduce collision risk.
   !!
   !! Missing/unreadable files return `0`.
   integer(int64) function file_fingerprint(path) result(fp)
      character(len=*), intent(in) :: path
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

   !> Hash a single file block at a specific stream position.
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

   !> Mix a string into a 64-bit FNV-1a state.
   subroutine fnv1a_mix_str(h, s)
      integer(int64), intent(inout) :: h
      character(len=*), intent(in)  :: s
      integer :: i
      do i = 1, len_trim(s)
         call fnv1a_mix_i64(h, int(iachar(s(i:i)), int64))
      end do
   end subroutine fnv1a_mix_str

   !> Mix a single 64-bit value into a 64-bit FNV-1a state.
   subroutine fnv1a_mix_i64(h, x)
      integer(int64), intent(inout) :: h
      integer(int64), intent(in)    :: x
      h = ieor(h, x)
      h = h * FNV_PRIME
   end subroutine fnv1a_mix_i64

end module watch_fingerprint
