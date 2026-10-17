!> Check complete byte coverage, file states, and bounded hash arithmetic.
program check_fingerprint
   use, intrinsic :: iso_fortran_env, only: int64
   use watch_fingerprint, only: file_fingerprint, init_fingerprints, scan_changes, accept_changes
   use fpm_filesystem, only: mkdir
   use fpm_strings, only: string_t
   implicit none
   character(len=*), parameter :: path = "build/watch-fingerprint-tests/content.bin"
   integer, parameter :: positions(*) = [5000, 65536, 65537, 131072, 196731]
   character(len=:), allocatable :: content
   type(string_t), allocatable :: files(:)
   integer(int64), allocatable :: previous(:), current(:)
   integer, allocatable :: changed(:)
   integer(int64) :: missing, empty, baseline
   integer :: unit, i, count
   logical :: exists

   call mkdir("build/watch-fingerprint-tests", echo=.false.)
   inquire(file=path, exist=exists)
   if (exists) then
      open(newunit=unit, file=path, status="old")
      close(unit, status="delete")
   end if
   missing = file_fingerprint(path)
   open(newunit=unit, file=path, access="stream", form="unformatted", status="new")
   close(unit)
   empty = file_fingerprint(path)
   if (empty == missing) error stop "empty and missing fingerprints collide"

   files = [string_t(path)]
   call init_fingerprints(files, previous, current, changed)
   open(newunit=unit, file=path, status="old")
   close(unit, status="delete")
   call scan_changes(files, previous, current, changed, count)
   if (count /= 1) error stop "empty file deletion was missed"
   call accept_changes(previous, current, changed, count)
   open(newunit=unit, file=path, access="stream", form="unformatted", status="new")
   close(unit)
   call scan_changes(files, previous, current, changed, count)
   if (count /= 1) error stop "empty file creation was missed"

   content = repeat("a", 3 * 65536 + 123)
   open(newunit=unit, file=path, access="stream", form="unformatted", status="replace")
   write(unit) content
   close(unit)
   baseline = file_fingerprint(path)
   if (baseline >= 0_int64) error stop "nonempty hash overlaps reserved states"
   do i = 1, size(positions)
      open(newunit=unit, file=path, access="stream", form="unformatted", status="old")
      write(unit, pos=positions(i)) "b"
      close(unit)
      if (file_fingerprint(path) == baseline) error stop "same-length byte edit was missed"
      open(newunit=unit, file=path, access="stream", form="unformatted", status="old")
      write(unit, pos=positions(i)) "a"
      close(unit)
      if (file_fingerprint(path) /= baseline) error stop "restored contents hash differently"
   end do

   open(newunit=unit, file=path, access="stream", form="unformatted", status="replace")
   write(unit) "abc"
   close(unit)
   if (file_fingerprint(path) /= -2204510569963675907_int64) error stop "bounded FNV arithmetic differs"
   open(newunit=unit, file=path, status="old")
   close(unit, status="delete")
   print *, "Full-content fingerprint checks passed"
end program check_fingerprint
