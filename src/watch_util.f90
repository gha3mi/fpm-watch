!> Miscellaneous string utilities.
!!
!! Provides small helpers for formatting and manipulating allocatable strings
!! and string arrays.
module watch_util
   use fpm_strings, only: string_t
   implicit none
   private
   public ftoa, trim_or_default, trim_or_empty, join_csv, starts_with

contains

   !> Compare a case-sensitive, right-trimmed prefix; an empty prefix matches every string.
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

   !> Format a real using two decimal places by default and a leading zero.
   !!
   !! Optional `precision` is clamped to 0..16. Fixed output that overflows the
   !! 64-character buffer falls back to scientific notation. Nonfinite values
   !! are not rejected here; typed real logging handles them separately.
   pure function ftoa(x, precision) result(s)
      real, intent(in) :: x
      integer, intent(in), optional :: precision
      character(len=:), allocatable :: s
      character(len=64) :: buf
      character(len=16) :: format
      integer :: digits
      digits = 2
      if (present(precision)) digits = max(0, min(precision, 16))
      write(format, "(a,i0,a)") "(f0.", digits, ")"
      write(buf, format) x
      if (index(buf, "*") > 0) write(buf, "(es24.16e3)") x
      s = trim(adjustl(buf))
      if (s(1:1) == ".") s = "0" // s
      if (s(1:2) == "-.") s = "-0" // s(2:)
   end function ftoa

   !> Trim an allocatable string or return a default if empty/unallocated.
   pure function trim_or_default(s, default) result(r)
      character(len=:), allocatable, intent(in) :: s
      character(len=*), intent(in) :: default
      character(len=:), allocatable :: r
      if (allocated(s)) then
         if (len_trim(s) > 0) then
            r = trim(s)
         else
            r = default
         end if
      else
         r = default
      end if
   end function trim_or_default

   !> Trim an allocatable string or return an empty string if unallocated.
   pure function trim_or_empty(s) result(r)
      character(len=:), allocatable, intent(in) :: s
      character(len=:), allocatable :: r
      if (allocated(s)) then
         r = trim(s)
      else
         r = ""
      end if
   end function trim_or_empty

   !> Join a list of strings into a comma-separated value string.
   !!
   !! Empty entries are skipped. If all entries are empty (or the array is not
   !! allocated), returns the provided `empty` placeholder string.
   pure function join_csv(a, empty) result(s)
      type(string_t), allocatable, intent(in) :: a(:)
      character(len=*), intent(in) :: empty
      character(len=:), allocatable :: s
      character(len=:), allocatable :: tmp
      integer :: i
      tmp = ""
      if (allocated(a)) then
         do i = 1, size(a)
            if (len_trim(a(i)%s) == 0) cycle
            if (len(tmp) > 0) tmp = tmp // ","
            tmp = tmp // trim(a(i)%s)
         end do
      end if
      if (len(tmp) == 0) then
         s = empty
      else
         s = tmp
      end if
   end function join_csv

end module watch_util
