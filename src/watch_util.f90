!> Miscellaneous string utilities.
!!
!! Provides small helpers for formatting and manipulating allocatable strings
!! and string arrays.
module watch_util
   use fpm_strings, only: string_t
   implicit none
   private
   public ftoa, trim_or_default, trim_or_empty, join_csv, string_accum_t, sort_unique_strings

   !> Simple amortized string accumulator.
   !!
   !! Used to build arrays of strings efficiently without repeated reallocation.
   type string_accum_t
      type(string_t), allocatable :: a(:)
      integer :: n = 0
   contains
      procedure :: push => accum_push
      procedure :: to_array => accum_to_array
   end type string_accum_t

contains

   !> Format a real number with two decimal places.
   pure function ftoa(x) result(s)
      real, intent(in) :: x
      character(len=:), allocatable :: s
      character(len=64) :: buf
      write(buf,'(f0.2)') x
      s = trim(buf)
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

   !> Append a string to the accumulator (grows capacity as needed).
   subroutine accum_push(self, s)
      class(string_accum_t), intent(inout) :: self
      character(len=*), intent(in) :: s
      type(string_t), allocatable :: tmp(:)
      integer :: cap

      if (len_trim(s) == 0) return

      if (.not. allocated(self%a)) then
         allocate(self%a(256))
         self%n = 0
      end if

      cap = size(self%a)
      if (self%n >= cap) then
         allocate(tmp(max(1, 2*cap)))
         if (cap > 0) tmp(1:cap) = self%a(1:cap)
         call move_alloc(tmp, self%a)
      end if

      self%n = self%n + 1
      self%a(self%n)%s = s
   end subroutine accum_push

   !> Materialize the accumulator into a right-sized array.
   !!
   !! The accumulator storage is released after conversion.
   subroutine accum_to_array(self, out)
      class(string_accum_t), intent(inout) :: self
      type(string_t), allocatable, intent(out) :: out(:)

      if (.not. allocated(self%a) .or. self%n <= 0) then
         allocate(out(0))
         return
      end if

      allocate(out(self%n))
      out = self%a(1:self%n)
      deallocate(self%a)
      self%n = 0
   end subroutine accum_to_array

   !> Sort an array of strings and remove duplicates.
   !!
   !! Sorting is lexicographic on `string_t%s`.
   subroutine sort_unique_strings(a)
      type(string_t), allocatable, intent(inout) :: a(:)
      type(string_t), allocatable :: out(:)
      integer :: i, nuniq

      if (.not. allocated(a)) return
      if (size(a) <= 1) return

      call qsort(a, 1, size(a))

      nuniq = 1
      do i = 2, size(a)
         if (a(i)%s /= a(i-1)%s) nuniq = nuniq + 1
      end do

      if (nuniq == size(a)) return

      allocate(out(nuniq))
      out(1) = a(1)
      nuniq = 1
      do i = 2, size(a)
         if (a(i)%s == a(i-1)%s) cycle
         nuniq = nuniq + 1
         out(nuniq) = a(i)
      end do

      call move_alloc(out, a)
   contains
      recursive subroutine qsort(x, lo, hi)
         type(string_t), intent(inout) :: x(:)
         integer, intent(in) :: lo, hi
         integer :: i, j
         character(len=:), allocatable :: p
         type(string_t) :: t

         if (lo >= hi) return

         i = lo
         j = hi
         p = x((lo+hi)/2)%s

         do
            do while (x(i)%s < p)
               i = i + 1
            end do
            do while (p < x(j)%s)
               j = j - 1
            end do
            if (i <= j) then
               t = x(i)
               x(i) = x(j)
               x(j) = t
               i = i + 1
               j = j - 1
            end if
            if (i > j) exit
         end do

         if (lo < j) call qsort(x, lo, j)
         if (i < hi) call qsort(x, i, hi)
      end subroutine qsort
   end subroutine sort_unique_strings

end module watch_util
