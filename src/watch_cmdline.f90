!> Command-line helpers.
!!
!! Provides convenience wrappers around `get_command_argument` and a small
!! collection of parsing and quoting helpers used throughout `fpm-watch`.
!!
!! Notes:
!! - Argument quoting here is intended for building a safe *command string*,
!!   not for implementing a complete shell escaping library.
!! - Windows vs POSIX quoting behavior is handled via `get_os_type()`.
module watch_cmdline
   use fpm_environment, only: get_os_type, OS_WINDOWS
   implicit none
   private
   public get_arg, join_argv, parse_int, parse_real, needs_quotes, quote_arg, is_windows_os, starts_with

contains

   !> Retrieve a command-line argument as an allocatable string.
   function get_arg(i) result(a)
      integer, intent(in) :: i
      !! 0-based/1-based index passed to `get_command_argument`.
      character(len=:), allocatable :: a
      integer :: n, istat
      n = 0
      call get_command_argument(i, length=n, status=istat)
      if (istat /= 0 .or. n <= 0) then
         a = ""
         return
      end if
      allocate(character(len=n) :: a)
      call get_command_argument(i, value=a, status=istat)
      if (istat /= 0) a = ""
   end function get_arg

   !> Join a range of argv entries into a single command-line string.
   !!
   !! Each token is quoted when needed, using `quote_arg()`; quoting rules are
   !! platform-dependent (Windows vs POSIX).
   function join_argv(i1, i2) result(s)
      integer, intent(in) :: i1, i2
      !! Inclusive range of argv indices to join.
      character(len=:), allocatable :: s
      character(len=:), allocatable :: a
      integer :: i

      s = ""
      do i = i1, i2
         a = get_arg(i)
         if (len_trim(a) == 0) cycle
         if (needs_quotes(a)) then
            s = s // quote_arg(a) // " "
         else
            s = s // trim(a) // " "
         end if
      end do
      s = trim(s)
   end function join_argv

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

   !> Determine whether an argument requires quoting for safe shell parsing.
   !!
   !! A conservative check: any whitespace or quotes trigger quoting.
   pure logical function needs_quotes(a) result(q)
      character(len=*), intent(in) :: a
      q = (index(a, ' ') /= 0) .or. (index(a, char(9)) /= 0) .or. (index(a, '"') /= 0) .or. (index(a, "'") /= 0)
   end function needs_quotes

   !> Quote an argument for the host shell, escaping embedded quotes as needed.
   !!
   !! - On Windows, `"` is doubled.
   !! - On POSIX shells, `'` is escaped using a standard `'` → `'"'"'` pattern.
   function quote_arg(a) result(q)
      character(len=*), intent(in) :: a
      character(len=:), allocatable :: q
      if (is_windows_os()) then
         q = '"' // escape_quotes_win(trim(a)) // '"'
      else
         q = "'" // escape_quotes_sh(trim(a)) // "'"
      end if
   contains
      pure function escape_quotes_win(s) result(r)
         character(len=*), intent(in) :: s
         character(len=:), allocatable :: r
         integer :: j
         r = ""
         do j = 1, len_trim(s)
            if (s(j:j) == '"') then
               r = r // '""'
            else
               r = r // s(j:j)
            end if
         end do
      end function escape_quotes_win

      pure function escape_quotes_sh(s) result(r)
         character(len=*), intent(in) :: s
         character(len=:), allocatable :: r
         integer :: j
         r = ""
         do j = 1, len_trim(s)
            if (s(j:j) == "'") then
               r = r // "'""'""'"
            else
               r = r // s(j:j)
            end if
         end do
      end function escape_quotes_sh
   end function quote_arg

   !> Return whether the current OS should be treated as Windows for quoting.
   logical function is_windows_os()
      is_windows_os = (get_os_type() == OS_WINDOWS)
   end function is_windows_os

   !> Parse an integer with a default fallback on I/O error.
   pure integer function parse_int(s, default) result(v)
      character(len=*), intent(in) :: s
      integer, intent(in) :: default
      integer :: ios
      read(s, *, iostat=ios) v
      if (ios /= 0) v = default
   end function parse_int

   !> Parse a real with a default fallback on I/O error.
   pure real function parse_real(s, default) result(v)
      character(len=*), intent(in) :: s
      real, intent(in) :: default
      integer :: ios
      read(s, *, iostat=ios) v
      if (ios /= 0) v = default
   end function parse_real

end module watch_cmdline
