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
   use watch_util, only: starts_with
   use watch_platform, only: is_windows_os => is_windows
   implicit none
   private
   public get_arg, join_argv, parse_int, parse_real, needs_quotes, quote_arg, is_windows_os, starts_with
   public watch_option_takes_value

contains

   !> Return whether a bare watcher option consumes the following argv token.
   pure logical function watch_option_takes_value(a) result(takes_value)
      character(len=*), intent(in) :: a
      select case (a)
       case ("--watch-restart-delay", "--watch-restart-max", "--watch-self", &
             "--watch-rescan", "--watch-ignore", "--watch-include", "--watch-feature", &
             "--watch-poll", "--watch-debounce", "--watch-log-format", "--watch-fpm")
         takes_value = .true.
       case default
         takes_value = .false.
      end select
   end function watch_option_takes_value

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
   !! Every token is quoted using `quote_arg()`, including empty arguments.
   !! Quoting rules are platform-dependent (Windows vs POSIX).
   function join_argv(i1, i2) result(s)
      integer, intent(in) :: i1, i2
      !! Inclusive range of argv indices to join.
      character(len=:), allocatable :: s
      character(len=:), allocatable :: a
      integer :: i

      s = ""
      do i = i1, i2
         a = get_arg(i)
         if (i > i1) s = s // " "
         s = s // quote_arg(a)
      end do
   end function join_argv


   !> Determine whether an argument requires quoting for safe shell parsing.
   !!
   !! Only a nonempty token containing shell-safe characters can be unquoted.
   pure logical function needs_quotes(a) result(q)
      character(len=*), intent(in) :: a
      character(len=*), parameter :: safe = &
         "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_@+=:,./-"
      q = len(a) == 0 .or. verify(a, safe) /= 0
   end function needs_quotes

   !> Quote an argument for the host shell, escaping embedded quotes as needed.
   !!
   !! - On Windows, embedded quotes use an odd number of backslashes and
   !!   trailing backslashes are doubled for the C runtime's argument parser.
   !! - On POSIX shells, `'` is escaped using a standard `'` → `'"'"'` pattern.
   function quote_arg(a, windows) result(q)
      character(len=*), intent(in) :: a
      logical, intent(in), optional :: windows
      !! Explicit platform selection supports portable serialization checks.
      character(len=:), allocatable :: q
      logical :: use_windows
      if (present(windows)) then
         use_windows = windows
      else
         use_windows = is_windows_os()
      end if
      if (use_windows) then
         q = '"' // escape_quotes_win(a) // '"'
      else
         q = "'" // escape_quotes_sh(a) // "'"
      end if
   contains
      pure function escape_quotes_win(s) result(r)
         character(len=*), intent(in) :: s
         character(len=:), allocatable :: r
         integer :: j, backslashes
         r = ""
         backslashes = 0
         do j = 1, len(s)
            if (s(j:j) == achar(92)) then
               backslashes = backslashes + 1
               cycle
            end if
            if (s(j:j) == '"') then
               r = r // repeat(achar(92), 2 * backslashes + 1) // '"'
            else
               r = r // repeat(achar(92), backslashes) // s(j:j)
            end if
            backslashes = 0
         end do
         r = r // repeat(achar(92), 2 * backslashes)
      end function escape_quotes_win

      pure function escape_quotes_sh(s) result(r)
         character(len=*), intent(in) :: s
         character(len=:), allocatable :: r
         integer :: j
         r = ""
         do j = 1, len(s)
            if (s(j:j) == "'") then
               r = r // "'""'""'"
            else
               r = r // s(j:j)
            end if
         end do
      end function escape_quotes_sh
   end function quote_arg

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
