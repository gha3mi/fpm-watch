!> Read argv tokens, parse option values, and serialize command arguments.
!!
!! `quote_arg` always quotes the full token, including empty arguments. POSIX
!! serialization uses single quotes; Windows uses double quotes and C-runtime
!! backslash rules. Platform selection comes from `watch_platform:is_windows`
!! unless a quoting caller supplies an explicit override.
module watch_cmdline
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   use watch_util, only: starts_with
   use watch_platform, only: is_windows_os => is_windows
   implicit none
   private
   public get_arg, join_argv, parse_int, parse_real, quote_arg, is_windows_os, starts_with
   public watch_option_takes_value, parse_bool

contains

   !> Parse trimmed, case-insensitive boolean spellings or return `default`.
   !!
   !! True accepts `1`, `true`, `on`, `yes`, `y`, `t`; false accepts `0`, `false`,
   !! `off`, `no`, `n`, `f`. Empty and unrecognized values use the fallback.
   pure logical function parse_bool(s, default) result(v)
      character(len=*), intent(in) :: s
      logical, intent(in) :: default
      character(len=:), allocatable :: t
      t = trim(adjustl(lower_ascii(s)))
      select case (t)
       case ("1","true","on","yes","y","t")
         v = .true.
       case ("0","false","off","no","n","f")
         v = .false.
       case default
         v = default
      end select
   end function parse_bool

   !> Normalize ASCII letters for case-insensitive option values.
   pure function lower_ascii(s) result(r)
      character(len=*), intent(in) :: s
      character(len=len(s)) :: r
      integer :: k, c, da
      da = iachar("a") - iachar("A")
      do k = 1, len(s)
         c = iachar(s(k:k))
         if (c >= iachar("A") .and. c <= iachar("Z")) then
            r(k:k) = achar(c + da)
         else
            r(k:k) = s(k:k)
         end if
      end do
   end function lower_ascii


   !> Return whether a bare watcher option consumes the following argv token.
   pure logical function watch_option_takes_value(a) result(takes_value)
      character(len=*), intent(in) :: a
      select case (a)
       case ("--watch-restart-delay", "--watch-restart-max", "--watch-self", &
             "--watch-rescan", "--watch-ignore", "--watch-include", &
             "--watch-poll", "--watch-debounce", "--watch-log-format", "--watch-fpm")
         takes_value = .true.
       case default
         takes_value = .false.
      end select
   end function watch_option_takes_value

   !> Return argv entry `i`, or an empty string for a missing, empty, or unreadable argument.
   function get_arg(i) result(a)
      integer, intent(in) :: i
      !! Argument index: 0 is the executable name; actual arguments start at 1.
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

   !> Serialize argv entries from `i1` through `i2` with spaces and platform quoting.
   !!
   !! Every token, including an empty one, passes through `quote_arg`. A reversed
   !! range returns an empty command string.
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

   !> Serialize one complete argv token, preserving whitespace and empty arguments.
   !!
   !! Optional `windows` overrides host detection. Windows doubles trailing
   !! backslashes and escapes embedded double quotes for the C-runtime parser.
   !! POSIX uses single quotes, breaking out of them to represent embedded quotes.
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
      !> Escape embedded quotes and trailing backslashes for a double-quoted Windows argv token.
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

      !> Replace embedded single quotes with quoted fragments suitable for a POSIX shell.
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

   !> Read an integer with list-directed input; return `default` if the read fails.
   pure integer function parse_int(s, default) result(v)
      character(len=*), intent(in) :: s
      integer, intent(in) :: default
      integer :: ios
      v = default
      read(s, *, iostat=ios) v
      if (ios /= 0) v = default
   end function parse_int

   !> Read a finite real; return `default` if the read fails or yields NaN/infinity.
   pure real function parse_real(s, default) result(v)
      character(len=*), intent(in) :: s
      real, intent(in) :: default
      integer :: ios
      v = default
      read(s, *, iostat=ios) v
      if (ios /= 0) v = default
      if (.not. ieee_is_finite(v)) v = default
   end function parse_real

end module watch_cmdline
