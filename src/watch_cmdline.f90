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
   use fpm_strings, only: string_t
   implicit none
   private
   public get_arg, join_argv, join_args, parse_int, parse_real, quote_arg, is_windows_os, starts_with
   public watch_option_takes_value, parse_bool

contains

   !> Parse one trimmed, case-insensitive boolean value without a fallback.
   !!
   !! True accepts `1`, `true`, `on`, `yes`, `y`, `t`; false accepts `0`, `false`,
   !! `off`, `no`, `n`, `f`. Empty and unrecognized values set `ok` false.
   pure subroutine parse_bool(s, v, ok)
      character(len=*), intent(in) :: s
      logical, intent(out) :: v, ok
      character(len=:), allocatable :: t
      ok = .true.
      v = .false.
      t = trim(adjustl(lower_ascii(s)))
      select case (t)
       case ("1","true","on","yes","y","t")
         v = .true.
       case ("0","false","off","no","n","f")
         v = .false.
       case default
         ok = .false.
      end select
   end subroutine parse_bool

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
       case ("--watch-restart-delay", "--watch-restart-max", &
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
   !! Collect tokens once, then share the list serializer's platform quoting.
   !! A reversed range returns an empty command string.
   function join_argv(i1, i2) result(s)
      integer, intent(in) :: i1, i2
      !! Inclusive range of argv indices to join.
      character(len=:), allocatable :: s
      type(string_t), allocatable :: arguments(:)
      integer :: i

      if (i2 < i1) then
         s = ""
         return
      end if
      allocate(arguments(i2 - i1 + 1))
      do i = i1, i2
         arguments(i - i1 + 1)%s = get_arg(i)
      end do
      s = join_args(arguments, is_windows_os())
   end function join_argv

   !> Serialize an initialized token list with one allocation and linear copying.
   !!
   !! Every token retains its full length, including empty strings and trailing
   !! whitespace. `windows` selects the same quoting rules as `quote_arg`.
   pure function join_args(arguments, windows) result(s)
      type(string_t), intent(in) :: arguments(:)
      logical, intent(in) :: windows
      character(len=:), allocatable :: s
      integer :: i, total, position

      total = max(0, size(arguments) - 1)
      do i = 1, size(arguments)
         total = total + quoted_length(arguments(i)%s, windows)
      end do
      allocate(character(len=total) :: s)
      position = 1
      do i = 1, size(arguments)
         if (i > 1) then
            s(position:position) = " "
            position = position + 1
         end if
         call put_quoted_arg(s, position, arguments(i)%s, windows)
      end do
   end function join_args

   !> Serialize one complete argv token, preserving whitespace and empty arguments.
   !!
   !! Optional `windows` overrides host detection. Windows doubles trailing
   !! backslashes and escapes embedded double quotes for the C-runtime parser.
   !! POSIX uses single quotes, breaking out of them to represent embedded quotes.
   !! Both forms allocate once and fill the result in linear time.
   function quote_arg(a, windows) result(q)
      character(len=*), intent(in) :: a
      logical, intent(in), optional :: windows
      !! Explicit platform selection supports portable serialization checks.
      character(len=:), allocatable :: q
      logical :: use_windows
      integer :: position, total
      if (present(windows)) then
         use_windows = windows
      else
         use_windows = is_windows_os()
      end if
      total = quoted_length(a, use_windows)
      allocate(character(len=total) :: q)
      position = 1
      call put_quoted_arg(q, position, a, use_windows)
   end function quote_arg

   !> Count the full quoted token using the same backslash rules as the writer.
   pure integer function quoted_length(value, windows) result(length)
      character(len=*), intent(in) :: value
      logical, intent(in) :: windows
      integer :: i, backslashes
      length = len(value) + 2
      backslashes = 0
      do i = 1, len(value)
         if (windows) then
            if (value(i:i) == achar(92)) then
               backslashes = backslashes + 1
               cycle
            end if
            if (value(i:i) == '"') length = length + backslashes + 1
            backslashes = 0
         else
            if (value(i:i) == "'") length = length + 4
         end if
      end do
      if (windows) length = length + backslashes
   end function quoted_length

   !> Fill a preallocated command buffer while retaining bytes outside this token.
   pure subroutine put_quoted_arg(buffer, position, value, windows)
      ! allow(C072)
      character(len=*), intent(inout) :: buffer
      integer, intent(inout) :: position
      character(len=*), intent(in) :: value
      logical, intent(in) :: windows
      integer :: i, backslashes, written_slashes
      character :: delimiter

      delimiter = "'"
      if (windows) delimiter = '"'
      buffer(position:position) = delimiter
      position = position + 1
      backslashes = 0
      do i = 1, len(value)
         if (windows) then
            if (value(i:i) == achar(92)) then
               backslashes = backslashes + 1
               cycle
            end if
            written_slashes = backslashes
            if (value(i:i) == '"') written_slashes = 2 * backslashes + 1
            if (written_slashes > 0) then
               buffer(position:position + written_slashes - 1) = repeat(achar(92), written_slashes)
               position = position + written_slashes
            end if
            backslashes = 0
         else if (value(i:i) == "'") then
            buffer(position:position + 4) = "'""'""'"
            position = position + 5
            cycle
         end if
         buffer(position:position) = value(i:i)
         position = position + 1
      end do
      if (windows .and. backslashes > 0) then
         written_slashes = 2 * backslashes
         buffer(position:position + written_slashes - 1) = repeat(achar(92), written_slashes)
         position = position + written_slashes
      end if
      buffer(position:position) = delimiter
      position = position + 1
   end subroutine put_quoted_arg

   !> Read one decimal integer, rejecting extra tokens, null fields, and overflow.
   pure subroutine parse_int(s, v, ok)
      character(len=*), intent(in) :: s
      integer, intent(out) :: v
      logical, intent(out) :: ok
      integer :: ios
      v = 0
      ok = numeric_token(s, real_value=.false.)
      if (.not. ok) return
      read(s, *, iostat=ios) v
      ok = ios == 0
      if (.not. ok) v = 0
   end subroutine parse_int

   !> Read one finite decimal real, rejecting extra tokens and nonnumeric syntax.
   pure subroutine parse_real(s, v, ok)
      character(len=*), intent(in) :: s
      real, intent(out) :: v
      logical, intent(out) :: ok
      integer :: ios
      v = 0.0
      ok = numeric_token(s, real_value=.true.)
      if (.not. ok) return
      read(s, *, iostat=ios) v
      ok = ios == 0
      if (ok) ok = ieee_is_finite(v)
      if (.not. ok) v = 0.0
   end subroutine parse_real

   !> Validate a signed decimal token before list-directed conversion.
   !!
   !! Reals allow a decimal point and E/D exponent. Internal whitespace, list
   !! separators, repetition syntax, null fields, and special values are rejected.
   pure logical function numeric_token(s, real_value) result(ok)
      character(len=*), intent(in) :: s
      logical, intent(in) :: real_value
      character(len=:), allocatable :: token
      integer :: i, n, digits

      ok = .false.
      token = trim(adjustl(s))
      n = len(token)
      if (n == 0) return
      i = 1
      if (token(i:i) == "+" .or. token(i:i) == "-") i = i + 1
      digits = 0
      do while (i <= n)
         if (token(i:i) < "0" .or. token(i:i) > "9") exit
         digits = digits + 1
         i = i + 1
      end do
      if (real_value .and. i <= n) then
         if (token(i:i) == ".") then
            i = i + 1
            do while (i <= n)
               if (token(i:i) < "0" .or. token(i:i) > "9") exit
               digits = digits + 1
               i = i + 1
            end do
         end if
      end if
      if (digits == 0) return
      if (real_value .and. i <= n) then
         if (index("eEdD", token(i:i)) > 0) then
            i = i + 1
            if (i <= n) then
               if (token(i:i) == "+" .or. token(i:i) == "-") i = i + 1
            end if
            digits = 0
            do while (i <= n)
               if (token(i:i) < "0" .or. token(i:i) > "9") exit
               digits = digits + 1
               i = i + 1
            end do
            if (digits == 0) return
         end if
      end if
      ok = i > n
   end function numeric_token

end module watch_cmdline
