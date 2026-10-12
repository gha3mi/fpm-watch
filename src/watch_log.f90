!> Shared event logging for terminal text, JSON Lines, and logfmt.
!!
!! Structured records always use stdout, including errors. Text errors use
!! stderr. Every record is flushed and occupies exactly one physical line.
module watch_log
   use, intrinsic :: iso_fortran_env, only: int64, output_unit, error_unit
   use watch_types, only: watch_opts_t
   use watch_util, only: ftoa
   use fpm_strings, only: string_t
   implicit none
   private
   public configure_logging, log_event
   public log_field_t, log_string, log_strings, log_int, log_real, log_bool
   public format_log_record, json_quote, valid_log_format, structured_logging

   !> A typed event attribute, stored as its JSON representation.
   type log_field_t
      character(len=:), allocatable :: key, value
   end type log_field_t

   interface log_int
      module procedure log_default_int, log_int64
   end interface log_int

   character(len=:), allocatable, save :: active_format
   integer, save :: active_verbosity = 0
   logical, save :: active_debug = .false.

contains

   !> Set process logging options after configuration has been accepted.
   subroutine configure_logging(w)
      type(watch_opts_t), intent(in) :: w
      active_format = "text"
      if (allocated(w%log_format)) active_format = w%log_format
      active_verbosity = w%verbosity
      active_debug = w%debug
   end subroutine configure_logging

   !> Return whether a format name is supported.
   pure logical function valid_log_format(name) result(valid)
      character(len=*), intent(in) :: name
      valid = name == "text" .or. name == "json" .or. name == "logfmt"
   end function valid_log_format

   !> Return whether the process currently emits a machine-readable stream.
   logical function structured_logging() result(structured)
      structured = .false.
      if (allocated(active_format)) structured = active_format /= "text"
   end function structured_logging

   !> Emit an event with stable identity and optional typed attributes.
   !! Errors and forced protocol records bypass verbosity and debug filtering.
   !! Other debug events require debug mode and are suppressed by quiet mode.
   !! Verbosity controls additional detail.
   subroutine log_event(level, event, message, fields, verbosity, force)
      character(len=*), intent(in) :: level, event, message
      type(log_field_t), intent(in), optional :: fields(:)
      integer, intent(in), optional :: verbosity
      logical, intent(in), optional :: force
      integer :: unit, detail
      logical :: required
      character(len=:), allocatable :: line, timestamp
      if (.not. allocated(active_format)) active_format = "text"
      required = .false.
      if (present(force)) required = force
      if (level /= "error" .and. .not. required) then
         if (active_verbosity < 0) return
         detail = 0
         if (present(verbosity)) detail = verbosity
         if (active_verbosity < detail) return
         if (level == "debug" .and. .not. active_debug) return
      end if
      timestamp = log_timestamp()
      line = format_log_record(active_format, timestamp, level, event, message, fields)
      unit = output_unit
      if (active_format == "text" .and. level == "error") unit = error_unit
      write(unit, "(a)") line
      flush(unit)
   end subroutine log_event

   !> Render a record without performing I/O or reading the clock.
   pure function format_log_record(format, timestamp, level, event, message, fields) result(line)
      character(len=*), intent(in) :: format, timestamp, level, event, message
      type(log_field_t), intent(in), optional :: fields(:)
      character(len=:), allocatable :: line, label
      integer :: i
      select case (format)
       case ("json")
         line = '{"timestamp":' // json_quote(timestamp) // ',"level":' // json_quote(level) // &
                ',"event":' // json_quote(event) // ',"message":' // json_quote(message)
         if (present(fields)) then
            do i = 1, size(fields)
               line = line // "," // json_quote(fields(i)%key) // ":" // fields(i)%value
            end do
         end if
         line = line // "}"
       case ("logfmt")
         line = "timestamp=" // timestamp // " level=" // level // " event=" // event // &
                " msg=" // json_quote(message)
         if (present(fields)) then
            do i = 1, size(fields)
               if (fields(i)%value(1:1) == "[") then
                  line = line // " " // fields(i)%key // "=" // json_quote(fields(i)%value)
               else
                  line = line // " " // fields(i)%key // "=" // fields(i)%value
               end if
            end do
         end if
       case default
         select case (level)
          case ("error")
            label = "ERROR"
          case ("warn")
            label = "WARN "
          case ("debug")
            label = "DEBUG"
          case default
            label = "INFO "
         end select
         line = timestamp(12:19) // " " // label // " " // escape_text(message, .false.)
      end select
   end function format_log_record

   !> Quote a UTF-8 string, escaping JSON control characters and delimiters.
   pure function json_quote(value) result(quoted)
      character(len=*), intent(in) :: value
      character(len=:), allocatable :: quoted
      quoted = '"' // escape_text(value, .true.) // '"'
   end function json_quote

   !> Escape controls for a physical line, and JSON delimiters when requested.
   pure function escape_text(value, delimiters) result(escaped)
      character(len=*), intent(in) :: value
      logical, intent(in) :: delimiters
      character(len=:), allocatable :: escaped
      character(len=4) :: hex
      integer :: i, code, width
      escaped = ""
      i = 1
      do while (i <= len(value))
         code = iachar(value(i:i))
         select case (code)
          case (34, 92)
            if (delimiters) escaped = escaped // achar(92)
            escaped = escaped // value(i:i)
          case (8)
            escaped = escaped // achar(92) // "b"
          case (9)
            escaped = escaped // achar(92) // "t"
          case (10)
            escaped = escaped // achar(92) // "n"
          case (12)
            escaped = escaped // achar(92) // "f"
          case (13)
            escaped = escaped // achar(92) // "r"
          case (0:7, 11, 14:31)
            write(hex, "(z4.4)") code
            escaped = escaped // achar(92) // "u" // hex
          case (128:255)
            if (delimiters) then
               width = utf8_width(value, i)
               if (width == 0) then
                  escaped = escaped // achar(92) // "uFFFD"
               else
                  escaped = escaped // value(i:i+width-1)
                  i = i + width - 1
               end if
            else
               escaped = escaped // value(i:i)
            end if
          case default
            escaped = escaped // value(i:i)
         end select
         i = i + 1
      end do
   end function escape_text

   !> Return a valid UTF-8 sequence width, or zero for an invalid byte sequence.
   pure integer function utf8_width(value, position) result(width)
      character(len=*), intent(in) :: value
      integer, intent(in) :: position
      integer :: first, second, i, expected
      width = 0
      first = iachar(value(position:position))
      select case (first)
       case (194:223)
         expected = 2
       case (224:239)
         expected = 3
       case (240:244)
         expected = 4
       case default
         return
      end select
      if (position + expected - 1 > len(value)) return
      do i = position + 1, position + expected - 1
         second = iachar(value(i:i))
         if (second < 128 .or. second > 191) return
      end do
      second = iachar(value(position+1:position+1))
      if (first == 224 .and. second < 160) return
      if (first == 237 .and. second > 159) return
      if (first == 240 .and. second < 144) return
      if (first == 244 .and. second > 143) return
      width = expected
   end function utf8_width

   !> Construct a string attribute without trimming meaningful whitespace.
   pure function log_string(key, value) result(field)
      character(len=*), intent(in) :: key, value
      type(log_field_t) :: field
      field%key = key
      field%value = json_quote(value)
   end function log_string

   !> Construct a JSON string array; logfmt represents it as a quoted JSON value.
   pure function log_strings(key, values) result(field)
      character(len=*), intent(in) :: key
      type(string_t), intent(in) :: values(:)
      type(log_field_t) :: field
      integer :: i
      field%key = key
      field%value = "["
      do i = 1, size(values)
         if (i > 1) field%value = field%value // ","
         field%value = field%value // json_quote(values(i)%s)
      end do
      field%value = field%value // "]"
   end function log_strings

   !> Construct a default-integer attribute.
   pure function log_default_int(key, value) result(field)
      character(len=*), intent(in) :: key
      integer, intent(in) :: value
      type(log_field_t) :: field
      field = log_int64(key, int(value, int64))
   end function log_default_int

   !> Construct an integer attribute, including long-running counters.
   pure function log_int64(key, value) result(field)
      character(len=*), intent(in) :: key
      integer(int64), intent(in) :: value
      type(log_field_t) :: field
      character(len=32) :: buffer
      write(buffer, "(i0)") value
      field%key = key
      field%value = trim(buffer)
   end function log_int64

   !> Construct a finite real attribute with three decimal places.
   pure function log_real(key, value) result(field)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      character(len=*), intent(in) :: key
      real, intent(in) :: value
      type(log_field_t) :: field
      field%key = key
      field%value = "null"
      if (.not. ieee_is_finite(value)) return
      field%value = ftoa(value, precision=3)
   end function log_real

   !> Construct a boolean attribute.
   pure function log_bool(key, value) result(field)
      character(len=*), intent(in) :: key
      logical, intent(in) :: value
      type(log_field_t) :: field
      field%key = key
      field%value = "false"
      if (value) field%value = "true"
   end function log_bool

   !> Read local wall time with milliseconds and its explicit UTC offset.
   function log_timestamp() result(timestamp)
      character(len=:), allocatable :: timestamp
      character(len=29) :: buffer
      character(len=5) :: zone
      integer :: values(8)
      call date_and_time(zone=zone, values=values)
      write(buffer, "(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i3.3,a,a,a)") &
         values(1), "-", values(2), "-", values(3), "T", values(5), ":", values(6), ":", &
         values(7), ".", values(8), zone(1:3), ":", zone(4:5)
      timestamp = buffer
   end function log_timestamp

end module watch_log
