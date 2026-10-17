!> Emit terminal text, JSON Lines, and logfmt using process-wide settings.
!!
!! Structured records, including errors, use stdout; text errors use stderr.
!! Each emitted record is flushed immediately and occupies one physical line.
!! Field constructors serialize typed values as JSON; logfmt quotes array values.
!! Structured strings escape controls and replace invalid UTF-8 bytes with U+FFFD.
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
      !> Attribute name (`key`) and its complete serialized JSON representation (`value`).
      character(len=:), allocatable :: key, value
   end type log_field_t

   interface log_int
      module procedure log_default_int, log_int64
   end interface log_int

   !> Selected process format; unallocated state is treated as text.
   character(len=:), allocatable, save :: active_format
   !> Minimum-detail filter: -1 quiet, 0 normal, 1..2 additional detail.
   integer, save :: active_verbosity = 0
   !> Allow debug-level records when verbosity also permits them.
   logical, save :: active_debug = .false.

contains

   !> Copy format, verbosity, and debug options into process-wide logger state.
   !!
   !! An unallocated format selects text. Callers must validate format names;
   !! this routine neither normalizes nor validates the configuration.
   subroutine configure_logging(w)
      type(watch_opts_t), intent(in) :: w
      active_format = "text"
      if (allocated(w%log_format)) active_format = w%log_format
      active_verbosity = w%verbosity
      active_debug = w%debug
   end subroutine configure_logging

   !> Accept exactly the case-sensitive format names `text`, `json`, and `logfmt`.
   pure logical function valid_log_format(name) result(valid)
      character(len=*), intent(in) :: name
      valid = name == "text" .or. name == "json" .or. name == "logfmt"
   end function valid_log_format

   !> Return whether the process currently emits a machine-readable stream.
   logical function structured_logging() result(structured)
      structured = .false.
      if (allocated(active_format)) structured = active_format /= "text"
   end function structured_logging

   !> Format, write, and flush one timestamped event with optional typed attributes.
   !!
   !! `verbosity` is the minimum detail level, defaulting to zero. Quiet mode
   !! suppresses non-error records; debug records additionally require debug mode.
   !! `force=true` and errors bypass both filters. Structured output always uses
   !! stdout; only text errors use stderr. `fields` should use the typed constructors.
   !! Output errors are nonfatal so a disconnected terminal cannot replace a
   !! command or cancellation status with a Fortran runtime I/O failure.
   subroutine log_event(level, event, message, fields, verbosity, force)
      character(len=*), intent(in) :: level, event, message
      !! Severity, stable event identifier, and human-readable message, respectively.
      type(log_field_t), intent(in), optional :: fields(:)
      !! Typed attributes constructed with log_string/log_strings/log_int/log_real/log_bool.
      integer, intent(in), optional :: verbosity
      !! Minimum active verbosity for this non-error event; defaults to zero.
      logical, intent(in), optional :: force
      !! Bypass verbosity/debug filters for required protocol events; defaults to false.
      integer :: unit, detail, io_status
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
      write(unit, "(a)", iostat=io_status) line
      if (io_status /= 0) return
      flush(unit, iostat=io_status)
      if (io_status /= 0) return
   end subroutine log_event

   !> Render text, JSON, or logfmt without logging I/O or reading the clock.
   !!
   !! `timestamp` must provide RFC 3339 text (at least 19 characters for text mode).
   !! Structured fields require allocated keys and nonempty serialized JSON values.
   !! Text uses HH:MM:SS, level, and message; it omits event/attribute fields.
   !! Unrecognized formats use text. This routine does not apply verbosity filters.
   pure function format_log_record(format, timestamp, level, event, message, fields) result(line)
      character(len=*), intent(in) :: format, timestamp, level, event, message
      type(log_field_t), intent(in), optional :: fields(:)
      character(len=:), allocatable :: line, label, prefix
      integer :: i, total, position
      select case (format)
       case ("json")
         prefix = '{"timestamp":' // json_quote(timestamp) // ',"level":' // json_quote(level) // &
                  ',"event":' // json_quote(event) // ',"message":' // json_quote(message)
         total = len(prefix) + 1
         if (present(fields)) then
            do i = 1, size(fields)
               total = total + 4 + escaped_length(fields(i)%key, .true.) + len(fields(i)%value)
            end do
         end if
         allocate(character(len=total) :: line)
         position = 1
         call put_text(line, position, prefix)
         if (present(fields)) then
            do i = 1, size(fields)
               call put_text(line, position, ",")
               call put_json_string(line, position, fields(i)%key)
               call put_text(line, position, ":")
               call put_text(line, position, fields(i)%value)
            end do
         end if
         call put_text(line, position, "}")
       case ("logfmt")
         prefix = "timestamp=" // timestamp // " level=" // level // " event=" // event // &
                  " msg=" // json_quote(message)
         total = len(prefix)
         if (present(fields)) then
            do i = 1, size(fields)
               total = total + 2 + len(fields(i)%key)
               if (fields(i)%value(1:1) == "[") then
                  total = total + 2 + escaped_length(fields(i)%value, .true.)
               else
                  total = total + len(fields(i)%value)
               end if
            end do
         end if
         allocate(character(len=total) :: line)
         position = 1
         call put_text(line, position, prefix)
         if (present(fields)) then
            do i = 1, size(fields)
               call put_text(line, position, " " // fields(i)%key // "=")
               if (fields(i)%value(1:1) == "[") then
                  call put_json_string(line, position, fields(i)%value)
               else
                  call put_text(line, position, fields(i)%value)
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

   !> Quote a byte string as JSON, escaping controls and replacing invalid UTF-8 bytes with U+FFFD.
   pure function json_quote(value) result(quoted)
      character(len=*), intent(in) :: value
      character(len=:), allocatable :: quoted
      integer :: position, total
      total = escaped_length(value, .true.) + 2
      allocate(character(len=total) :: quoted)
      position = 1
      call put_json_string(quoted, position, value)
   end function json_quote

   !> Escape control bytes so a message stays on one physical line.
   !!
   !! When `delimiters` is true, also escape quotes/backslashes and replace invalid
   !! UTF-8 bytes with JSON U+FFFD escapes. Text mode preserves non-ASCII bytes.
   pure function escape_text(value, delimiters) result(escaped)
      character(len=*), intent(in) :: value
      logical, intent(in) :: delimiters
      character(len=:), allocatable :: escaped
      integer :: position, total
      total = escaped_length(value, delimiters)
      allocate(character(len=total) :: escaped)
      position = 1
      call put_escaped(escaped, position, value, delimiters)
   end function escape_text

   !> Count escaped bytes using the same encoder as the output pass.
   pure integer function escaped_length(value, delimiters) result(length)
      character(len=*), intent(in) :: value
      logical, intent(in) :: delimiters
      character(len=6) :: piece
      integer :: i, consumed, written
      length = 0
      i = 1
      do while (i <= len(value))
         call escape_piece(value, i, delimiters, piece, consumed, written)
         length = length + written
         i = i + consumed
      end do
   end function escaped_length

   !> Append bytes into an already sized buffer without reallocating it.
   pure subroutine put_text(buffer, position, value)
      !> Caller-owned capacity and untouched bytes must remain unchanged while appending.
      ! allow(C072)
      character(len=*), intent(inout) :: buffer
      integer, intent(inout) :: position
      character(len=*), intent(in) :: value
      buffer(position:position + len(value) - 1) = value
      position = position + len(value)
   end subroutine put_text

   !> Append one quoted JSON string to an already sized buffer.
   pure subroutine put_json_string(buffer, position, value)
      !> Caller-owned capacity and untouched bytes must remain unchanged while appending.
      ! allow(C072)
      character(len=*), intent(inout) :: buffer
      integer, intent(inout) :: position
      character(len=*), intent(in) :: value
      call put_text(buffer, position, '"')
      call put_escaped(buffer, position, value, .true.)
      call put_text(buffer, position, '"')
   end subroutine put_json_string

   !> Append escaped bytes in one pass using bounded six-byte fragments.
   pure subroutine put_escaped(buffer, position, value, delimiters)
      !> Caller-owned capacity and untouched bytes must remain unchanged while appending.
      ! allow(C072)
      character(len=*), intent(inout) :: buffer
      integer, intent(inout) :: position
      character(len=*), intent(in) :: value
      logical, intent(in) :: delimiters
      character(len=6) :: piece
      integer :: i, consumed, written
      i = 1
      do while (i <= len(value))
         call escape_piece(value, i, delimiters, piece, consumed, written)
         call put_text(buffer, position, piece(:written))
         i = i + consumed
      end do
   end subroutine put_escaped

   !> Encode one ASCII byte or UTF-8 sequence and report consumed/output lengths.
   pure subroutine escape_piece(value, position, delimiters, piece, consumed, written)
      character(len=*), intent(in) :: value
      integer, intent(in) :: position
      logical, intent(in) :: delimiters
      character(len=6), intent(out) :: piece
      integer, intent(out) :: consumed, written
      character(len=4) :: hex
      integer :: code, width
      code = iachar(value(position:position))
      consumed = 1
      written = 1
      piece = value(position:position)
      select case (code)
       case (34, 92)
         if (delimiters) then
            piece = achar(92) // value(position:position)
            written = 2
         end if
       case (8, 9, 10, 12, 13)
         select case (code)
          case (8)
            piece = achar(92) // "b"
          case (9)
            piece = achar(92) // "t"
          case (10)
            piece = achar(92) // "n"
          case (12)
            piece = achar(92) // "f"
          case default
            piece = achar(92) // "r"
         end select
         written = 2
       case (0:7, 11, 14:31)
         write(hex, "(z4.4)") code
         piece = achar(92) // "u" // hex
         written = 6
       case (128:255)
         if (.not. delimiters) return
         width = utf8_width(value, position)
         if (width == 0) then
            piece = achar(92) // "uFFFD"
            written = 6
         else
            piece = value(position:position + width - 1)
            consumed = width
            written = width
         end if
       case default
         continue
      end select
   end subroutine escape_piece

   !> Validate a multibyte UTF-8 sequence at a valid 1-based byte position.
   !!
   !! Return 2..4 for a valid sequence, or zero for invalid/truncated input,
   !! ASCII bytes, surrogate encodings, or out-of-range Unicode values.
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
      integer :: i, total, position
      field%key = key
      total = 2 + max(0, size(values) - 1)
      do i = 1, size(values)
         total = total + 2 + escaped_length(values(i)%s, .true.)
      end do
      allocate(character(len=total) :: field%value)
      position = 1
      call put_text(field%value, position, "[")
      do i = 1, size(values)
         if (i > 1) call put_text(field%value, position, ",")
         call put_json_string(field%value, position, values(i)%s)
      end do
      call put_text(field%value, position, "]")
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

   !> Serialize a real with three decimal places (scientific fallback); nonfinite values become JSON null.
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
