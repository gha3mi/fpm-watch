!> Validate emitted string and number representations using the existing JSON parser.
program check_log
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
   use watch_log, only: format_log_record, log_string, log_strings, log_int, log_real, log_bool, valid_log_format
   use watch_cmdline, only: get_arg
   use fpm_strings, only: string_t
   use jonquil, only: json_loads, json_value, json_object, json_error, get_value
   implicit none
   character(len=*), parameter :: timestamp = "2026-10-12T14:32:16.842+02:00"
   character(len=:), allocatable :: message, record, decoded, quoted
   class(json_value), allocatable :: object
   type(json_error), allocatable :: error
   integer(int64) :: counter
   real(real64) :: duration
   logical :: flag
   integer :: i, begin, finish

   message = 'quotes " \ café ' // achar(92) // achar(9) // achar(10) // achar(13)
   if (get_arg(1) == "--fixture") then
      do i = 0, 31
         message = message // achar(i)
      end do
      print '(a)', format_log_record("json", timestamp, "info", "test.record", message, &
                                    [log_int("count", huge(0_int64)), log_strings("paths", [string_t(message)]), &
                                     log_real("missing", ieee_value(0.0, ieee_quiet_nan)), &
                                     log_string("unicode", "café € 😀"), &
                                     log_string("invalid_utf8", achar(128) // achar(255))])
      print '(a)', format_log_record("logfmt", timestamp, "info", "test.record", message, &
                                    [log_int("count", 42), log_strings("paths", [string_t(message)]), &
                                     log_string("unicode", "café € 😀"), &
                                     log_string("invalid_utf8", achar(128) // achar(255))])
      stop
   end if
   record = format_log_record("json", timestamp, "info", "test.record", message, &
                             [log_int("count", huge(0_int64)), log_real("duration_ms", -0.125), &
                              log_bool("enabled", .true.), log_string("path", message)])
   if (index(record, achar(10)) /= 0 .or. index(record, achar(13)) /= 0) stop 1
   call json_loads(object, record, error=error)
   if (allocated(error)) then
      print *, error%message
      print *, record
      stop 2
   end if
   select type (object)
    type is (json_object)
      call get_value(object, "message", decoded)
      if (decoded /= message) stop 3
      call get_value(object, "path", decoded)
      if (decoded /= message) stop 4
      call get_value(object, "count", counter)
      if (counter /= huge(0_int64)) stop 5
      call get_value(object, "duration_ms", duration)
      if (abs(duration + 0.125_real64) > 1.0e-12_real64) stop 6
      call get_value(object, "enabled", flag)
      if (.not. flag) stop 7
    class default
      stop 8
   end select

   record = format_log_record("logfmt", timestamp, "info", "test.record", message, [log_int("count", 42)])
   if (index(record, achar(10)) /= 0 .or. index(record, achar(13)) /= 0) stop 9
   begin = index(record, ' msg=') + len(' msg=')
   finish = index(record, ' count=') - 1
   quoted = '{"message":' // record(begin:finish) // '}'
   call json_loads(object, quoted, error=error)
   if (allocated(error)) stop 10
   select type (object)
    type is (json_object)
      call get_value(object, "message", decoded)
      if (decoded /= message) stop 11
    class default
      stop 12
   end select

   record = format_log_record("text", timestamp, "error", "test.record", message)
   if (record(:14) /= "14:32:16 ERROR ") stop 13
   if (index(record, achar(10)) /= 0 .or. index(record, achar(13)) /= 0) stop 14
   if (.not. valid_log_format("text") .or. .not. valid_log_format("json") .or. .not. valid_log_format("logfmt")) stop 15
   if (valid_log_format("unknown")) stop 16
end program check_log
