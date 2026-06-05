!> Lightweight structured logging for console output.
!!
!! Provides a consistent `key | message` style format with optional color.
!!
!! Logging respects `w%verbosity`:
!! - `< 0`: suppress info/warn output
!! - `debug` messages require `w%debug = .true.`
module watch_log
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   use watch_types, only: watch_opts_t
   use face, only: colorize
   implicit none
   private
   public log_info, log_warn, log_err, log_debug

contains

   !> Log an informational message.
   subroutine log_info(w, msg)
      type(watch_opts_t), intent(in) :: w
      character(len=*), intent(in)   :: msg
      if (w%verbosity < 0) return
      write(output_unit,'(a)') &
         colorize("info       |", color_fg='blue_intense', style='bold_on') // "  " // trim(msg)
   end subroutine log_info

   !> Log a warning message.
   subroutine log_warn(w, msg)
      type(watch_opts_t), intent(in) :: w
      character(len=*), intent(in)   :: msg
      if (w%verbosity < 0) return
      write(output_unit,'(a)') &
         colorize("warn       |", color_fg='yellow_intense', style='bold_on') // "  " // trim(msg)
   end subroutine log_warn

   !> Log an error message (always prints).
   subroutine log_err(msg)
      character(len=*), intent(in) :: msg
      write(error_unit,'(a)') &
         colorize("error      |", color_fg='red_intense', style='bold_on') // "  " // trim(msg)
   end subroutine log_err

   !> Log a debug message (prints only when `debug` is enabled).
   subroutine log_debug(w, msg)
      type(watch_opts_t), intent(in) :: w
      character(len=*), intent(in)   :: msg
      if (.not. w%debug) return
      write(output_unit,'(a)') &
         colorize("debug      |", color_fg='magenta_intense', style='bold_on') // "  " // trim(msg)
   end subroutine log_debug

end module watch_log
