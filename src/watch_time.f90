!> Use OS sleep for polling, debounce, and restart delays.
!!
!! Windows uses `Sleep`; POSIX uses `nanosleep`, retrying the remaining duration
!! after EINTR. Waiting always yields to the scheduler and has no global mode.
!! These waits do not poll the watcher's termination flag.
module watch_time
   use, intrinsic :: iso_c_binding,  only: c_double
   implicit none
   private
   public sleep_seconds

   !> Binding to the C implementation of portable OS sleep.
   interface
      !> C OS sleep; `sec` is passed by value, with nonpositive/nonfinite values ignored.
      subroutine fpm_watch_sleep_seconds(sec) bind(C, name="fpm_watch_sleep_seconds")
         import c_double
         implicit none
         real(c_double), value :: sec
      end subroutine fpm_watch_sleep_seconds
   end interface

contains

   !> Wait approximately `s` seconds while yielding to the OS scheduler.
   !!
   !! Nonpositive/nonfinite values return immediately. The portable C shim
   !! handles platform limits and interrupted sleeps without busy waiting.
   subroutine sleep_seconds(s)
      real, intent(in) :: s
      call fpm_watch_sleep_seconds(real(s, c_double))
   end subroutine sleep_seconds

end module watch_time
