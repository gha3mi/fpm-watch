!> Choose busy waiting or OS sleep for polling, debounce, and restart delays.
!!
!! `set_low_cpu` selects a process-wide backend: `system_clock` spin waiting
!! by default, or the C sleep shim when enabled. Windows uses `Sleep` with
!! millisecond rounding; POSIX uses `nanosleep`, retrying the remaining duration
!! after EINTR. These waits do not poll the watcher's termination flag.
module watch_time
   use, intrinsic :: iso_fortran_env, only: int64
   use, intrinsic :: iso_c_binding,  only: c_double
   implicit none
   private
   public sleep_seconds, set_low_cpu

   !> Global mode switch that selects OS sleep vs spin sleep.
   !!
   !! - `.false.` (default): `sleep_seconds` uses `sleep_spin` (busy wait).
   !! - `.true.`           : `sleep_seconds` uses `sleep_os` (OS-backed sleep).
   logical, save :: low_cpu_mode = .false.

   !> Binding to the C implementation of portable OS sleep.
   interface
      !> C OS-sleep backend; `sec` is passed by value in seconds, with nonpositive values ignored.
      subroutine fpm_watch_sleep_seconds(sec) bind(C, name="fpm_watch_sleep_seconds")
         import c_double
         implicit none
         real(c_double), value :: sec
      end subroutine fpm_watch_sleep_seconds
   end interface

contains

   !> Set the process-wide waiting backend: true selects OS sleep, false selects busy waiting.
   subroutine set_low_cpu(flag)
      logical, intent(in) :: flag
      low_cpu_mode = flag
   end subroutine set_low_cpu

   !> Wait approximately `s` seconds using the currently selected backend.
   !!
   !! Nonpositive values return immediately. OS sleep yields to the scheduler;
   !! spin waiting consumes CPU. Neither backend checks watcher cancellation.
   subroutine sleep_seconds(s)
      real, intent(in) :: s
      if (s <= 0.0) return
      if (low_cpu_mode) then
         call sleep_os(s)
      else
         call sleep_spin(s)
      end if
   end subroutine sleep_seconds

   !> Busy-wait for the truncated number of system-clock ticks in `s` seconds.
   !!
   !! Nonpositive/sub-tick durations return immediately. A nonpositive clock
   !! rate falls back to 1000 ticks/s. The loop does not yield or check cancellation.
   subroutine sleep_spin(s)
      real, intent(in) :: s
      integer(int64) :: rate, t0, t1, ticks
      if (s <= 0.0) return
      call system_clock(count_rate=rate)
      if (rate <= 0_int64) rate = 1000_int64
      ticks = int(s * real(rate), int64)
      if (ticks <= 0_int64) return
      call system_clock(t0)
      do
         call system_clock(t1)
         if (t1 - t0 >= ticks) exit
      end do
   end subroutine sleep_spin

   !> Convert seconds to C double and invoke the OS-sleep shim; nonpositive durations are ignored there.
   subroutine sleep_os(s)
      real, intent(in) :: s
      call fpm_watch_sleep_seconds(real(s, c_double))
   end subroutine sleep_os

end module watch_time
