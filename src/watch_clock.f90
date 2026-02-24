!> Clock utilities for timing measurements.
!!
!! Provides a small wrapper over `system_clock` that standardizes rate/max
!! initialization and offers helpers for deltas and conversion to seconds.
!!
!! These helpers are intended for lightweight runtime timing, not for strict
!! profiling or performance benchmarking.
module watch_clock
   use, intrinsic :: iso_fortran_env, only: int64
   implicit none
   private
   public clock_init, clock_now, clock_delta, ticks_to_seconds

contains

   !> Initialize `system_clock` parameters.
   pure subroutine clock_init(rate, max)
      integer(int64), intent(out) :: rate, max
      !! Clock tick rate and maximum tick count as reported by `system_clock`.
      !!
      !! If the platform reports non-positive values, fallbacks are applied:
      !! - `rate` defaults to `1000` ticks/sec.
      !! - `max` defaults to `huge(1_int64)`.
      call system_clock(count_rate=rate, count_max=max)
      if (rate <= 0_int64) rate = 1000_int64
      if (max  <= 0_int64) max  = huge(1_int64)
   end subroutine clock_init

   !> Fetch the current clock tick count.
   subroutine clock_now(t)
      integer(int64), intent(out) :: t
      !! Current tick count from `system_clock`.
      call system_clock(t)
   end subroutine clock_now

   !> Compute tick delta between two readings with wraparound handling.
   !!
   !! Some `system_clock` implementations wrap at `count_max`. This helper
   !! treats `t0` and `t1` as readings from the same clock configuration.
   pure integer(int64) function clock_delta(t0, t1, max) result(dt)
      integer(int64), intent(in) :: t0, t1, max
      !! Start tick, end tick, and maximum tick before wrap.
      if (t1 >= t0) then
         dt = t1 - t0
      else
         dt = (max - t0 + 1_int64) + t1
      end if
   end function clock_delta

   !> Convert ticks to seconds using a specified tick rate.
   pure real function ticks_to_seconds(ticks, rate) result(secs)
      integer(int64), intent(in) :: ticks, rate
      !! Tick delta and tick rate.
      secs = real(ticks) / real(rate)
   end function ticks_to_seconds

end module watch_clock
