module watch_time
   use, intrinsic :: iso_fortran_env, only: int64
   use, intrinsic :: iso_c_binding,  only: c_double
   use fpm_environment, only: get_os_type, OS_WINDOWS
   implicit none
   private
   public sleep_seconds, set_low_cpu

   logical, save :: low_cpu_mode = .false.

   interface
      subroutine fpm_watch_sleep_seconds(sec) bind(C, name="fpm_watch_sleep_seconds")
         import c_double
         implicit none
         real(c_double), value :: sec
      end subroutine fpm_watch_sleep_seconds
   end interface

contains

   subroutine set_low_cpu(flag)
      logical, intent(in) :: flag
      low_cpu_mode = flag
   end subroutine set_low_cpu

   subroutine sleep_seconds(s)
      real, intent(in) :: s
      if (s <= 0.0) return
      if (low_cpu_mode) then
         call sleep_os(s)
      else
         call sleep_spin(s)
      end if
   end subroutine sleep_seconds

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

   subroutine sleep_os(s)
      real, intent(in) :: s
      if (get_os_type() == OS_WINDOWS) then
         call fpm_watch_sleep_seconds(real(s, c_double))
      else
         call fpm_watch_sleep_seconds(real(s, c_double))
      end if
   end subroutine sleep_os

end module watch_time