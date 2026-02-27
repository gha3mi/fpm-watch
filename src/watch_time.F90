!> Sleep utilities and low-CPU waiting.
!!
!! `fpm-watch` is primarily poll-based. To avoid burning CPU while idle, this
!! module provides:
!! - a default spin-wait implementation (`sleep_spin`) for portability, and
!! - an OS sleep implementation (`sleep_os`) for near-zero CPU usage.
!!
!! `set_low_cpu(.true.)` selects OS sleep for `sleep_seconds`.
module watch_time
   use, intrinsic :: iso_fortran_env, only: int64
   use, intrinsic :: iso_c_binding,  only: c_int, c_long
   implicit none
   private
   public sleep_seconds, set_low_cpu

   !> Global mode switch that selects OS sleep vs spin sleep.
   logical, save :: low_cpu_mode = .false.

! POSIX detection is reliable on Linux/macOS. Default to Windows otherwise.
#if defined(__unix__) || defined(__unix) || defined(__linux__) || defined(__APPLE__)
   type, bind(C) :: timespec
      integer(c_long) :: tv_sec
      integer(c_long) :: tv_nsec
   end type timespec

   interface
      integer(c_int) function c_nanosleep(req, rem) bind(C, name="nanosleep")
         import c_int, timespec
         implicit none
         type(timespec), intent(in)  :: req
         type(timespec), intent(out) :: rem
      end function c_nanosleep
   end interface
#else
   interface
      subroutine c_sleep_ms(ms) bind(C, name="Sleep")
         import c_int
         implicit none
         integer(c_int), value :: ms
      end subroutine c_sleep_ms
   end interface
#endif

contains

   !> Enable or disable low-CPU mode.
   !!
   !! When enabled, `sleep_seconds` uses the OS sleep implementation.
   subroutine set_low_cpu(flag)
      logical, intent(in) :: flag
      low_cpu_mode = flag
   end subroutine set_low_cpu

   !> Sleep for a number of seconds.
   !!
   !! Dispatches to either `sleep_os` (low CPU) or `sleep_spin` (busy wait)
   !! depending on the global `low_cpu_mode`.
   subroutine sleep_seconds(s)
      real, intent(in) :: s
      if (s <= 0.0) return
      if (low_cpu_mode) then
         call sleep_os(s)
      else
         call sleep_spin(s)
      end if
   end subroutine sleep_seconds

   !> Busy-wait for approximately `s` seconds.
   !!
   !! This is used only when low CPU mode is disabled.
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

   !> OS-backed sleep for approximately `s` seconds.
   !!
   !! - Windows: calls `Sleep(ms)`
   !! - POSIX: calls `nanosleep(timespec)`
   !!
   !! This is intended to minimize CPU usage while waiting.
   subroutine sleep_os(s)
      real, intent(in) :: s
#if defined(__unix__) || defined(__unix) || defined(__linux__) || defined(__APPLE__)
      call sleep_os_posix(s)
#else
      call sleep_os_win(s)
#endif
   end subroutine sleep_os

#if defined(__unix__) || defined(__unix) || defined(__linux__) || defined(__APPLE__)
   subroutine sleep_os_posix(s)
      real, intent(in) :: s
      type(timespec) :: req, rem
      integer(c_int) :: rc
      real :: ss, frac
      integer(c_long) :: sec, nsec
      ss = s
      if (ss <= 0.0) return
      sec  = int(ss, kind=c_long)
      frac = ss - real(sec)
      if (frac < 0.0) frac = 0.0
      nsec = int(frac * 1000000000.0, kind=c_long)
      if (nsec < 0_c_long) nsec = 0_c_long
      if (nsec >= 1000000000_c_long) then
         sec = sec + 1_c_long
         nsec = nsec - 1000000000_c_long
      end if
      req%tv_sec  = sec
      req%tv_nsec = nsec
      do
         rc = c_nanosleep(req, rem)
         if (rc == 0_c_int) exit
         req = rem
      end do
   end subroutine sleep_os_posix
#else
   subroutine sleep_os_win(s)
      real, intent(in) :: s
      integer(c_int) :: ms
      real :: ss
      ss = s
      if (ss <= 0.0) return
      ms = int(ss * 1000.0 + 0.5, kind=c_int)
      if (ms < 0_c_int) ms = 0_c_int
      call c_sleep_ms(ms)
   end subroutine sleep_os_win
#endif

end module watch_time