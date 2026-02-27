!> Sleep utilities and low-CPU waiting.
!!
!! `fpm-watch` is primarily poll-based. This module provides two waiting
!! strategies:
!!
!! - **Spin wait** (`sleep_spin`): uses `system_clock` in a tight loop.
!!   This is portable and offers good timing resolution, but it can consume CPU.
!!
!! - **OS sleep** (`sleep_os`): delegates to a small C shim
!!   (`fpm_watch_sleep_seconds`) which performs an efficient, platform-native
!!   sleep:
!!     - Windows: `Sleep(milliseconds)`
!!     - POSIX: `nanosleep(timespec)` (with EINTR retry)
!!
!! The public entry point `sleep_seconds` dispatches between these strategies
!! depending on the global low-CPU mode flag set by `set_low_cpu`.
!!
!! ## Design notes
!! - The OS sleep is implemented in C to avoid link-time issues where referencing
!!   platform-specific symbols (e.g. `Sleep` or `nanosleep`) from Fortran can
!!   break linking on the “other” platform even if the routine is never called.
!! - `sleep_seconds` is intentionally lightweight and safe to call frequently.
!!
!! ## Usage
!! ```fortran
!! use watch_time, only: set_low_cpu, sleep_seconds
!!
!! call set_low_cpu(.true.)   ! prefer OS sleep (near-zero idle CPU)
!! call sleep_seconds(0.25)   ! sleep for ~250 ms
!! ```
!!
!! ## Threading/Signals
!! - Spin sleep does not yield; it runs until time has elapsed.
!! - OS sleep yields to the OS scheduler; on POSIX the C shim typically retries
!!   on interrupt (EINTR) to honor the requested duration.
module watch_time
   use, intrinsic :: iso_fortran_env, only: int64
   use, intrinsic :: iso_c_binding,  only: c_double
   use fpm_environment, only: get_os_type, OS_WINDOWS
   implicit none
   private
   public sleep_seconds, set_low_cpu

   !> Global mode switch that selects OS sleep vs spin sleep.
   !!
   !! - `.false.` (default): `sleep_seconds` uses `sleep_spin` (busy wait).
   !! - `.true.`           : `sleep_seconds` uses `sleep_os` (OS-backed sleep).
   logical, save :: low_cpu_mode = .false.

   !> C shim providing a portable OS sleep implementation.
   !!
   !! The actual platform-specific sleep is implemented in C to prevent
   !! link-time failures that can occur if Fortran directly references
   !! `Sleep` (Windows) or `nanosleep` (POSIX) symbols in the same object.
   !!
   !! ### Arguments
   !! - `sec`: requested sleep duration in seconds.
   interface
      subroutine fpm_watch_sleep_seconds(sec) bind(C, name="fpm_watch_sleep_seconds")
         import c_double
         implicit none
         real(c_double), value :: sec
      end subroutine fpm_watch_sleep_seconds
   end interface

contains

   !> Enable or disable low-CPU mode.
   !!
   !! When enabled, `sleep_seconds` uses the OS sleep backend which typically
   !! yields to the scheduler and keeps idle CPU usage close to zero.
   !!
   !! ### Arguments
   !! - `flag`: `.true.` to enable low-CPU mode, `.false.` to use spin-waiting.
   subroutine set_low_cpu(flag)
      logical, intent(in) :: flag
      low_cpu_mode = flag
   end subroutine set_low_cpu

   !> Sleep for approximately `s` seconds.
   !!
   !! Dispatches to either:
   !! - `sleep_os` when low-CPU mode is enabled, or
   !! - `sleep_spin` when low-CPU mode is disabled.
   !!
   !! ### Arguments
   !! - `s`: requested duration in seconds. Non-positive values return immediately.
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
   !! This implementation uses `system_clock` to measure elapsed ticks.
   !! It does not yield to the OS scheduler and may consume CPU while waiting.
   !!
   !! ### Arguments
   !! - `s`: requested duration in seconds. Non-positive values return immediately.
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
   !! This routine delegates to the C shim `fpm_watch_sleep_seconds`, which
   !! performs platform-native sleeping (Windows `Sleep`, POSIX `nanosleep`).
   !!
   !! The `fpm_environment` OS check is retained for clarity and future
   !! platform-specific extensions, but both branches call the same shim.
   !!
   !! ### Arguments
   !! - `s`: requested duration in seconds. Non-positive values return immediately.
   subroutine sleep_os(s)
      real, intent(in) :: s
      if (get_os_type() == OS_WINDOWS) then
         call fpm_watch_sleep_seconds(real(s, c_double))
      else
         call fpm_watch_sleep_seconds(real(s, c_double))
      end if
   end subroutine sleep_os

end module watch_time