!> Check that the sole waiting backend yields CPU and handles invalid durations.
program check_time
   use, intrinsic :: iso_fortran_env, only: int64
   use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf
   use watch_time, only: sleep_seconds
   implicit none
   integer(int64) :: start, finish, rate
   real :: cpu_start, cpu_finish, elapsed

   call sleep_seconds(0.0)
   call sleep_seconds(-1.0)
   call sleep_seconds(ieee_value(0.0, ieee_quiet_nan))
   call sleep_seconds(ieee_value(0.0, ieee_positive_inf))

   call system_clock(start, rate)
   call cpu_time(cpu_start)
   call sleep_seconds(0.25)
   call cpu_time(cpu_finish)
   call system_clock(finish)
   elapsed = real(finish - start) / real(rate)
   if (elapsed < 0.20) error stop "OS sleep returned before its requested duration"
   if (cpu_finish - cpu_start > 0.10) error stop "OS sleep consumed CPU instead of yielding"
end program check_time
