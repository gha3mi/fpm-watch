!> Check that command failures are distinct from console interruption.
program check_platform
   use watch_cmdline, only: get_arg, quote_arg
   use watch_platform, only: execute_command, termination_requested
   implicit none

   character(len=:), allocatable :: mode, executable, command
   integer :: status, i
   integer, parameter :: codes(3) = [0, 2, 130]
   logical :: interrupted

   mode = get_arg(1)
   select case (mode)
    case ("--exit-0")
      stop 0
    case ("--exit-2")
      stop 2
    case ("--exit-130")
      stop 130
    case default
      continue
   end select

   executable = get_arg(0)
   do i = 1, size(codes)
      select case (codes(i))
       case (0)
         mode = "--exit-0"
       case (2)
         mode = "--exit-2"
       case (130)
         mode = "--exit-130"
       case default
         error stop "unexpected test exit status"
      end select
      command = quote_arg(executable) // " " // mode
      call execute_command(command, status, interrupted, silent=.true.)
      if (status /= codes(i)) error stop "command exit status was not preserved"
      if (interrupted) error stop "ordinary command failure was treated as interruption"
      if (termination_requested()) error stop "ordinary command failure requested termination"
   end do
end program check_platform
