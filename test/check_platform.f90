!> Check that command failures are distinct from console interruption.
program check_platform
   use watch_cmdline, only: get_arg, quote_arg
   use watch_platform, only: execute_command, termination_requested, create_temp_directory, remove_temp_directory
   use fpm_error, only: error_t
   use fpm_filesystem, only: is_absolute_path
   implicit none

   character(len=:), allocatable :: mode, executable, command
   integer :: status, i, unit
   type(error_t), allocatable :: error
   character(len=:), allocatable :: first_temp, second_temp
   logical :: present_file
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

   command = quote_arg("fpm-watch-command-that-does-not-exist-7e4a9c")
   call execute_command(command, status, interrupted, silent=.true.)
   if (status /= 127) error stop "missing executable did not return launch-failure status"
   if (interrupted .or. termination_requested()) error stop "missing executable requested cancellation"
   call create_temp_directory(first_temp, error)
   if (allocated(error)) error stop "could not reserve first temporary directory"
   call create_temp_directory(second_temp, error)
   if (allocated(error)) then
      call remove_temp_directory(first_temp)
      error stop "could not reserve second temporary directory"
   end if
   if (first_temp == second_temp) error stop "temporary directorynames are not unique"
   if (.not. is_absolute_path(first_temp)) error stop "temporary path is not absolute"
   inquire(file=first_temp, exist=present_file)
   if (.not. present_file) error stop "temporary directory was not created"
   open(newunit=unit, file=first_temp // "/response", status="new", action="write", iostat=status)
   if (status /= 0) error stop "reserved temporary directory could not hold a response"
   write(unit, "(a)") "graph response"
   close(unit)
   call remove_temp_directory(first_temp)
   call remove_temp_directory(first_temp)
   call remove_temp_directory(second_temp)
   inquire(file=first_temp, exist=present_file)
   if (present_file) error stop "temporary directory was not removed"
end program check_platform
