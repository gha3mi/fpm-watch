!> Platform-specific helpers.
!!
!! Provides shell wrapping and null-device paths for portable command execution.
!!
!! This module is intentionally minimal and uses `fpm_environment:get_os_type`
!! for OS detection.
module watch_platform
   use fpm_environment, only: get_os_type, OS_WINDOWS
   implicit none
   private
   public is_windows, shell_wrap, null_device

contains

   !> Return whether the current OS is Windows.
   logical function is_windows()
      is_windows = (get_os_type() == OS_WINDOWS)
   end function is_windows

   !> Return the system null device path.
   function null_device() result(p)
      character(len=:), allocatable :: p
      if (is_windows()) then
         p = "NUL"
      else
         p = "/dev/null"
      end if
   end function null_device

   !> Wrap a command for execution by the platform shell.
   !!
   !! - Windows: `cmd /c "..."` with doubled quotes
   !! - POSIX: `sh -c '...'` with standard single-quote escaping
   function shell_wrap(cmd) result(wrapped)
      character(len=*), intent(in) :: cmd
      character(len=:), allocatable :: wrapped

      if (is_windows()) then
         wrapped = 'cmd /c "' // escape_quotes_win(trim(cmd)) // '"'
      else
         wrapped = "sh -c '" // escape_quotes_sh(trim(cmd)) // "'"
      end if

   contains

      function escape_quotes_win(s) result(r)
         character(len=*), intent(in) :: s
         character(len=:), allocatable :: r
         integer :: i
         r = ""
         do i = 1, len_trim(s)
            if (s(i:i) == '"') then
               r = r // '""'
            else
               r = r // s(i:i)
            end if
         end do
      end function escape_quotes_win

      function escape_quotes_sh(s) result(r)
         character(len=*), intent(in) :: s
         character(len=:), allocatable :: r
         integer :: i
         r = ""
         do i = 1, len_trim(s)
            if (s(i:i) == "'") then
               r = r // "'""'""'"
            else
               r = r // s(i:i)
            end if
         end do
      end function escape_quotes_sh

   end function shell_wrap

end module watch_platform
