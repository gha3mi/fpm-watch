!> Helpers for manipulating `fpm` command lines.
!!
!! Provides utilities to split an `fpm <subcmd> ...` command into a prefix and
!! remainder, and to inject target names into the correct position while
!! preserving a `--` delimiter for arguments passed to the executed program.
!!
!! These utilities operate on *plain strings* and assume a space-delimited
!! token structure. They do not implement general shell parsing.
module watch_cmdsplice
   implicit none
   private
   public split_cmd_after_subcmd, inject_names_into_cmd

contains

   !> Split a command line into `<fpm subcmd>` prefix and remaining arguments.
   !!
   !! This is used to support name injection for `fpm run`/`fpm test`:
   !! the injected names are inserted after `prefix` and before the rest,
   !! while respecting a `--` delimiter.
   subroutine split_cmd_after_subcmd(full, prefix, rest)
      character(len=*), intent(in) :: full
      !! Full command string.
      character(len=:), allocatable, intent(out) :: prefix
      !! Prefix up to and including the subcommand token.
      character(len=:), allocatable, intent(out) :: rest
      !! Remaining arguments after the subcommand.

      character(len=:), allocatable :: s
      integer :: p_fpm_end, p_sub_start, p_sub_end

      s = adjustl(trim(full))
      prefix = ""
      rest   = ""

      if (len_trim(s) == 0) return
      if (.not. starts_with_token(s, "fpm")) return

      p_fpm_end = token_end(s, 1)
      p_sub_start = next_token_start(s, p_fpm_end + 1)
      if (p_sub_start == 0) return
      p_sub_end = token_end(s, p_sub_start)

      prefix = trim(s(1:p_sub_end))
      if (p_sub_end < len_trim(s)) then
         rest = adjustl(s(p_sub_end+1:))
      else
         rest = ""
      end if

   contains

      pure logical function starts_with_token(str, tok) result(ok)
         character(len=*), intent(in) :: str, tok
         integer :: e, n
         ok = .false.
         n = len_trim(tok)
         if (n == 0) return
         if (len_trim(str) < n) return
         if (str(1:n) /= tok(1:n)) return
         e = token_end(str, 1)
         ok = (e == n)
      end function starts_with_token

      pure integer function next_token_start(str, i) result(pos)
         character(len=*), intent(in) :: str
         integer, intent(in) :: i
         integer :: k, n
         pos = 0
         n = len_trim(str)
         k = max(1, i)
         do while (k <= n)
            if (str(k:k) /= " " .and. str(k:k) /= char(9)) then
               pos = k
               return
            end if
            k = k + 1
         end do
      end function next_token_start

      pure integer function token_end(str, i) result(pos)
         character(len=*), intent(in) :: str
         integer, intent(in) :: i
         integer :: k, n
         pos = 0
         n = len_trim(str)
         if (i < 1 .or. i > n) return
         k = i
         do while (k <= n)
            if (str(k:k) == " " .or. str(k:k) == char(9)) then
               pos = k - 1
               return
            end if
            k = k + 1
         end do
         pos = n
      end function token_end

   end subroutine split_cmd_after_subcmd

   !> Inject target names into a command line while respecting a `--` delimiter.
   !!
   !! `fpm run` and `fpm test` accept optional name patterns. This helper
   !! inserts those names into the right location:
   !! - after the `prefix` and any existing `fpm` options,
   !! - before a `--` delimiter (if present), so that program arguments remain
   !!   attached to the executed program.
   function inject_names_into_cmd(prefix, rest, names) result(out)
      character(len=*), intent(in) :: prefix
      !! Prefix up to and including the subcommand.
      character(len=*), intent(in) :: rest
      !! Remaining arguments after the subcommand.
      character(len=*), intent(in) :: names
      !! Space-separated names to inject.
      character(len=:), allocatable :: out

      character(len=:), allocatable :: p, r, n, before, after
      integer :: dd

      p = trim(prefix)
      r = adjustl(trim(rest))
      n = trim(names)

      if (len_trim(p) == 0) then
         out = ""
         return
      end if

      if (len_trim(n) == 0) then
         if (len_trim(r) == 0) then
            out = p
         else
            out = p // " " // r
         end if
         return
      end if

      dd = find_double_dash_delim(r)
      if (dd > 0) then
         if (dd > 1) then
            before = trim(r(1:dd-1))
         else
            before = ""
         end if
         after = trim(adjustl(r(dd:)))
      else
         before = trim(r)
         after = ""
      end if

      out = p
      if (len_trim(before) > 0) out = out // " " // before
      out = out // " " // n
      if (len_trim(after) > 0) out = out // " " // after

   contains

      pure integer function find_double_dash_delim(str) result(pos)
         character(len=*), intent(in) :: str
         integer :: i, n
         pos = 0
         n = len_trim(str)
         if (n < 2) return
         do i = 1, n-1
            if (str(i:i+1) == "--") then
               if (is_boundary(str, i-1) .and. is_boundary(str, i+2)) then
                  pos = i
                  return
               end if
            end if
         end do
      end function find_double_dash_delim

      pure logical function is_boundary(str, j) result(ok)
         character(len=*), intent(in) :: str
         integer, intent(in) :: j
         integer :: n
         n = len_trim(str)
         if (j < 1) then
            ok = .true.
         else if (j > n) then
            ok = .true.
         else
            ok = (str(j:j) == " " .or. str(j:j) == char(9))
         end if
      end function is_boundary

   end function inject_names_into_cmd

end module watch_cmdsplice
