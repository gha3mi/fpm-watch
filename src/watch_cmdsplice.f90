!> Helpers for manipulating `fpm` command lines.
!!
!! Provides utilities to split an `fpm <subcmd> ...` command into a prefix and
!! remainder, and to inject target names into the correct position while
!! preserving a `--` delimiter for arguments passed to the executed program.
!!
!! These utilities operate on command strings serialized by `quote_arg()`.
!! Delimiter detection respects quoted argv tokens and their escaped quotes.
module watch_cmdsplice
   use watch_cmdline, only: is_windows_os
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

      dd = find_double_dash_delim(r, is_windows_os())
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

      !> Locate a complete `--` argv token, including its serialized quotes.
      pure integer function find_double_dash_delim(str, windows) result(pos)
         character(len=*), intent(in) :: str
         logical, intent(in) :: windows
         integer :: i, n, first
         character :: quote, ch
         pos = 0
         n = len_trim(str)
         i = 1
         do while (i <= n)
            if (is_space(str(i:i))) then
               i = i + 1
               cycle
            end if
            first = i
            quote = achar(0)
            do while (i <= n)
               ch = str(i:i)
               if (quote == achar(0)) then
                  if (is_space(ch)) exit
                  if (ch == '"' .or. (ch == "'" .and. .not. windows)) quote = ch
               else if (ch == quote) then
                  if (windows .and. quote == '"' .and. i < n) then
                     if (str(i+1:i+1) == '"') then
                        i = i + 2
                        cycle
                     end if
                  end if
                  quote = achar(0)
               end if
               if (.not. windows .and. ch == achar(92) .and. quote /= "'") then
                  i = i + 1
               end if
               i = i + 1
            end do
            select case (str(first:min(i-1, n)))
             case ("--", "'--'", '"--"')
               pos = first
               return
             case default
               continue
            end select
         end do
      end function find_double_dash_delim

      pure logical function is_space(ch) result(ok)
         character, intent(in) :: ch
         ok = ch == " " .or. ch == char(9)
      end function is_space

   end function inject_names_into_cmd

end module watch_cmdsplice
