!> Regression checks for watch-list changes discovered during rescans.
program check_state
   use, intrinsic :: iso_fortran_env, only: int64
   use fpm_strings, only: string_t
   use watch_fingerprint, only: watch_state_changed, preserve_fingerprints
   implicit none
   type(string_t), allocatable :: old_files(:), new_files(:)
   integer(int64), allocatable :: old_fp(:), new_fp(:)
   integer :: i, n
   character(len=32) :: path

   if (watch_state_changed(old_files, old_fp, new_files, new_fp)) error stop "empty snapshots differ"
   allocate(old_files(2), new_files(2), old_fp(2), new_fp(2))
   old_files(1)%s = "src/a.f90"
   old_files(2)%s = "src/b.f90"
   old_fp = [11_int64, 22_int64]
   new_files(1)%s = "src/b.f90"
   new_files(2)%s = "src/a.f90"
   new_fp = [22_int64, 11_int64]
   if (watch_state_changed(old_files, old_fp, new_files, new_fp)) error stop "ordering triggered a run"
   new_fp(1) = 23_int64
   if (.not. watch_state_changed(old_files, old_fp, new_files, new_fp)) error stop "edit was lost"
   new_fp(1) = 22_int64
   new_files(1)%s = "src/c.f90"
   if (.not. watch_state_changed(old_files, old_fp, new_files, new_fp)) error stop "replacement was lost"
   new_files = old_files(1:1)
   new_fp = old_fp(1:1)
   if (.not. watch_state_changed(old_files, old_fp, new_files, new_fp)) error stop "removal was lost"
   if (.not. watch_state_changed(new_files, new_fp, old_files, old_fp)) error stop "addition was lost"
   deallocate(old_files, new_files, old_fp, new_fp)
   n = 5000
   allocate(old_files(n), new_files(n), old_fp(n), new_fp(n))
   do i = 1, n
      write(path, "(a,i8.8)") "src/file_", i
      old_files(i)%s = trim(path)
      old_fp(i) = int(i, int64)
   end do
   new_files = old_files(n:1:-1)
   new_fp = old_fp(n:1:-1)
   if (watch_state_changed(old_files, old_fp, new_files, new_fp)) error stop "large reversal changed snapshot"
   new_fp(n / 2) = -1_int64
   if (.not. watch_state_changed(old_files, old_fp, new_files, new_fp)) error stop "reordered edit was lost"
   new_files = old_files
   new_fp = old_fp
   if (watch_state_changed(old_files, old_fp, new_files, new_fp)) error stop "aligned snapshot changed"
   new_fp(n) = -1_int64
   if (.not. watch_state_changed(old_files, old_fp, new_files, new_fp)) error stop "aligned edit was lost"
   deallocate(new_fp)
   if (.not. watch_state_changed(old_files, old_fp, new_files, new_fp)) error stop "missing fingerprints accepted"
   allocate(new_fp(1))
   if (.not. watch_state_changed(old_files, old_fp, new_files, new_fp)) error stop "short fingerprints accepted"
   deallocate(old_files, new_files, old_fp, new_fp)
   old_files = [string_t("missing.inc"), string_t("empty.inc"), string_t("kept.f90")]
   old_fp = [0_int64, 1_int64, -123_int64]
   new_files = [string_t("new.f90"), string_t("kept.f90"), string_t("missing.inc"), string_t("empty.inc")]
   new_fp = [77_int64, 88_int64, 99_int64, 111_int64]
   call preserve_fingerprints(old_files, old_fp, new_files, new_fp)
   if (any(new_fp /= [77_int64, -123_int64, 0_int64, 1_int64])) error stop "mapped baselines changed"
   if (new_files(1)%s /= "new.f90") error stop "preservation reordered paths"
   new_files = old_files
   new_fp = [99_int64, 99_int64, 99_int64]
   call preserve_fingerprints(old_files, old_fp, new_files, new_fp)
   if (any(new_fp /= old_fp)) error stop "aligned baselines were not preserved"
   new_files = [string_t("kept.f90"), string_t("new.f90")]
   new_fp = [88_int64, 77_int64]
   call preserve_fingerprints(old_files, old_fp, new_files, new_fp)
   if (any(new_fp /= [-123_int64, 77_int64])) error stop "removal lost surviving baseline"
   deallocate(old_fp)
   new_fp = 55_int64
   call preserve_fingerprints(old_files, old_fp, new_files, new_fp)
   if (any(new_fp /= 55_int64)) error stop "missing old baselines changed new values"
   allocate(old_fp(1))
   call preserve_fingerprints(old_files, old_fp, new_files, new_fp)
   if (any(new_fp /= 55_int64)) error stop "malformed old baselines changed new values"
   print *, "Watch snapshot checks passed"
end program check_state
