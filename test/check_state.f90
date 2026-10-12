!> Regression checks for watch-list changes discovered during rescans.
program check_state
   use, intrinsic :: iso_fortran_env, only: int64
   use fpm_strings, only: string_t
   use watch_fingerprint, only: watch_state_changed
   implicit none
   type(string_t), allocatable :: old_files(:), new_files(:)
   integer(int64), allocatable :: old_fp(:), new_fp(:)

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
   print *, "Watch snapshot checks passed"
end program check_state
