!> Keep release metadata and fpm's package version on the same source of truth.
program check_version
   use fpm_manifest, only: package_config_t, get_package_data
   use fpm_error, only: error_t
   implicit none
   type(package_config_t) :: package
   type(error_t), allocatable :: error
   character(len=128) :: version
   integer :: unit, status

   open(newunit=unit, file="VERSION", status="old", action="read", iostat=status)
   if (status /= 0) error stop "cannot read release VERSION"
   read(unit, "(a)", iostat=status) version
   close(unit)
   if (status /= 0) error stop "cannot read release version text"
   call get_package_data(package, "fpm.toml", error)
   if (allocated(error)) error stop "cannot read package version through fpm"
   if (package%version%s() /= trim(version)) error stop "manifest and release versions differ"
end program check_version
