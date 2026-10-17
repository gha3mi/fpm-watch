!> Reject incomplete worker responses and preserve native snapshot bytes.
program check_graph_worker
   use, intrinsic :: iso_fortran_env, only: int64
   use watch_graph_worker, only: read_graph_response
   use watch_platform, only: create_temp_directory, remove_temp_directory
   use fpm_filesystem, only: join_path
   use watch_types, only: root_info_t
   use fpm_strings, only: string_t
   use fpm_error, only: error_t
   implicit none
   type(string_t), allocatable :: lines(:), files(:)
   type(root_info_t), allocatable :: roots(:)
   integer(int64), allocatable :: masks(:)
   real :: total, model, targets, watch
   type(error_t), allocatable :: error
   character(len=:), allocatable :: workspace, path
   integer :: i, unit

   call create_temp_directory(workspace, error)
   if (allocated(error)) error stop "cannot reserve response workspace"
   path = join_path(workspace, "response.toml")
   open(newunit=unit, file=path, status="new", action="write")
   close(unit)
   call read_response()
   if (.not. allocated(error)) error stop "empty worker response accepted"
   lines = [string_t("version = 2"), string_t("success = true"), &
            string_t('files = ["7372632f612e663930"]'), string_t("file-masks = [1]"), &
            string_t('root-names = ["617070"]'), string_t("root-masks = [1]"), &
            string_t("timings = [0.4, 0.1, 0.2, 0.1]")]
   call write_response()
   call read_response()
   if (allocated(error)) error stop "valid worker response rejected"
   if (size(files) /= 1 .or. size(roots) /= 1 .or. size(masks) /= 1) error stop "response shape changed"
   if (files(1)%s /= "src/a.f90" .or. roots(1)%name /= "app") error stop "response bytes changed"
   if (masks(1) /= 1_int64 .or. roots(1)%mask /= 1_int64) error stop "response masks changed"
   if (abs(total - 0.4) > 1.0e-6) error stop "response timing changed"

   do i = 1, size(lines)
      call reject_line(i, "")
   end do
   call reject_line(1, 'version = "1"')
   call reject_line(1, "version = 1")
   call reject_line(2, 'success = "true"')
   call reject_line(3, 'files = ["7"]')
   call reject_line(3, 'files = ["zz"]')
   call reject_line(3, 'files = [""]')
   call reject_line(3, 'files = ["00"]')
   call reject_line(3, "files = [1]")
   call reject_line(4, "file-masks = []")
   call reject_line(4, "file-masks = [1.0]")
   call reject_line(4, "file-masks = [2]")
   call reject_line(5, 'root-names = ["0"]')
   call reject_line(5, 'root-names = [""]')
   call reject_line(6, "root-masks = []")
   call reject_line(6, "root-masks = [0]")
   call reject_line(6, "root-masks = [3]")
   call reject_line(7, "timings = [0.4, 0.1, 0.2]")
   call reject_line(7, "timings = [nan, 0.1, 0.2, 0.1]")
   call reject_line(7, "timings = [inf, 0.1, 0.2, 0.1]")
   call reject_line(7, "timings = [-0.4, 0.1, 0.2, 0.1]")
   call reject_line(7, 'timings = ["slow", 0.1, 0.2, 0.1]')

   lines(3)%s = 'files = ["ff0a225c"]'
   call write_response()
   call read_response()
   if (allocated(error)) error stop "native non-UTF8 path rejected"
   if (files(1)%s /= achar(255) // achar(10) // '"' // achar(92)) error stop "native path was corrupted"

   lines = [string_t("version = 2"), string_t("success = true"), &
            string_t("files = []"), string_t("file-masks = []"), &
            string_t("root-names = []"), string_t("root-masks = []"), &
            string_t("timings = [0.0, 0.0, 0.0, 0.0]")]
   call write_response()
   call read_response()
   if (allocated(error)) error stop "empty valid snapshot rejected"
   if (size(files) /= 0 .or. size(roots) /= 0) error stop "empty snapshot changed"

   lines = [string_t("version = 2"), string_t("success = false"), string_t('error = "6661696c6564"')]
   call write_response()
   call read_response()
   if (.not. allocated(error)) error stop "worker failure accepted"
   if (error%message /= "failed") error stop "worker error message changed"
   call reject_line(3, 'error = ""')
   call reject_line(3, 'error = "zz"')
   call reject_line(3, "")
   call remove_temp_directory(workspace)
   call read_response()
   if (.not. allocated(error)) error stop "missing worker response accepted"
   print *, "Graph worker response checks passed"

contains

   !> Read every protocol output, reusing storage to expose partial acceptance.
   subroutine read_response()
      call read_graph_response(path, files, masks, roots, total, model, targets, watch, error)
   end subroutine read_response

   !> Rewrite the fixture response inside its private temporary workspace.
   subroutine write_response()
      integer :: unit, j
      open(newunit=unit, file=path, status="old", action="write")
      do j = 1, size(lines)
         write(unit, "(a)") lines(j)%s
      end do
      close(unit)
   end subroutine write_response

   !> Require malformed fields to return an error without any accepted output arrays.
   subroutine reject_line(index, replacement)
      integer, intent(in) :: index
      character(len=*), intent(in) :: replacement
      character(len=:), allocatable :: original
      original = lines(index)%s
      lines(index)%s = replacement
      call write_response()
      call read_response()
      if (.not. allocated(error)) error stop "malformed worker response accepted"
      if (allocated(files) .or. allocated(masks) .or. allocated(roots)) error stop "partial worker response accepted"
      lines(index)%s = original
   end subroutine reject_line

end program check_graph_worker
