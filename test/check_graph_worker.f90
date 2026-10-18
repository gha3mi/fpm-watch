!> Reject incomplete worker responses and preserve native snapshot bytes.
program check_graph_worker
   use, intrinsic :: iso_fortran_env, only: int64
   use watch_graph_worker, only: read_graph_response, write_graph_response
   use watch_platform, only: create_temp_directory, remove_temp_directory
   use fpm_filesystem, only: join_path
   use watch_types, only: root_info_t
   use fpm_strings, only: string_t, str
   use fpm_error, only: error_t, fatal_error
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
   call check_streamed_response()
   call remove_temp_directory(workspace)
   call read_response()
   if (.not. allocated(error)) error stop "missing worker response accepted"
   print *, "Graph worker response checks passed"

contains

   !> Round-trip a large streamed snapshot, native bytes, high mask bits, and worker failures.
   subroutine check_streamed_response()
      type(string_t), allocatable :: source_files(:), decoded_files(:)
      type(root_info_t), allocatable :: source_roots(:), decoded_roots(:)
      integer(int64), allocatable :: source_masks(:), decoded_masks(:)
      type(error_t), allocatable :: graph_error, write_error, read_error
      character(len=:), allocatable :: stream_path, native_bytes
      integer :: j
      real, parameter :: timings(4) = [0.125, 0.25, 0.5, 1.0]
      real :: actual_timings(4)

      native_bytes = repeat(" ", 255)
      do j = 1, 255
         native_bytes(j:j) = achar(j)
      end do
      allocate(source_files(10000), source_masks(10000), source_roots(64))
      do j = 1, size(source_roots)
         source_roots(j)%name = "root/" // str(j) // native_bytes
         source_roots(j)%mask = ibset(0_int64, j - 1)
      end do
      do j = 1, size(source_files)
         source_files(j)%s = "src/" // str(j) // "/" // repeat("long-path/", 10) // "tail  "
         source_masks(j) = ior(source_roots(1)%mask, source_roots(64)%mask)
      end do
      source_files(1)%s = native_bytes
      stream_path = join_path(workspace, "streamed.toml")
      call write_graph_response(stream_path, source_files, source_masks, source_roots, timings, graph_error, write_error)
      if (allocated(write_error)) error stop "cannot write large streamed response"
      call read_graph_response(stream_path, decoded_files, decoded_masks, decoded_roots, &
         actual_timings(1), actual_timings(2), actual_timings(3), actual_timings(4), read_error)
      if (allocated(read_error)) error stop "cannot read large streamed response"
      if (size(decoded_files) /= size(source_files)) error stop "streamed file count changed"
      if (size(decoded_roots) /= size(source_roots)) error stop "streamed root count changed"
      if (any(decoded_masks /= source_masks)) error stop "streamed high-bit masks changed"
      if (any(abs(actual_timings - timings) > 1.0e-6)) error stop "streamed timings changed"
      do j = 1, size(source_files)
         if (decoded_files(j)%s /= source_files(j)%s) error stop "streamed native path changed"
      end do
      do j = 1, size(source_roots)
         if (decoded_roots(j)%name /= source_roots(j)%name) error stop "streamed native root changed"
         if (decoded_roots(j)%mask /= source_roots(j)%mask) error stop "streamed root mask changed"
      end do
      call write_graph_response(stream_path, source_files, source_masks, source_roots, timings, graph_error, write_error)
      if (.not. allocated(write_error)) error stop "streamed writer overwrote an existing response"

      deallocate(source_files, source_masks, source_roots)
      allocate(source_files(0), source_masks(0), source_roots(0))
      stream_path = join_path(workspace, "streamed-empty.toml")
      call write_graph_response(stream_path, source_files, source_masks, source_roots, timings, graph_error, write_error)
      if (allocated(write_error)) error stop "cannot write empty streamed response"
      call read_graph_response(stream_path, decoded_files, decoded_masks, decoded_roots, &
         actual_timings(1), actual_timings(2), actual_timings(3), actual_timings(4), read_error)
      if (allocated(read_error)) error stop "cannot read empty streamed response"
      if (size(decoded_files) /= 0 .or. size(decoded_roots) /= 0) error stop "empty streamed arrays changed"

      deallocate(source_files, source_masks, source_roots)
      call fatal_error(graph_error, "worker failure/" // native_bytes)
      stream_path = join_path(workspace, "streamed-error.toml")
      call write_graph_response(stream_path, source_files, source_masks, source_roots, timings, graph_error, write_error)
      if (allocated(write_error)) error stop "cannot write streamed worker failure"
      call read_graph_response(stream_path, decoded_files, decoded_masks, decoded_roots, &
         actual_timings(1), actual_timings(2), actual_timings(3), actual_timings(4), read_error)
      if (.not. allocated(read_error)) error stop "streamed worker failure accepted"
      if (read_error%message /= graph_error%message) error stop "streamed native error changed"
      if (allocated(decoded_files) .or. allocated(decoded_masks) .or. allocated(decoded_roots)) &
         error stop "streamed worker failure accepted partial arrays"
   end subroutine check_streamed_response

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
