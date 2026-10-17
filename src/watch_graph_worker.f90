!> Isolate fpm model construction in a short-lived worker process.
!!
!! fpm owns resources that cannot all be released through its public model API.
!! Each refresh therefore runs in the actual watcher executable and returns one
!! validated TOML snapshot. A private workspace also contains fpm probe files;
!! the parent removes the whole workspace after success, failure, or cancellation. Byte strings use hexadecimal encoding so native paths
!! survive control bytes and invalid UTF-8 without relying on TOML string encoding.
module watch_graph_worker
   use, intrinsic :: iso_fortran_env, only: int64
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   use watch_types, only: watch_config_t, root_info_t
   use watch_cmdline, only: get_arg, quote_arg
   use watch_platform, only: create_temp_directory, remove_temp_directory, set_temp_directory, execute_command, current_executable
   use watch_fpm_graph, only: compute_watch_files_from_settings
   use watch_log, only: log_event
   use fpm_strings, only: string_t, str
   use fpm_error, only: error_t, fatal_error
   use fpm_os, only: change_directory
   use fpm_filesystem, only: join_path, dirname
   use fpm_toml, only: toml_table, toml_array, toml_load, toml_error, new_table, add_array, len
   use tomlf, only: get_value, set_value, toml_dump, toml_stat
   implicit none
   private
   public compute_watch_snapshot, run_graph_worker, read_graph_response

   integer, parameter :: RESPONSE_VERSION = 2

contains

   !> Launch the watcher itself with original argv and accept only a complete response.
   subroutine compute_watch_snapshot(cfg, files, file_mask, roots, &
                                    build_secs, secs_model, secs_targets, secs_watch, error)
      type(watch_config_t), intent(inout) :: cfg
      type(string_t), allocatable, intent(out) :: files(:)
      integer(int64), allocatable, intent(out) :: file_mask(:)
      type(root_info_t), allocatable, intent(out) :: roots(:)
      real, intent(out) :: build_secs, secs_model, secs_targets, secs_watch
      type(error_t), allocatable, intent(out) :: error
      character(len=:), allocatable :: workspace, response_path, command, format, executable
      integer :: i, status

      build_secs = 0.0
      secs_model = 0.0
      secs_targets = 0.0
      secs_watch = 0.0
      executable = current_executable(error)
      if (allocated(error)) return
      call create_temp_directory(workspace, error)
      if (allocated(error)) return
      response_path = join_path(workspace, "response.toml")
      format = "text"
      if (allocated(cfg%w%log_format)) format = cfg%w%log_format
      command = quote_arg(executable) // " --watch-graph-worker " // quote_arg(response_path) // " " // quote_arg(format)
      do i = 1, command_argument_count()
         command = command // " " // quote_arg(get_arg(i))
      end do
      call execute_command(command, status)
      if (status == 0) then
         call read_graph_response(response_path, files, file_mask, roots, &
                                  build_secs, secs_model, secs_targets, secs_watch, error)
      else
         call fatal_error(error, "Watch graph worker exited with status " // str(status))
      end if
      call remove_temp_directory(workspace)
   end subroutine compute_watch_snapshot

   !> Change to the selected project, compute its graph, and write a worker response.
   subroutine run_graph_worker(cfg, response_path)
      type(watch_config_t), intent(inout) :: cfg
      character(len=*), intent(in) :: response_path
      type(string_t), allocatable :: files(:)
      integer(int64), allocatable :: file_mask(:)
      type(root_info_t), allocatable :: roots(:)
      type(error_t), allocatable :: error, write_error
      real :: build_secs, secs_model, secs_targets, secs_watch

      build_secs = 0.0
      secs_model = 0.0
      secs_targets = 0.0
      secs_watch = 0.0
      call set_temp_directory(dirname(response_path), error)
      if (.not. allocated(error)) then
         if (allocated(cfg%project_dir)) then
            call change_directory(cfg%project_dir, error)
         else
            call fatal_error(error, "Watch graph worker has no project directory")
         end if
      end if
      if (.not. allocated(error)) then
         call compute_watch_files_from_settings(cfg%settings, cfg%w, files, file_mask, roots, &
                                               build_secs, secs_model, secs_targets, secs_watch, error, &
                                               metadata_files=cfg%response_files)
      end if
      call write_graph_response(response_path, files, file_mask, roots, &
                                [build_secs, secs_model, secs_targets, secs_watch], error, write_error)
      if (allocated(write_error)) then
         call log_event("error", "graph.worker_failed", write_error%message)
         stop 1
      end if
   end subroutine run_graph_worker

   !> Write one versioned TOML response using fpm's TOML dependency.
   subroutine write_graph_response(path, files, file_mask, roots, timings, graph_error, error)
      character(len=*), intent(in) :: path
      type(string_t), allocatable, intent(in) :: files(:)
      integer(int64), allocatable, intent(in) :: file_mask(:)
      type(root_info_t), allocatable, intent(in) :: roots(:)
      real, intent(in) :: timings(4)
      type(error_t), allocatable, intent(in) :: graph_error
      type(error_t), allocatable, intent(out) :: error
      type(toml_table) :: table
      type(toml_array), pointer :: array
      type(toml_error), allocatable :: serialization_error
      integer :: i, unit, status, close_status
      character(len=512) :: message

      call new_table(table)
      call set_value(table, "version", RESPONSE_VERSION)
      call set_value(table, "success", .not. allocated(graph_error))
      if (allocated(graph_error)) then
         call set_value(table, "error", encode_bytes(graph_error%message))
      else
         call add_array(table, "files", array)
         do i = 1, size(files)
            call set_value(array, i, encode_bytes(files(i)%s))
         end do
         call add_array(table, "file-masks", array)
         do i = 1, size(file_mask)
            call set_value(array, i, file_mask(i))
         end do
         call add_array(table, "root-names", array)
         do i = 1, size(roots)
            call set_value(array, i, encode_bytes(roots(i)%name))
         end do
         call add_array(table, "root-masks", array)
         do i = 1, size(roots)
            call set_value(array, i, roots(i)%mask)
         end do
         call add_array(table, "timings", array)
         do i = 1, size(timings)
            call set_value(array, i, timings(i))
         end do
      end if
      open(newunit=unit, file=path, status="new", action="write", iostat=status, iomsg=message)
      if (status /= 0) then
         call fatal_error(error, "Cannot write watch graph response: " // trim(message))
         return
      end if
      call toml_dump(table, unit, serialization_error)
      close(unit, iostat=close_status, iomsg=message)
      if (allocated(serialization_error)) then
         call fatal_error(error, "Cannot serialize watch graph response: " // serialization_error%message)
      else if (close_status /= 0) then
         call fatal_error(error, "Cannot close watch graph response: " // trim(message))
      end if
   end subroutine write_graph_response

   !> Validate version, fields, alignment, masks, paths, and timings before accepting a response.
   subroutine read_graph_response(path, files, file_mask, roots, &
                                  build_secs, secs_model, secs_targets, secs_watch, error)
      character(len=*), intent(in) :: path
      type(string_t), allocatable, intent(out) :: files(:)
      integer(int64), allocatable, intent(out) :: file_mask(:)
      type(root_info_t), allocatable, intent(out) :: roots(:)
      real, intent(out) :: build_secs, secs_model, secs_targets, secs_watch
      type(error_t), allocatable, intent(out) :: error
      type(toml_table), allocatable :: table
      type(toml_array), pointer :: paths, masks, names, root_masks, times
      type(toml_error), allocatable :: parse_error
      type(string_t), allocatable :: candidate_files(:)
      integer(int64), allocatable :: candidate_masks(:)
      type(root_info_t), allocatable :: candidate_roots(:)
      character(len=:), allocatable :: encoded, decoded
      character(len=512) :: message
      integer :: unit, status, version, i, nfiles, nroots
      integer(int64) :: allowed_mask
      real :: timings(4)
      logical :: success, valid

      build_secs = 0.0
      secs_model = 0.0
      secs_targets = 0.0
      secs_watch = 0.0
      open(newunit=unit, file=path, status="old", action="read", iostat=status, iomsg=message)
      if (status /= 0) then
         call fatal_error(error, "Cannot read watch graph response: " // trim(message))
         return
      end if
      call toml_load(table, unit, error=parse_error)
      close(unit)
      if (allocated(parse_error)) then
         call invalid_response(error, "invalid TOML")
         return
      end if
      if (.not. allocated(table)) then
         call invalid_response(error, "empty response")
         return
      end if
      call get_value(table, "version", version, stat=status)
      if (status /= toml_stat%success) then
         call invalid_response(error, "missing version")
         return
      end if
      if (version /= RESPONSE_VERSION) then
         call invalid_response(error, "unsupported version")
         return
      end if
      call get_value(table, "success", success, stat=status)
      if (status /= toml_stat%success) then
         call invalid_response(error, "missing success state")
         return
      end if
      if (.not. success) then
         call get_value(table, "error", encoded, stat=status)
         if (status /= toml_stat%success .or. .not. allocated(encoded)) then
            call invalid_response(error, "missing error message")
            return
         end if
         call decode_bytes(encoded, decoded, valid)
         if (.not. valid .or. len(decoded) == 0) then
            call invalid_response(error, "invalid error message")
         else
            call fatal_error(error, decoded)
         end if
         return
      end if
      call get_value(table, "files", paths, requested=.false.)
      call get_value(table, "file-masks", masks, requested=.false.)
      call get_value(table, "root-names", names, requested=.false.)
      call get_value(table, "root-masks", root_masks, requested=.false.)
      call get_value(table, "timings", times, requested=.false.)
      if (.not. associated(paths) .or. .not. associated(masks) .or. .not. associated(names) .or. &
          .not. associated(root_masks) .or. .not. associated(times)) then
         call invalid_response(error, "missing snapshot arrays")
         return
      end if
      nfiles = len(paths)
      nroots = len(names)
      if (len(masks) /= nfiles .or. len(root_masks) /= nroots .or. len(times) /= 4 .or. nroots > 64) then
         call invalid_response(error, "misaligned snapshot arrays")
         return
      end if
      allocate(candidate_files(nfiles), candidate_masks(nfiles), candidate_roots(nroots))
      allowed_mask = 0_int64
      do i = 1, nroots
         call get_value(names, i, encoded, stat=status)
         if (status /= toml_stat%success) exit
         call decode_bytes(encoded, candidate_roots(i)%name, valid)
         if (.not. valid) exit
         if (len(candidate_roots(i)%name) == 0) exit
         call get_value(root_masks, i, candidate_roots(i)%mask, stat=status)
         if (status /= toml_stat%success) exit
         if (popcnt(candidate_roots(i)%mask) /= 1) exit
         if (iand(allowed_mask, candidate_roots(i)%mask) /= 0_int64) exit
         allowed_mask = ior(allowed_mask, candidate_roots(i)%mask)
      end do
      if (i <= nroots) then
         call invalid_response(error, "invalid root entry")
         return
      end if
      do i = 1, nfiles
         call get_value(paths, i, encoded, stat=status)
         if (status /= toml_stat%success) exit
         call decode_bytes(encoded, candidate_files(i)%s, valid)
         if (.not. valid) exit
         if (len(candidate_files(i)%s) == 0) exit
         call get_value(masks, i, candidate_masks(i), stat=status)
         if (status /= toml_stat%success) exit
         if (iand(candidate_masks(i), not(allowed_mask)) /= 0_int64) exit
      end do
      if (i <= nfiles) then
         call invalid_response(error, "invalid file entry")
         return
      end if
      do i = 1, 4
         call get_value(times, i, timings(i), stat=status)
         if (status /= toml_stat%success) exit
         if (.not. ieee_is_finite(timings(i))) exit
         if (timings(i) < 0.0) exit
      end do
      if (i <= 4) then
         call invalid_response(error, "invalid timing entry")
         return
      end if
      call move_alloc(candidate_files, files)
      call move_alloc(candidate_masks, file_mask)
      call move_alloc(candidate_roots, roots)
      build_secs = timings(1)
      secs_model = timings(2)
      secs_targets = timings(3)
      secs_watch = timings(4)
   end subroutine read_graph_response

   !> Report malformed worker data without accepting a partial snapshot.
   subroutine invalid_response(error, detail)
      type(error_t), allocatable, intent(out) :: error
      character(len=*), intent(in) :: detail
      call fatal_error(error, "Invalid watch graph worker response: " // detail)
   end subroutine invalid_response

   !> Encode arbitrary native bytes in portable ASCII TOML strings.
   pure function encode_bytes(value) result(encoded)
      character(len=*), intent(in) :: value
      character(len=:), allocatable :: encoded
      character(len=*), parameter :: digits = "0123456789abcdef"
      integer :: i, byte
      allocate(character(len=2 * len(value)) :: encoded)
      do i = 1, len(value)
         byte = iachar(value(i:i))
         encoded(2*i-1:2*i-1) = digits(byte / 16 + 1:byte / 16 + 1)
         encoded(2*i:2*i) = digits(mod(byte, 16) + 1:mod(byte, 16) + 1)
      end do
   end function encode_bytes

   !> Decode hexadecimal byte strings, rejecting malformed data and embedded NULs.
   pure subroutine decode_bytes(encoded, decoded, valid)
      character(len=*), intent(in) :: encoded
      character(len=:), allocatable, intent(out) :: decoded
      logical, intent(out) :: valid
      character(len=*), parameter :: digits = "0123456789abcdef"
      integer :: i, high, low, byte
      valid = .false.
      decoded = ""
      if (mod(len(encoded), 2) /= 0) return
      deallocate(decoded)
      allocate(character(len=len(encoded) / 2) :: decoded)
      do i = 1, len(decoded)
         high = index(digits, encoded(2*i-1:2*i-1)) - 1
         low = index(digits, encoded(2*i:2*i)) - 1
         if (high < 0 .or. low < 0) return
         byte = 16 * high + low
         if (byte == 0) return
         decoded(i:i) = achar(byte)
      end do
      valid = .true.
   end subroutine decode_bytes

end module watch_graph_worker
