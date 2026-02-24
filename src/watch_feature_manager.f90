!> Feature plugin manager.
!!
!! Provides storage and fan-out calls to a set of enabled feature plugins.
!!
!! The manager owns a growable array of feature objects. Calls are forwarded
!! to each feature in insertion order.
module watch_feature_manager
   use, intrinsic :: iso_fortran_env, only: int64
   use watch_feature_api, only: watch_feature_t
   use watch_types, only: watch_config_t, root_info_t
   use fpm_strings, only: string_t
   implicit none
   private
   public feature_manager_t

   !> A thin wrapper to store an allocatable polymorphic feature instance.
   type feature_box_t
      class(watch_feature_t), allocatable :: p
   end type feature_box_t

   !> Container for multiple feature instances and their callback fan-out.
   type feature_manager_t
      type(feature_box_t), allocatable :: feat(:)
   contains
      procedure :: add => fm_add
      procedure :: init_all
      procedure :: on_start_all
      procedure :: on_watch_list_built_all
      procedure :: on_manifest_changed_all
      procedure :: on_change_detected_all
      procedure :: on_before_run_all
      procedure :: on_after_run_all
   end type feature_manager_t

contains

   !> Add a feature instance to the manager, transferring ownership.
   !!
   !! The `f` argument is moved into internal storage using `move_alloc`.
   subroutine fm_add(self, f)
      class(feature_manager_t), intent(inout) :: self
      class(watch_feature_t), allocatable, intent(inout) :: f
      type(feature_box_t), allocatable :: tmp(:)
      integer :: n, i

      if (.not. allocated(self%feat)) then
         allocate(self%feat(1))
         call move_alloc(f, self%feat(1)%p)
         return
      end if

      n = size(self%feat)
      allocate(tmp(n+1))
      do i = 1, n
         call move_alloc(self%feat(i)%p, tmp(i)%p)
      end do
      call move_alloc(f, tmp(n+1)%p)
      call move_alloc(tmp, self%feat)
   end subroutine fm_add

   !> Call `init(cfg)` on all registered features.
   subroutine init_all(self, cfg)
      class(feature_manager_t), intent(inout) :: self
      type(watch_config_t), intent(in) :: cfg
      integer :: i
      if (.not. allocated(self%feat)) return
      do i = 1, size(self%feat)
         if (allocated(self%feat(i)%p)) call self%feat(i)%p%init(cfg)
      end do
   end subroutine init_all

   !> Call `on_start()` on all registered features.
   subroutine on_start_all(self)
      class(feature_manager_t), intent(inout) :: self
      integer :: i
      if (.not. allocated(self%feat)) return
      do i = 1, size(self%feat)
         if (allocated(self%feat(i)%p)) call self%feat(i)%p%on_start()
      end do
   end subroutine on_start_all

   !> Call `on_watch_list_built(files, roots)` on all registered features.
   subroutine on_watch_list_built_all(self, files, roots)
      class(feature_manager_t), intent(inout) :: self
      type(string_t), allocatable, intent(inout) :: files(:)
      type(root_info_t), allocatable, intent(inout) :: roots(:)
      integer :: i
      if (.not. allocated(self%feat)) return
      do i = 1, size(self%feat)
         if (allocated(self%feat(i)%p)) call self%feat(i)%p%on_watch_list_built(files, roots)
      end do
   end subroutine on_watch_list_built_all

   !> Call `on_manifest_changed(old_key, new_key)` on all registered features.
   subroutine on_manifest_changed_all(self, old_key, new_key)
      class(feature_manager_t), intent(inout) :: self
      integer(int64), intent(in) :: old_key, new_key
      integer :: i
      if (.not. allocated(self%feat)) return
      do i = 1, size(self%feat)
         if (allocated(self%feat(i)%p)) call self%feat(i)%p%on_manifest_changed(old_key, new_key)
      end do
   end subroutine on_manifest_changed_all

   !> Call `on_change_detected(changed)` on all registered features.
   subroutine on_change_detected_all(self, changed)
      class(feature_manager_t), intent(inout) :: self
      type(string_t), allocatable, intent(in) :: changed(:)
      integer :: i
      if (.not. allocated(self%feat)) return
      do i = 1, size(self%feat)
         if (allocated(self%feat(i)%p)) call self%feat(i)%p%on_change_detected(changed)
      end do
   end subroutine on_change_detected_all

   !> Call `on_before_run(cmd)` on all registered features.
   subroutine on_before_run_all(self, cmd)
      class(feature_manager_t), intent(inout) :: self
      character(len=:), allocatable, intent(inout) :: cmd
      integer :: i
      if (.not. allocated(self%feat)) return
      do i = 1, size(self%feat)
         if (allocated(self%feat(i)%p)) call self%feat(i)%p%on_before_run(cmd)
      end do
   end subroutine on_before_run_all

   !> Call `on_after_run(exitstat, seconds)` on all registered features.
   subroutine on_after_run_all(self, exitstat, seconds)
      class(feature_manager_t), intent(inout) :: self
      integer, intent(in) :: exitstat
      real, intent(in) :: seconds
      integer :: i
      if (.not. allocated(self%feat)) return
      do i = 1, size(self%feat)
         if (allocated(self%feat(i)%p)) call self%feat(i)%p%on_after_run(exitstat, seconds)
      end do
   end subroutine on_after_run_all

end module watch_feature_manager
