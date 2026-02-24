!> Feature plugin API.
!!
!! Defines the base `watch_feature_t` type, which acts as a lightweight
!! callback interface. Feature implementations can extend this type and
!! override any subset of callbacks.
!!
!! ### Callback order (typical)
!! - `init(cfg)` once during watcher init
!! - `on_start()` once after init
!! - `on_watch_list_built(files, roots)` after watch list computation (and on rebuild)
!! - `on_manifest_changed(old_key, new_key)` when the manifest requires rebuild
!! - `on_change_detected(changed)` when file changes are detected
!! - `on_before_run(cmd)` just before running the command
!! - `on_after_run(exitstat, seconds)` after command completion
!!
!! Default implementations are no-ops. Dummy references are used where needed
!! to avoid compiler warnings about unused dummy arguments.
module watch_feature_api
   use, intrinsic :: iso_fortran_env, only: int64
   use watch_types, only: watch_config_t, root_info_t
   use fpm_strings, only: string_t
   implicit none
   private
   public watch_feature_t

   !> Base feature type (override callbacks to implement behavior).
   type watch_feature_t
   contains
      procedure :: init                => feat_init_noop
      procedure :: on_start            => feat_noop
      procedure :: on_watch_list_built => feat_list_noop
      procedure :: on_manifest_changed => feat_manifest_noop
      procedure :: on_change_detected  => feat_change_noop
      procedure :: on_before_run       => feat_before_noop
      procedure :: on_after_run        => feat_after_noop
   end type watch_feature_t

contains

   !> No-op init callback.
   subroutine feat_init_noop(self, cfg)
      class(watch_feature_t), intent(inout) :: self
      type(watch_config_t),  intent(in)     :: cfg
      if (.false.) then
         if (cfg%w%verbosity < -1) continue
      end if
   end subroutine feat_init_noop

   !> No-op start callback.
   subroutine feat_noop(self)
      class(watch_feature_t), intent(inout) :: self
   end subroutine feat_noop

   !> No-op callback for watch list rebuild completion.
   subroutine feat_list_noop(self, files, roots)
      class(watch_feature_t), intent(inout) :: self
      type(string_t), allocatable, intent(inout) :: files(:)
      type(root_info_t), allocatable, intent(inout) :: roots(:)
      if (.false.) then
         if (allocated(files)) continue
         if (allocated(roots)) continue
      end if
   end subroutine feat_list_noop

   !> No-op callback for manifest key change.
   subroutine feat_manifest_noop(self, old_key, new_key)
      class(watch_feature_t), intent(inout) :: self
      integer(int64), intent(in) :: old_key, new_key
      if (.false.) then
         if (old_key == new_key) continue
      end if
   end subroutine feat_manifest_noop

   !> No-op callback for detected file changes.
   subroutine feat_change_noop(self, changed)
      class(watch_feature_t), intent(inout) :: self
      type(string_t), allocatable, intent(in) :: changed(:)
      if (.false.) then
         if (allocated(changed)) continue
      end if
   end subroutine feat_change_noop

   !> No-op callback invoked just before executing the command.
   subroutine feat_before_noop(self, cmd)
      class(watch_feature_t), intent(inout) :: self
      character(len=:), allocatable, intent(inout) :: cmd
      if (.false.) then
         if (allocated(cmd)) continue
      end if
   end subroutine feat_before_noop

   !> No-op callback invoked after executing the command.
   subroutine feat_after_noop(self, exitstat, seconds)
      class(watch_feature_t), intent(inout) :: self
      integer, intent(in) :: exitstat
      real,    intent(in) :: seconds
      if (.false.) then
         if (exitstat /= 0) continue
         if (seconds < 0.0) continue
      end if
   end subroutine feat_after_noop

end module watch_feature_api
