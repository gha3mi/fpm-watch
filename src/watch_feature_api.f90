!> Synchronous watcher-plugin callbacks with overridable no-op defaults.
!!
!! Initialization calls `init`, then `on_start`, then `on_watch_list_built`.
!! Successful refreshes call `on_watch_list_built` again. Manifest refreshes
!! additionally call `on_manifest_changed` only if the manifest/settings key
!! differs. Ordinary source changes call `on_change_detected` before execution;
!! every command calls `on_before_run`, then `on_after_run`, including failures.
!!
!! Watch-list and command callbacks receive mutable arguments. Callbacks must
!! preserve file/root ordering and membership because per-file masks are managed
!! separately by the engine. Plugins are enabled at initialization, not refreshed
!! when manifest plugin settings change.
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

   !> No-op watch-list callback; overrides must preserve file/root alignment with engine masks.
   subroutine feat_list_noop(self, files, roots)
      class(watch_feature_t), intent(inout) :: self
      type(string_t), allocatable, intent(inout) :: files(:)
      !! Accepted path list; overrides must preserve alignment with the engine-owned per-file masks.
      type(root_info_t), allocatable, intent(inout) :: roots(:)
      !! Root names/bits; overrides must preserve membership represented in file masks.
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

   !> No-op command callback; overrides may replace the allocated command before execution.
   subroutine feat_before_noop(self, cmd)
      class(watch_feature_t), intent(inout) :: self
      character(len=:), allocatable, intent(inout) :: cmd
      !! Allocated command string that an override may modify before it is executed.
      if (.false.) then
         if (allocated(cmd)) continue
      end if
   end subroutine feat_before_noop

   !> No-op completion callback receiving the command status and elapsed seconds, including failures.
   subroutine feat_after_noop(self, exitstat, seconds)
      class(watch_feature_t), intent(inout) :: self
      integer, intent(in) :: exitstat
      !! Command exit status, including failures and interruptions.
      real,    intent(in) :: seconds
      !! Measured duration of the command in seconds.
      if (.false.) then
         if (exitstat /= 0) continue
         if (seconds < 0.0) continue
      end if
   end subroutine feat_after_noop

end module watch_feature_api
