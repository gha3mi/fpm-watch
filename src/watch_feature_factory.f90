!> Feature enablement and factory wiring.
!!
!! Maps feature names (strings) to concrete feature implementations.
!!
!! Feature selection is user-controlled via:
!! - `--watch-feature <name>` flags, and/or
!! - `features = ["..."]` in `fpm.toml` `[extra.fpm-watch]`.
module watch_feature_factory
   use watch_feature_manager, only: feature_manager_t
   use watch_types, only: watch_opts_t
   use watch_log, only: log_warn
   use watch_feat_stats, only: new_stats_feature
   use watch_feature_api, only: watch_feature_t
   implicit none
   private
   public enable_features

contains

   !> Enable features requested in watch options.
   !!
   !! Unknown feature names are ignored with a warning.
   subroutine enable_features(fm, w)
      type(feature_manager_t), intent(inout) :: fm
      type(watch_opts_t), intent(in) :: w
      integer :: i
      character(len=:), allocatable :: name

      if (.not. allocated(w%enabled_features)) return

      do i = 1, size(w%enabled_features)
         name = trim(w%enabled_features(i)%s)
         if (len_trim(name) == 0) cycle

         select case (name)
          case ("stats")
            call add_stats(fm)
          case default
            call log_warn(w, "unknown feature '" // name // "' (ignored)")
         end select
      end do
   end subroutine enable_features

   !> Factory helper: add the built-in `stats` feature.
   subroutine add_stats(fm)
      type(feature_manager_t), intent(inout) :: fm
      class(watch_feature_t), allocatable :: f
      call new_stats_feature(f)
      call fm%add(f)
   end subroutine add_stats

end module watch_feature_factory
