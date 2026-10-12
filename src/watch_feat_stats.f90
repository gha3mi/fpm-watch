!> Example "stats" feature plugin.
!!
!! This feature demonstrates the plugin system by tracking the number of runs.
!! It currently does not print or export statistics; it simply increments an
!! internal counter after each run.
!!
!! The `exitstat` and `seconds` arguments are accepted to conform to the plugin
!! interface. Dummy references are used to prevent unused-argument warnings.
module watch_feat_stats
   use watch_feature_api, only: watch_feature_t
   use watch_types, only: watch_config_t
   use watch_log, only: log_event
   implicit none
   private
   public new_stats_feature

   !> A feature that counts how many command runs have completed.
   type, extends(watch_feature_t) :: stats_feature_t
      !> Completed command cycles, including failed or interrupted commands.
      integer :: runs = 0
   contains
      procedure :: init         => stats_init
      procedure :: on_after_run => stats_after
   end type stats_feature_t

contains

   !> Allocate a new stats feature instance.
   subroutine new_stats_feature(f)
      class(watch_feature_t), allocatable, intent(out) :: f
      allocate(stats_feature_t :: f)
   end subroutine new_stats_feature

   !> Initialize the stats feature.
   !!
   !! Currently logs a single "feature enabled" message.
   subroutine stats_init(self, cfg)
      class(stats_feature_t), intent(inout) :: self
      type(watch_config_t), intent(in) :: cfg
      self%runs = 0
      if (cfg%w%verbosity >= 1) call log_event("info", "feature.enabled", "Feature enabled: stats", verbosity=1)
   end subroutine stats_init

   !> Increment the internal completed-command counter for every outcome; no statistics are emitted.
   subroutine stats_after(self, exitstat, seconds)
      class(stats_feature_t), intent(inout) :: self
      integer, intent(in) :: exitstat
      real, intent(in) :: seconds
      self%runs = self%runs + 1
      if (.false.) then
         if (exitstat /= 0) continue
         if (seconds < 0.0) continue
      end if
   end subroutine stats_after

end module watch_feat_stats
