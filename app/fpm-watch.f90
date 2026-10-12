!> Entry point for `fpm-watch`.
!!
!! This program optionally runs under a supervisor (auto-restart), parses the
!! command-line and project configuration, initializes the watcher, and runs
!! the watch loop.
!!
!! ## Execution flow
!! 1. `parse_cli_config()` builds configuration and selects the log format.
!! 2. `maybe_supervise()` optionally re-executes `fpm-watch` under a supervisor.
!! 3. `watcher_t%init()` initializes plugins, computes the initial watch list,
!!    and (optionally) runs the command once.
!! 4. `watcher_t%run()` enters the watch loop: sleep → scan → debounce → run.
!!
!! ## Low CPU mode
!! When low CPU mode is enabled (`--watch-low-cpu` or `low-cpu=true` in the
!! manifest), the watcher uses an OS sleep implementation for polling and
!! debouncing. This keeps idle CPU usage close to zero while waiting.
program fpm_watch
   use watch_restart, only: maybe_supervise
   use watch_cli,     only: parse_cli_config
   use watch_engine,  only: watcher_t
   use watch_types,   only: watch_config_t
   use watch_platform, only: termination_requested
   implicit none

   type(watch_config_t) :: cfg
   type(watcher_t)      :: w

   call parse_cli_config(cfg)
   call maybe_supervise(cfg%w%log_format)

   call w%init(cfg)
   call w%run()
   if (termination_requested()) stop 130
   if (cfg%w%once .and. w%last_exit /= 0) stop w%last_exit
end program fpm_watch
