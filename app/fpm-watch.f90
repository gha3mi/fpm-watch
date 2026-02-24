!> Entry point for `fpm-watch`.
!!
!! This program optionally runs under a supervisor (auto-restart), parses the
!! command-line and project configuration, initializes the watcher, and runs
!! the watch loop.
!!
!! ## Execution flow
!! 1. `maybe_supervise()` optionally re-executes `fpm-watch` under a supervisor
!!    process when auto-restart is enabled.
!! 2. `parse_cli_config()` builds a `watch_config_t` from defaults, `fpm.toml`
!!    values, and command-line overrides.
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
   implicit none

   type(watch_config_t) :: cfg
   type(watcher_t)      :: w

   call maybe_supervise()

   call parse_cli_config(cfg)

   call w%init(cfg)
   call w%run()
end program fpm_watch
