!> Entry point for the watcher and its optional restart supervisor.
!!
!! Parse settings and select logging before entering supervisor mode. The watcher
!! initializes the watch list, optionally runs fpm, and enters the polling loop.
!! In one-shot mode the loop returns immediately; the program propagates the
!! command status, or exits successfully if no command ran. A latched command
!! interruption exits with status 130. Continuous watching otherwise exits
!! successfully when the stop file is detected, regardless of prior fpm failures.
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
