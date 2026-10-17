!> Entry point for the watcher and its optional restart supervisor.
!!
!! Parse settings and select logging before entering supervisor mode. The watcher
!! initializes the watch list, optionally runs fpm, and enters the polling loop.
!! In one-shot mode the loop returns immediately; the program propagates the
!! command status, or exits successfully if no command ran. A latched command
!! interruption exits with status 130. Continuous watching otherwise exits
!! successfully when the stop file is detected, regardless of prior fpm failures.
!! Propagated exit statuses suppress runtime STOP output, including after a
!! terminal disconnects.
program fpm_watch
   use watch_restart, only: maybe_supervise
   use watch_cli,     only: parse_cli_config
   use watch_engine,  only: watcher_t
   use watch_types,   only: watch_config_t
   use watch_platform, only: termination_requested
   use watch_cmdline, only: get_arg
   use watch_graph_worker, only: run_graph_worker
   implicit none

   type(watch_config_t) :: cfg
   type(watcher_t)      :: w

   if (get_arg(1) == "--watch-graph-worker") then
      if (command_argument_count() < 4) stop 2
      call parse_cli_config(cfg, start_arg=4, log_format=get_arg(3))
      call run_graph_worker(cfg, get_arg(2))
   else
      call parse_cli_config(cfg)
      call maybe_supervise(cfg)

      call w%init(cfg)
      call w%run()
      if (termination_requested()) stop 130, quiet=.true.
      if (cfg%w%once .and. w%last_exit /= 0) stop w%last_exit, quiet=.true.
   end if
end program fpm_watch
