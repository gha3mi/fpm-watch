fpm-watch is a plugin for the [Fortran Package Manager (fpm)](https://fpm.fortran-lang.org) that automatically rebuilds, tests or runs your project whenever relevant source files change. It supports dependency awareness, low CPU idle mode and automatic restart capability.

## Requirements

* [Fortran Package Manager (fpm)](https://fpm.fortran-lang.org) version 0.13.0
* A supported Fortran compiler.
* Tested on Ubuntu with gfortran.

## Installation

```bash
git clone https://github.com/gha3mi/fpm-watch.git
cd fpm-watch
fpm install --profile release
```

## Basic Usage

```bash
fpm-watch build
fpm-watch test
fpm-watch run
```

Everything after `<build|test|run>` is passed directly to `fpm`.

Example:

```bash
fpm-watch --watch-low-cpu --watch-auto-restart run --example my_example
```

## Watch Options

```
--watch-help
    Show help and exit.

--watch-quiet | -q
    Quiet mode (errors only).

--watch-verbose[=N] | -v
    Increase verbosity (0–2).

--watch-very-verbose | -vv
    Maximum verbosity (prints watch list).

--watch-debug
    Enable debug logging.
```

### Dependency Watching

```
--watch-deps
    Also watch dependency source trees.

--watch-no-deps
    Disable dependency watching (default).
```

### CPU Mode

```
--watch-low-cpu
    Use OS sleep instead of busy-waiting (near 0% idle CPU).

--watch-no-low-cpu
    Use busy polling (default).
```

### Polling & Execution

```
--watch-poll <sec>
    Poll interval (default: 0.5).

--watch-debounce <sec>
    Debounce delay before running (default: 0.2).

--watch-rescan <sec>
    Periodically rebuild fpm model (default: 0 = disabled).

--watch-no-rescan
    Disable periodic rescan.

--watch-run-on-start
    Run once immediately on startup (default).

--watch-no-run-on-start
    Wait for first change before running.
```

### Output Control

```
--watch-silent-fpm
    Suppress fpm output; show only fpm-watch status.

--watch-print-files
    Print watched file list at startup.
```

### Filtering

```
--watch-ignore <glob>
    Ignore matching files (repeatable).

--watch-include <glob>
    Only watch matching files (repeatable).

--watch-feature <name>
    Enable optional feature plugin (repeatable).
```

### Auto-Restart (Supervisor Mode)

```
--watch-auto-restart
    Restart fpm-watch automatically if it crashes.

--watch-restart-delay <sec>
    Delay before restart (default: 1.0).

--watch-restart-max <N>
    Maximum restart attempts (0 = unlimited).

--watch-self <path>
    Explicit path to fpm-watch executable (advanced).
```

## Configuration via `fpm.toml`

Defaults can be defined in your project:

```toml
[extra.fpm-watch]
poll          = 0.5
debounce      = 0.2
rescan        = 0.0
run-on-start  = true
silent-fpm    = false
print-files   = false
deps          = false
low-cpu       = false
auto-restart  = false
restart-delay = 1.0
restart-max   = 0
self          = ""
debug         = false
verbosity     = 0
ignore        = []
include       = []
features      = []
```

## Stopping

Press:

```
Ctrl+C
```

Or create:

```
touch .fpm-watch.stop
```