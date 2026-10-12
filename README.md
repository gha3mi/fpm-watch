[![GitHub](https://img.shields.io/badge/GitHub-fpm_watch-blue.svg?style=social&logo=github)](https://github.com/gha3mi/fpm-watch)
[![Version](https://img.shields.io/github/v/tag/gha3mi/fpm-watch?label=version&sort=semver)](https://github.com/gha3mi/fpm-watch/releases)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/fpm-watch/)
[![Setup Fortran Conda CI/CD](https://github.com/gha3mi/fpm-watch/actions/workflows/CI-CD.yml/badge.svg?branch=main)](https://github.com/gha3mi/fpm-watch/actions/workflows/CI-CD.yml)
[![License](https://img.shields.io/github/license/gha3mi/fpm-watch?color=green)](https://github.com/gha3mi/fpm-watch/blob/main/LICENSE)

fpm-watch is a plugin for the [Fortran Package Manager (fpm)](https://fpm.fortran-lang.org) that automatically rebuilds, tests or runs your project whenever relevant source files change. It supports dependency awareness, low CPU idle mode and automatic restart capability.

## CI Status

<!-- STATUS:setup-fortran-conda:START -->

| OS | Compiler | Version | fpm |
| --- | --- | ---: | :---: |
| ubuntu 24.04 | `gfortran` | 16.1.0 | 0.13.0 ✅ |
| ubuntu 24.04 | `ifx` | 2026.1.1 | 0.13.0 ✅ |
| macos 26 | `gfortran` | 16.1.0 | 0.13.0 ✅ |
| windows 2025 | `gfortran` | 16.1.0 | 0.13.0 ✅ |

<!-- STATUS:setup-fortran-conda:END -->

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
Target lists such as `run --target first,second` use the same selection as `fpm`.

Example:

```bash
fpm-watch --watch-low-cpu --watch-auto-restart run --example my_example
```

## Watch Options

```
--help | -h
    Show general help and exit.

--watch-help
    Show help and exit.

--watch-quiet | -q
    Quiet mode (errors only).

--watch-log-format <text|json|logfmt>
    Log format (default: text). Also accepts --watch-log-format=json.

--watch-verbose[=N] | -v
    Increase verbosity (0–2).

--watch-very-verbose | -vv
    Maximum verbosity (prints watch list).

--watch-debug
    Enable debug logging. Use --watch-debug=false to disable a manifest setting.
```

### Dependency Watching

```
--watch-deps
    Also watch dependency source trees and manifests, including feature/profile dependencies.

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
    Periodically rebuild the fpm model and run on added/removed sources (default: 0 = disabled).

--watch-no-rescan
    Disable periodic rescan.

--watch-run-on-start
    Run once immediately on startup (default).

--watch-no-run-on-start
    Wait for first change before running.

--watch-once
    Initialize, run once if run-on-start is enabled, and exit with fpm's status.
```

### Output Control

```
--watch-silent-fpm
    Suppress fpm output; show only fpm-watch status.
    Use --watch-silent-fpm=false to override silent-fpm = true in fpm.toml.

--watch-fpm <path>
    Select the fpm executable (default: fpm on PATH).
    Also accepts --watch-fpm=<path>.

--watch-output-markers
    In structured logging mode, frame raw stderr with per-command cycle markers
    for GUI integrations (default: disabled).
    Keep matching command.started and command.finished events visible in quiet mode.

--watch-print-files
    Print watched file list at startup. --watch-print-files=false disables it.
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
    Restart fpm-watch automatically if it crashes. --watch-auto-restart=false
    disables a manifest setting.

--watch-restart-delay <sec>
    Delay before restart (default: 1.0).

--watch-restart-max <N>
    Maximum restart attempts after the initial launch (0 = unlimited).

--watch-self <path>
    Explicit path to fpm-watch executable (advanced).
```

## Logging

Help and watcher logs are plain text without ANSI color codes; no color library
is required. The default `text` format uses short, timestamped status lines.
Configuration and timing details require `--watch-verbose`;
individual watched files require `--watch-very-verbose` or `--watch-print-files`.
`--watch-debug` emits additional records at the `debug` level. Quiet mode
suppresses all watcher records except errors. Raw fpm output is controlled
separately by `--watch-silent-fpm`.

```text
14:32:10 INFO  Watching 24 files (build); Ctrl+C or touch .fpm-watch.stop to stop
14:32:15 INFO  Changed src/math.f90
14:32:15 INFO  Running fpm build
14:32:16 INFO  Build succeeded in 0.84s
```

Use `json` for [JSON Lines](https://jsonlines.org/), with one UTF-8 JSON object
per line, or `logfmt` for [key/value records](https://brandur.org/logfmt):

```bash
fpm-watch --watch-log-format json build > watch.jsonl 2> fpm.log
fpm-watch --watch-log-format logfmt test > watch.log 2> fpm.log
```

In both structured formats, **all watcher records, including errors, use
stdout**. Raw fpm/model output and compiler/runtime diagnostics use stderr,
so stdout can be parsed directly. Avoid merging the streams with `2>&1` when
collecting structured records. Text mode keeps the usual fpm streams and sends
watcher errors to stderr.

```json
{"timestamp":"2026-10-12T14:32:16.842+02:00","level":"info","event":"command.finished","message":"Build succeeded in 0.84s","command":"fpm build","exit_code":0,"duration_ms":842,"interrupted":false,"cycle_id":"0:1"}
```

```text
timestamp=2026-10-12T14:32:16.842+02:00 level=info event=command.finished msg="Build succeeded in 0.84s" command="fpm build" exit_code=0 duration_ms=842 interrupted=false cycle_id="0:1"
```

Every structured record has `timestamp` (RFC 3339 with milliseconds and an
explicit UTC offset), `level`, `event`, and a human message (`message` in JSON,
`msg` in logfmt). Event attributes use JSON numbers, booleans, strings, and
arrays; logfmt uses scalar key/value fields, with arrays stored as quoted JSON.
Quotes, backslashes, and control characters are escaped to keep each record on
one physical line. Invalid UTF-8 bytes become Unicode replacement characters.
Records are flushed immediately.

Use `event` and its attributes for automation rather than parsing the message:

| Event | Useful attributes |
|---|---|
| `watch.ready` | `file_count`, `root_count`, `mode`, `command`, `once` |
| `watch.configuration` (verbosity 1) | Effective `compiler`, `profile`, `features`, `build_dir`, `deps`, `low_cpu`, `poll_ms`, `debounce_ms`, `rescan_s`, `ignore`, `include` |
| `watch.refreshed` (verbosity 1) | Updated `file_count`, watch-list refresh `duration_ms` |
| `watch.changed` | `file_count`, `paths`, `debounce_ms` |
| `watch.manifest_changed` | Manifest refresh is starting |
| `watch.refresh_failed`, `watch.init_failed` | Error details in the message |
| `command.started` | `command`, `silent_fpm`, `cycle_id` |
| `command.finished` | `command`, `exit_code`, `duration_ms`, `interrupted`, `cycle_id` |
| `watch.stopped` | `reason` (`stop_file` or `interrupt`) |
| `supervisor.restart`, `supervisor.limit_reached` | `exit_code`, `restart_count`; restart events also report `delay_ms` |

The selected format stays fixed for the process lifetime and supervisor
restarts. Changing the manifest's `log-format` requires restarting the watcher. CLI options override
the manifest setting.
At verbosity 1, `watch.configuration` is emitted at startup and after each
successful watch-list refresh, including the effective include/ignore arrays.

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
log-format    = "text"
fpm-executable = "fpm"
output-markers = false
ignore        = []
include       = []
features      = []
once          = false
```

GUI integrations can select the same fpm executable as their ordinary builds
and retain progress output even when the manifest defaults to silent mode:

```bash
fpm-watch --watch-log-format=json --watch-verbose=0 --watch-silent-fpm=false --watch-fpm /path/to/fpm build
```

For reliable per-cycle output attribution, add `--watch-output-markers`.
Stderr then contains `@@fpm-watch:started:N@@` before each command's raw
output and `@@fpm-watch:finished:N@@` after it. Match `N` to the JSON events'
`cycle_id`, an opaque string combining the supervisor restart count and the
command counter (for example, `0:1`, then `1:1` after a restart). IDs stay unique
for the lifetime of a watcher or its supervisor. Stdout and stderr may arrive
in either order. Both command events remain visible in quiet mode when markers
are enabled. Consume the markers as protocol records rather than displaying
them. Without this option, stderr retains its ordinary output format.

## Stopping

Press:

```
Ctrl+C
```

This stops the watcher and an active command, including in supervisor mode.
User interrupts do not trigger automatic restarts.

Or create:

```
touch .fpm-watch.stop
```

Changes to a watched manifest rebuild the watch list and run the command.
An invalid manifest during watching reports an error and keeps the previous
watch list until it can be refreshed successfully. Enable `--watch-rescan`
to discover new source files without restarting the watcher. Fortran includes
and quoted C headers are resolved through source and fpm include directories.

## Development Checks

macOS CI uses `fpm build --profile ci-release`, which selects GNU `-O1`
to reduce compilation time for the fpm dependency. Other CI jobs use the
standard release profile. CI installation reuses that build, and tests use
the debug profile.

```bash
fpm build --profile debug --verbose
fpm build --profile release --verbose
fpm test
fpm install --prefix .
./bin/fpm-watch --watch-low-cpu --watch-once test
python test/integration.py --watcher ./bin/fpm-watch
python test/platform_regression.py ./bin/fpm-watch
```

On Windows, use `./bin/fpm-watch.exe`. The Python checks use only the standard
library and temporary projects; `fpm test` covers command-line helpers,
dependency graphs, command exit statuses, watch-list snapshots, and log encoding.
The integration checks parse both structured formats and verify output-stream
separation, configuration overrides, failures, and supervisor events.

## Bash Completion

Enable bash tab-completion for `fpm-watch` by installing the provided script:

```bash
mkdir -p ~/.local/share/bash-completion/completions
cp completion.bash ~/.local/share/bash-completion/completions/fpm-watch
source ~/.local/share/bash-completion/completions/fpm-watch
```

Then use:

```bash
fpm-watch --<TAB>
```
