[![GitHub](https://img.shields.io/badge/GitHub-fpm_watch-blue.svg?style=social&logo=github)](https://github.com/gha3mi/fpm-watch)
[![Version](https://img.shields.io/github/v/tag/gha3mi/fpm-watch?label=version&sort=semver)](https://github.com/gha3mi/fpm-watch/releases)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/fpm-watch/)
[![Setup Fortran Conda CI/CD](https://github.com/gha3mi/fpm-watch/actions/workflows/CI-CD.yml/badge.svg?branch=main)](https://github.com/gha3mi/fpm-watch/actions/workflows/CI-CD.yml)
[![License](https://img.shields.io/github/license/gha3mi/fpm-watch?color=green)](https://github.com/gha3mi/fpm-watch/blob/main/LICENSE)

`fpm-watch` is an [fpm](https://fpm.fortran-lang.org) plugin that reruns
`fpm build`, `test`, or `run` when relevant files change. It uses fpm's dependency
graph, with optional dependency watching, OS sleep, and a restart supervisor.

## CI Status

<!-- STATUS:setup-fortran-conda:START -->

| OS | Compiler | Version | fpm |
| --- | --- | ---: | :---: |
| ubuntu 24.04 | `gfortran` | 16.1.0 | 0.13.0 ✅ |
| ubuntu 24.04 | `ifx` | 2026.1.1 | 0.13.0 ✅ |
| macos 26 | `gfortran` | 16.1.0 | 0.13.0 ✅ |
| windows 2025 | `gfortran` | 16.1.0 | 0.13.0 ✅ |

<!-- STATUS:setup-fortran-conda:END -->

## Installation

Use fpm 0.13.0 (the version used in CI) and a Fortran compiler:

```bash
git clone https://github.com/gha3mi/fpm-watch.git
cd fpm-watch
fpm install --profile release
```

Ensure `fpm` and the installed `fpm-watch` executable are on `PATH`.

## Usage

Run from your project's directory:

```text
fpm-watch [watch options] <build|test|run> [fpm arguments...]
```

```bash
fpm-watch build
fpm-watch test
fpm-watch run
fpm-watch --watch-low-cpu --watch-deps run --example my_example
```

Place watcher options **before** the subcommand; subsequent arguments go to fpm.
Target lists such as `run --target first,second` follow fpm's selection.
On source changes, `run`/`test` select affected targets unless explicitly chosen.

By default, the command runs at startup and after changes. Changes are checked
between command runs. The watch list includes the project manifest, relevant
sources, resolved Fortran includes, and quoted C headers.

Manifest edits refresh settings and the watch list, then run the command. Failed
refreshes retain the previous settings and list until a later edit or rescan
succeeds. Use `--watch-rescan 2` to discover added sources periodically.

Stop with **Ctrl+C**, which also interrupts an active command, or create
`.fpm-watch.stop` in the project directory (checked between commands). Remove
that file before restarting. User interrupts do not trigger supervisor restarts.

## Watch options

Value options accept `--watch-option VALUE` or `--watch-option=VALUE`.
Flags marked `[=false]` enable a setting; append `=false` to disable it. Times are in seconds;
defaults appear in the configuration block.

| Option | Effect |
| --- | --- |
| `--help`, `-h`, `--watch-help` | Show help and exit. |
| `--watch-deps[=false]`, `--watch-no-deps` | Enable/disable dependency sources and manifests, including feature/profile dependencies. |
| `--watch-low-cpu[=false]`, `--watch-no-low-cpu` | Use OS sleep/busy waiting. |
| `--watch-poll SEC` | Poll interval, clamped to at least 0.05. |
| `--watch-debounce SEC` | Wait after a change before checking again and running. |
| `--watch-rescan SEC`, `--watch-no-rescan` | Periodically refresh the watch list and run if it changes; 0 disables rescans. |
| `--watch-run-on-start`, `--watch-no-run-on-start` | Run immediately/wait for a change. |
| `--watch-once[=false]` | Initialize, run if run-on-start is enabled, and exit with fpm's status (0 without a run). |
| `--watch-ignore GLOB`, `--watch-include GLOB` | Exclude/restrict paths; repeatable. Quote globs against shell expansion. |
| `--watch-feature NAME` | Enable a watcher plugin; repeatable (currently `stats`, an internal counter). |
| `--watch-fpm PATH` | Select the fpm executable by path or name. |
| `--watch-silent-fpm[=false]` | Suppress both fpm output streams. |
| `--watch-log-format FORMAT` | Select `text`, `json`, or `logfmt`. |
| `--watch-quiet`, `-q` | Watcher errors only, except enabled output protocol events. |
| `--watch-verbose[=N]`, `-v` | Raise verbosity to N (default 1, maximum 2); adds configuration and timings. |
| `--watch-very-verbose`, `-vv` | Verbosity 2; lists files after refreshes. |
| `--watch-print-files[=false]` | List up to 200 startup paths and raise verbosity to 2. |
| `--watch-debug[=false]` | Enable additional debug records. |
| `--watch-output-markers[=false]` | Frame command stderr in structured mode; see below. |
| `--watch-auto-restart[=false]` | Restart on nonzero watcher exit; command failures during continuous watching keep the watcher running. |
| `--watch-restart-delay SEC` | Wait before restarting. |
| `--watch-restart-max N` | Restart limit after initial launch; 0 means unlimited. |
| `--watch-self PATH` | Explicit watcher executable path for the supervisor. |

Ignore rules override include rules; watched manifests bypass both.
Watcher plugins differ from fpm build features (`--features` after the subcommand).

## Configuration

CLI flags override `fpm.toml` settings, except include/ignore/feature flags append
to lists and `--watch-verbose` only raises verbosity. Built-in defaults:

```toml
[extra.fpm-watch]
poll           = 0.5
debounce       = 0.2
rescan         = 0.0
run-on-start   = true
once           = false
deps           = false
low-cpu        = false
silent-fpm     = false
print-files    = false
debug          = false
verbosity      = 0
log-format     = "text"
fpm-executable = "fpm"
output-markers = false
auto-restart   = false
restart-delay  = 1.0
restart-max    = 0
self           = ""
ignore         = []
include        = []
features       = []
```

## Logging and integrations

Text logs are timestamped without ANSI colors; watcher errors go to stderr.
Quiet mode controls watcher logs; `--watch-silent-fpm` controls fpm output.

For JSON Lines or logfmt, **all watcher records, including errors, go to stdout**;
raw fpm/model output and compiler/runtime diagnostics go to stderr. Keep the
streams separate when parsing records:

```bash
fpm-watch --watch-log-format json build > watch.jsonl 2> fpm.log
fpm-watch --watch-log-format logfmt test > watch.log 2> fpm.log
```

Each record contains `timestamp` (RFC 3339 with milliseconds and UTC offset),
`level`, `event`, and `message` (JSON) or `msg` (logfmt), and is flushed immediately.
JSON attributes retain their types; logfmt arrays use quoted JSON.

For automation, use events and attributes: `command.started`/`command.finished`
share `cycle_id`; completion includes `exit_code`, `duration_ms`, and `interrupted`.
`watch.changed` includes `paths`. At verbosity 1, `watch.configuration` reports
effective settings at startup and after successful refreshes.

The log format stays fixed across manifest refreshes and supervisor restarts;
changing it requires restarting the watcher.

For per-command output attribution, enable `--watch-output-markers` with a
structured format:

```bash
fpm-watch --watch-log-format=json --watch-output-markers --watch-silent-fpm=false --watch-fpm /path/to/fpm build
```

Stderr brackets raw output with `@@fpm-watch:started:ID@@` and
`@@fpm-watch:finished:ID@@`. `ID` matches the opaque `cycle_id` (e.g. `0:1`), unique
across supervisor restarts. Streams can arrive in either order. With markers,
both command events remain visible in quiet mode. Consume markers as protocol
records.

## Development

```bash
fpm build --profile debug --verbose
fpm build --profile release --verbose
fpm test
fpm install --prefix .
./bin/fpm-watch --watch-low-cpu --watch-once test
python test/integration.py --watcher ./bin/fpm-watch
python test/platform_regression.py ./bin/fpm-watch
fortitude check
ford README.md
```

Use `./bin/fpm-watch.exe` on Windows. Python checks use only the standard library
and temporary projects. Fortitude and FORD are optional local tools; FORD writes
docs to `doc/`. CI uses `ci-release` (GNU `-O1`) on macOS, `release` elsewhere,
and the debug profile for tests.

## Bash completion

```bash
mkdir -p ~/.local/share/bash-completion/completions
cp completion.bash ~/.local/share/bash-completion/completions/fpm-watch
source ~/.local/share/bash-completion/completions/fpm-watch
```
