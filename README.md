# fpm-watch

[![Version](https://img.shields.io/github/v/tag/gha3mi/fpm-watch?label=version&sort=semver)](https://github.com/gha3mi/fpm-watch/releases)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/fpm-watch/)
[![CI](https://github.com/gha3mi/fpm-watch/actions/workflows/CI-CD.yml/badge.svg?branch=main)](https://github.com/gha3mi/fpm-watch/actions/workflows/CI-CD.yml)
[![License](https://img.shields.io/github/license/gha3mi/fpm-watch?color=green)](LICENSE)

`fpm-watch` reruns `fpm build|test|run` when relevant files change, using fpm's
dependency model to select files and affected targets. Polling sleeps between scans.

## Installation

Requires fpm 0.13.0, a Fortran compiler, and a C compiler. The library dependency
is pinned to fpm 0.13.0 in [fpm.toml](fpm.toml).

```bash
git clone https://github.com/gha3mi/fpm-watch.git
cd fpm-watch
fpm install --profile release
```

Put both `fpm` and the installed `fpm-watch` executable on `PATH`.

## Usage

```text
fpm-watch [watch options] <build|test|run> [fpm arguments...]
```

```bash
fpm-watch build
fpm-watch test
fpm-watch run first -- input.dat
fpm-watch --watch-deps run --example my_example
fpm-watch run -C path/to/project
```

Run inside an fpm project or select one with `--directory`/`-C`. Watcher options
go **before** the subcommand; everything after it is forwarded to fpm. Use `--`
before application arguments.

### Behavior

The command runs at startup by default, then after debounced changes. Commands run
synchronously; polling pauses during execution and detects intervening edits afterward.

Build mode watches project sources; run/test watches selected targets' dependency
closures. Relevant includes, quoted C headers, manifests, version files, and
response-file candidates are included. `--watch-deps` adds dependency projects.

With 1–64 targets and no explicit selection, source edits normally run only
affected targets. Metadata or graph membership changes, or unavailable mapping,
run the original command. Explicit target names, `--target`, and `--all` are preserved.

Source/include edits refresh the graph; failure retains the previous list and
runs the original command. Invalid manifest/response settings pause execution
until a later edit or rescan succeeds.

New files appear on graph refresh. Enable periodic discovery with `--watch-rescan 2`
(disabled by default). Scans hash complete file contents; shorter intervals cost more CPU.

**Stop:** Ctrl+C stops the watcher and interrupts an active command. Creating
`.fpm-watch.stop` in the project stops between commands; remove it before the
next session. It does not prevent enabled startup execution.

**Supervision:** `--watch-auto-restart` restarts nonzero watcher exits. Continuous
watching survives fpm failures; `--watch-once` propagates them. User interruption
ends supervision.

### Response files

```text
# dev.rsp
options run
options --profile release
```

Run `fpm-watch run @dev` to load `dev.rsp` from the current directory or `CLI_RESPONSE_PATH`.
Use `@./config/dev.rsp` for an exact path. Include `options build|test|run`; it can
override the CLI subcommand. Comments, quoting, and OS sections follow fpm's parser.

Only options-only files are supported: no `system`/`print`/`stop` or compound
executable responses. Once response parsing is active, every raw `@` argument,
including those after `--`, must resolve to such a file. Lookup candidates are watched.

### Known fpm limitations

With fpm 0.13.0, some include-only edits leave cached objects unchanged. For the
default build directory, `fpm clean --skip` forces recompilation while retaining dependencies.
Intel ifx can also crash inside fpm when scanning an empty source directory; this remains unresolved.

## Watch options

Values accept `--watch-option VALUE` or `=VALUE`. Optional boolean/verbosity values
require `=`; bare booleans enable, `=false` disables. All times are seconds.

| Option | Effect |
| --- | --- |
| `--help`, `-h`, `--watch-help` | Show help. |
| `--watch-deps[=BOOL]`, `--watch-no-deps` | Watch dependency projects. |
| `--watch-poll SEC` | Scan interval; minimum 0.05. |
| `--watch-debounce SEC` | Delay before resampling an edit. |
| `--watch-rescan SEC`, `--watch-no-rescan` | Periodic graph refresh; 0 disables. |
| `--watch-run-on-start`, `--watch-no-run-on-start` | Control startup execution. |
| `--watch-once[=BOOL]` | Initialize, optionally execute once, return fpm's status (0 without a run). |
| `--watch-include GLOB`, `--watch-ignore GLOB` | Repeatable path filters. |
| `--watch-fpm PATH` | fpm executable. |
| `--watch-silent-fpm[=BOOL]` | Suppress command stdout/stderr. |
| `--watch-log-format FORMAT` | `text`, `json`, or `logfmt`. |
| `--watch-quiet`, `-q` | Watcher errors only; markers retain command events. |
| `--watch-verbose[=N]`, `-v` | Raise verbosity; default 1, maximum 2. |
| `--watch-very-verbose`, `-vv` | Verbosity 2; list paths after refreshes. |
| `--watch-print-files[=BOOL]` | Up to 200 startup paths; CLI enables verbosity 2. |
| `--watch-debug[=BOOL]` | Debug records unless quiet. |
| `--watch-output-markers[=BOOL]` | Frame command output in structured modes. |
| `--watch-auto-restart[=BOOL]` | Supervise watcher exits. |
| `--watch-restart-delay SEC` | Delay before restarting. |
| `--watch-restart-max N` | Restarts after initial launch; 0 = unlimited. |
| `--watch-self PATH` | Supervisor child executable; auto-detected by default. |

Quote globs; paths use `/` separators. Nonempty include lists require a match;
ignore matches exclude paths. Manifests, version metadata, and response candidates
bypass filters. Use `--watch-print-files` to inspect paths.

## Configuration

Add `[extra.fpm-watch]` to the project's `fpm.toml`. Defaults:

```toml
[extra.fpm-watch]
poll           = 0.5
debounce       = 0.2
rescan         = 0.0
run-on-start   = true
once           = false
deps           = false
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
```

CLI overrides settings; include/ignore flags append unique entries, and verbosity
flags raise the level. `verbosity=-1` means quiet. Manifest-only startup listings
need `print-files=true` and `verbosity=2`. Negative delays/rescan become 0;
poll is at least 0.05. Log format stays fixed for the watcher session.

## Logging and integrations

Text errors use stderr. JSON Lines/logfmt records use stdout; raw fpm/model and
compiler/runtime output uses stderr:

```bash
fpm-watch --watch-log-format=json build > watch.jsonl 2> fpm.log
```

Records contain `timestamp`, `level`, `event`, and `message` (`msg` in logfmt).
Correlate `command.started`/`command.finished` by session-scoped `cycle_id`;
completion includes `exit_code`, `duration_ms`, and `interrupted`.
In structured modes, `--watch-output-markers` brackets stderr with `@@fpm-watch:started:ID@@` and
`@@fpm-watch:finished:ID@@`, retaining both events even when quiet. Match IDs
rather than relying on stdout/stderr delivery order.

For Bash completion, run `source ./completion.bash` from this repository.

## Development

```bash
fpm build --profile debug --verbose
fpm build --profile release --verbose
fpm test
fpm install --prefix .
./bin/fpm-watch --watch-once test --profile debug
python test/integration.py --watcher ./bin/fpm-watch
python test/response_regression.py --watcher ./bin/fpm-watch
python test/platform_regression.py ./bin/fpm-watch
python test/release_regression.py
```

On Windows, replace `./bin/fpm-watch` with `.\bin\fpm-watch.exe`. Python tests need 3.9+;
release tests also need Bash/jq and are skipped on Windows.
Optional checks: `fortitude check`; `ford README.md` generates FORD docs in `doc/`.
CI targets GNU on Linux/macOS/Windows and Intel ifx on Linux. macOS uses the
optimized GNU `ci-release` profile (`-O1`); other release jobs use `release`.

### CI status

Generated by CI for `main`.

<!-- STATUS:setup-fortran-conda:START -->

| OS | Compiler | Version | fpm |
| --- | --- | ---: | :---: |
| ubuntu 24.04 | `gfortran` | 16.2.0 | 0.13.0 ✅ |
| ubuntu 24.04 | `ifx` | 2026.1.1 | 0.13.0 ✅ |
| macos 26 | `gfortran` | 16.2.0 | 0.13.0 ✅ |
| windows 2025 | `gfortran` | 16.2.0 | 0.13.0 ✅ |

<!-- STATUS:setup-fortran-conda:END -->
