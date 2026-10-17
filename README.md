# fpm-watch

[![Version](https://img.shields.io/github/v/tag/gha3mi/fpm-watch?label=version&sort=semver)](https://github.com/gha3mi/fpm-watch/releases)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/fpm-watch/)
[![CI](https://github.com/gha3mi/fpm-watch/actions/workflows/CI-CD.yml/badge.svg?branch=main)](https://github.com/gha3mi/fpm-watch/actions/workflows/CI-CD.yml)
[![License](https://img.shields.io/github/license/gha3mi/fpm-watch?color=green)](LICENSE)

`fpm-watch` reruns `fpm build`, `fpm test`, or `fpm run` when relevant files
change. It uses fpm's dependency model to select files and affected targets.
Polling uses OS sleep between scans; dependency watching and a restart supervisor
are optional.

## Installation

Use fpm 0.13.0, a Fortran compiler, and a C compiler. The fpm library dependency is
pinned to 0.13.0 in [fpm.toml](fpm.toml).

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

Run from an fpm project, or select one with fpm's `--directory`/`-C` option:

```bash
fpm-watch build
fpm-watch test
fpm-watch run first -- input.dat
fpm-watch --watch-deps run --example my_example
fpm-watch run --directory path/to/project
```

Place watcher options **before** the subcommand. Everything after it is forwarded
to fpm; use fpm's `--` delimiter for application arguments. Explicit target names,
`--target first,second`, and `--all` retain fpm's selection.

### Watching and target selection

The command runs at startup by default. After that, the watcher polls known paths,
waits for the debounce interval after detecting an edit, and checks again.
Commands run synchronously: polling pauses while fpm runs, and subsequent scans
detect edits made during that run.

The watch list contains relevant sources, nested Fortran/CPP includes, quoted C
headers, manifests, manifest version files, and response-file lookup candidates.
Build mode watches project sources from fpm's model. Run/test mode watches the
selected targets' dependency closures. `--watch-deps` includes dependency projects.

For unpinned run/test commands with 1–64 targets, source edits normally run only
affected targets. The original command runs when metadata changes, target/path
membership changes, or affected-target mapping is unavailable. Explicit user
selections remain part of that original command.

Source/include edits refresh the graph before execution. If that refresh fails,
the previous watch list is retained and the original command runs so fpm can
report the failure. Manifest/response edits also reparse settings; if that
configuration refresh fails, execution waits until a later edit or rescan succeeds.

New sources are discovered during graph refreshes. Use
`fpm-watch --watch-rescan 2 test` to discover additions without another watched
path changing. Rescans are disabled by default. Small poll intervals and frequent
rescans increase work; each polling scan hashes complete file contents.

### Stopping and supervision

Press **Ctrl+C** to stop the watcher and interrupt an active command. Alternatively,
create `.fpm-watch.stop` in the selected project directory. The stop file is checked
between commands; it does not interrupt an active command. Remove it before the
next session. Startup execution still occurs if run-on-start is enabled.

`--watch-auto-restart` restarts a watcher after a nonzero exit. Continuous watching
continues after fpm failures. In one-shot mode, fpm failure becomes the watcher's
exit status and can trigger a restart. User interruption ends supervision.

### Response files

Use a named options-only fpm response file:

```text
# dev.rsp
options run
options --profile release
```

Run it with `fpm-watch run @dev`. `@dev` searches for `dev.rsp` in the current
directory and `CLI_RESPONSE_PATH`. Use `@./config/dev.rsp` for an exact path.
Options, quoting, and named-file OS sections follow fpm's parser. Include an
`options build`, `options test`, or `options run` record; it may override the
command-line subcommand. Comments and OS section headers are allowed.

Response files and lookup candidates are watched, including missing candidates
whose creation could change lookup. Procedural records such as
`system`/`print`/`stop` and executable-specific compound response files are
unsupported. When response parsing is active, other raw `@` arguments, even
after `--`, must also resolve to options-only response files.

### fpm cache limitation

In fpm 0.13.0, some include-only edits can leave cached objects unchanged even
though the watcher reruns the command. For the default build directory,
`fpm clean --skip` forces recompilation on the next build while retaining
downloaded dependencies.

## Watch options

Value options accept `--watch-option VALUE` or `--watch-option=VALUE`.
Optional boolean and verbosity values require `=`: a bare boolean flag enables
the setting, and `=false` disables it. All times are seconds.

| Option | Effect |
| --- | --- |
| `--help`, `-h`, `--watch-help` | Show help and exit. |
| `--watch-deps[=BOOL]`, `--watch-no-deps` | Enable or disable dependency watching. |
| `--watch-poll SEC` | Poll interval; minimum 0.05. |
| `--watch-debounce SEC` | Delay after detecting an edit before resampling. |
| `--watch-rescan SEC`, `--watch-no-rescan` | Periodic graph refresh; 0 disables it. |
| `--watch-run-on-start`, `--watch-no-run-on-start` | Enable or disable startup execution. |
| `--watch-once[=BOOL]` | Initialize and optionally run once; propagate fpm's status, or exit 0 after successful initialization without a run. |
| `--watch-include GLOB`, `--watch-ignore GLOB` | Repeatable include/ignore filters. |
| `--watch-fpm PATH` | fpm executable path or name. |
| `--watch-silent-fpm[=BOOL]` | Suppress both command output streams. |
| `--watch-log-format FORMAT` | `text`, `json`, or `logfmt`. |
| `--watch-quiet`, `-q` | Watcher errors only, except forced output-marker events. |
| `--watch-verbose[=N]`, `-v` | Raise verbosity to N; default 1, maximum 2. |
| `--watch-very-verbose`, `-vv` | Raise verbosity to 2; list paths after refreshes. |
| `--watch-print-files[=BOOL]` | List up to 200 startup paths; enabling the CLI flag raises verbosity to 2. |
| `--watch-debug[=BOOL]` | Enable debug records unless quiet mode suppresses them. |
| `--watch-output-markers[=BOOL]` | Add command-output markers in structured logging modes. |
| `--watch-auto-restart[=BOOL]` | Enable or disable supervision. |
| `--watch-restart-delay SEC` | Delay before a supervisor restart. |
| `--watch-restart-max N` | Maximum restarts after the initial launch; 0 means unlimited. |
| `--watch-self PATH` | Watcher executable used by the supervisor; normally auto-detected. |

Quote glob arguments against shell expansion. Patterns match watched paths
normalized to `/` separators; use `--watch-print-files` to inspect them. An empty
include list admits all eligible paths; otherwise, at least one include pattern
must match. Ignore matches exclude paths. Manifests, version metadata, and
response-file candidates bypass these filters.

## Configuration

Add `[extra.fpm-watch]` to the selected project's `fpm.toml`. The following values
are the built-in defaults:

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

Leading CLI flags override manifest settings. Include/ignore flags append unique
entries, and verbosity flags raise rather than replace the current level.
`verbosity=-1` selects quiet mode. For a startup path list configured only in the
manifest, set both `print-files=true` and `verbosity=2`.

Negative debounce, rescan, and restart delays become 0. The poll interval is
clamped to at least 0.05. The log format stays fixed across configuration refreshes
and supervisor restarts; restart the whole watcher session to change it.

## Logging and integrations

Text logs have timestamps and no ANSI colors; errors use stderr. Quiet mode
controls watcher records, while `--watch-silent-fpm` controls command output.

In JSON Lines or logfmt mode, watcher records, including errors, use stdout.
Raw fpm/model output and compiler/runtime diagnostics use stderr. Keep the streams
separate:

```bash
fpm-watch --watch-log-format=json build > watch.jsonl 2> fpm.log
fpm-watch --watch-log-format=logfmt test > watch.log 2> fpm.log
```

Records contain `timestamp` (RFC 3339), `level`, `event`, and `message` (JSON) or
`msg` (logfmt), and are flushed immediately. JSON attributes retain their types;
logfmt arrays are quoted JSON.

Correlate `command.started` and `command.finished` with `cycle_id`. Completion
includes `exit_code`, `duration_ms`, and `interrupted`. Treat IDs as opaque and
scope them to one watcher session. At verbosity 1, `watch.configuration` reports
effective settings at startup and after successful refreshes.

To frame each command's raw output, enable markers:

```bash
fpm-watch --watch-log-format=json --watch-output-markers build
```

Stderr receives `@@fpm-watch:started:ID@@` and `@@fpm-watch:finished:ID@@`
around command output. The ID matches `cycle_id`. With markers enabled, both
command events remain visible even in quiet mode. Stdout and stderr delivery order
can differ; integrations should correlate by ID.

## Bash completion

From this repository, install and load the completion script:

```bash
mkdir -p ~/.local/share/bash-completion/completions
cp completion.bash ~/.local/share/bash-completion/completions/fpm-watch
source ~/.local/share/bash-completion/completions/fpm-watch
```

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

On Windows, use `.\bin\fpm-watch.exe` for direct execution and
`--watcher ./bin/fpm-watch.exe` for the Python suites that accept `--watcher`.
The platform suite takes the executable as a positional argument. The checks need
Python 3.9+ and use its standard library and temporary projects. Release tests require
Bash and jq and are skipped on Windows.

Optional local checks:

```bash
fortitude check
ford README.md
```

Documentation uses FORD only and is written to `doc/`. CI builds with gfortran
on Linux/macOS/Windows and ifx on Linux. The macOS job uses the GNU `ci-release`
profile (`-O1`); other release jobs use `release`, and tests use `debug`.

### Release helper

The package reads its version from `VERSION`. `release.sh` uses the previous
release tag as its baseline, or `VERSION` when there are no tags. Patch changes
promote a prerelease to its stable core (`v1.2.3-rc.1` → `v1.2.3`); feature
and breaking changes advance the minor and major versions. Invalid baseline
versions fail before files are modified.

### CI status

This block is maintained by CI.

<!-- STATUS:setup-fortran-conda:START -->

| OS | Compiler | Version | fpm |
| --- | --- | ---: | :---: |
| ubuntu 24.04 | `gfortran` | 16.2.0 | 0.13.0 ✅ |
| ubuntu 24.04 | `ifx` | 2026.1.1 | 0.13.0 ✅ |
| macos 26 | `gfortran` | 16.2.0 | 0.13.0 ✅ |
| windows 2025 | `gfortran` | 16.2.0 | 0.13.0 ✅ |

<!-- STATUS:setup-fortran-conda:END -->
