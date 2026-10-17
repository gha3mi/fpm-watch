# fpm-watch

[![Version](https://img.shields.io/github/v/tag/gha3mi/fpm-watch?label=version&sort=semver)](https://github.com/gha3mi/fpm-watch/releases)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/fpm-watch/)
[![CI](https://github.com/gha3mi/fpm-watch/actions/workflows/CI-CD.yml/badge.svg?branch=main)](https://github.com/gha3mi/fpm-watch/actions/workflows/CI-CD.yml)
[![License](https://img.shields.io/github/license/gha3mi/fpm-watch?color=green)](LICENSE)

`fpm-watch` reruns `fpm build|test|run` after relevant file changes, using
fpm's dependency model and polling that sleeps between scans.

## Install

Requires fpm 0.13.0 plus Fortran and C compilers.

```bash
git clone https://github.com/gha3mi/fpm-watch.git
cd fpm-watch
fpm install --profile release
```

Put `fpm` and `fpm-watch` on `PATH`.

## Use

```text
fpm-watch [watch options] <build|test|run> [fpm arguments...]
```

```bash
fpm-watch build
fpm-watch run first -- input.dat
fpm-watch --watch-deps test
fpm-watch --watch-once test
fpm-watch --watch-help
```

Watcher options go **before** the subcommand; remaining arguments pass unchanged
to fpm. Use `-C path/to/project` after the subcommand to select another project.
`--watch-help` lists options and defaults.

- Runs at startup by default, then after debounced edits. Commands are synchronous.
- Watches sources, includes, quoted C headers, manifests, version files, and response candidates.
- Run/test follows selected targets' dependencies. With 1–64 targets and no explicit
  selection, source edits normally run affected targets; metadata/graph changes or
  unavailable mapping use the original command. Explicit target selections are preserved.
- New files appear on graph refresh; `--watch-rescan 2` enables periodic discovery
  (default: off). Scans hash whole files; shorter intervals cost more CPU.
- Ctrl+C stops the watcher and interrupts commands. `.fpm-watch.stop` stops between
  commands, after enabled startup execution; remove it before restarting.
- Continuous watching survives fpm failures; `--watch-once` propagates their status.
  `--watch-auto-restart` restarts nonzero watcher exits; interruption ends supervision.

## Options

All public watcher options are listed below. Required values accept a space or `=`;
optional values in brackets require `=`. For `[=BOOL]`, bare enables and `=false` disables.
Times are seconds. Defaults show keys under `[extra.fpm-watch]`.

| Option | Config default | Effect |
| --- | --- | --- |
| `--watch-help`, `--help`, `-h` | — | Show help. |
| `--watch-deps[=BOOL]`, `--watch-no-deps` | `deps=false` | Enable/disable dependency watching. |
| `--watch-poll SEC` | `poll=0.5` | Scan interval; minimum 0.05. |
| `--watch-debounce SEC` | `debounce=0.2` | Delay before resampling edits. |
| `--watch-rescan SEC`, `--watch-no-rescan` | `rescan=0.0` | Periodic graph refresh; 0 disables. |
| `--watch-run-on-start`, `--watch-no-run-on-start` | `run-on-start=true` | Enable/disable startup execution. |
| `--watch-once[=BOOL]` | `once=false` | Initialize and optionally run once; propagate failures. |
| `--watch-include GLOB` | `include=[]` | Repeatable inclusion filter; empty means all. |
| `--watch-ignore GLOB` | `ignore=[]` | Repeatable exclusion filter. |
| `--watch-fpm PATH` | `fpm-executable="fpm"` | fpm executable path or name. |
| `--watch-silent-fpm[=BOOL]` | `silent-fpm=false` | Suppress command stdout/stderr. |
| `--watch-log-format FORMAT` | `log-format="text"` | `text`, `json`, or `logfmt`. |
| `--watch-quiet`, `-q` | `verbosity=0` | Set verbosity to -1: errors only, except marker events. |
| `--watch-verbose[=N]`, `-v` | `verbosity=0` | Raise verbosity to N (bare: 1), maximum 2. |
| `--watch-very-verbose`, `-vv` | `verbosity=0` | Raise verbosity to 2; list refreshed paths. |
| `--watch-print-files[=BOOL]` | `print-files=false` | Up to 200 startup paths; CLI raises verbosity to 2. |
| `--watch-debug[=BOOL]` | `debug=false` | Debug records unless quiet. |
| `--watch-output-markers[=BOOL]` | `output-markers=false` | Frame command stderr in structured modes, including quiet mode. |
| `--watch-auto-restart[=BOOL]` | `auto-restart=false` | Supervise nonzero watcher exits. |
| `--watch-restart-delay SEC` | `restart-delay=1.0` | Delay before restarting. |
| `--watch-restart-max N` | `restart-max=0` | Restarts after initial launch; 0 means unlimited. |
| `--watch-self PATH` | `self=""` | Supervisor child executable; auto-detected by default. |

For forwarded fpm options, use `fpm build --help`, `fpm test --help`, or `fpm run --help`.

## Configure

Common defaults in `fpm.toml` (times in seconds):

```toml
[extra.fpm-watch]
poll = 0.5
debounce = 0.2
rescan = 0.0
deps = false
ignore = []
```

CLI overrides scalars, appends unique filters, and raises verbosity. Quote globs;
paths use `/`. Metadata bypasses filters. Invalid manifest/response edits pause execution.

## Responses and logs

`fpm-watch run @dev` loads `dev.rsp` from the current directory or `CLI_RESPONSE_PATH`;
use `@./path/dev.rsp` for an exact path. Include `options run` (or `build`/`test`),
then option records. Only options-only responses are supported. When parsing responses,
every raw `@` argument, including after `--`, must resolve to one.

`--watch-log-format=json` or `=logfmt` sends watcher records to stdout and raw
fpm/model output to stderr. Correlate command events by session-scoped `cycle_id`.
For Bash completion, run `source ./completion.bash` from this repository.

## Known limits

- fpm 0.13.0 may reuse cached objects after include-only edits. With the default
  build directory, `fpm clean --skip` forces recompilation while retaining dependencies.
- Intel ifx can crash inside fpm while scanning empty source directories; unresolved.

## Development

```bash
fpm build --profile debug --verbose
fpm build --profile release --verbose
fpm test
```

See [CI](.github/workflows/CI-CD.yml) for platform and Python regression checks.
Optional: `fortitude check`; `ford README.md` generates FORD docs in `doc/`.

## CI status

Generated by CI for `main`.

<!-- STATUS:setup-fortran-conda:START -->

| OS | Compiler | Version | fpm |
| --- | --- | ---: | :---: |
| ubuntu 24.04 | `gfortran` | 16.2.0 | 0.13.0 ✅ |
| ubuntu 24.04 | `ifx` | 2026.1.1 | 0.13.0 ✅ |
| macos 26 | `gfortran` | 16.2.0 | 0.13.0 ✅ |
| windows 2025 | `gfortran` | 16.2.0 | 0.13.0 ✅ |

<!-- STATUS:setup-fortran-conda:END -->
