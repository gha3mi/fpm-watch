# AGENTS.md — fpm-watch (agent instructions)

This is the single source of truth for AI coding agents working in this repository.
If a tool supports only another filename, configure that tool to read this file (or use a local symlink),
but do not duplicate/maintain multiple instruction files.

## Project overview
`fpm-watch` is an fpm plugin that watches relevant files and re-runs `fpm build|test|run` when changes are detected.
It supports dependency-aware watching, low-CPU polling, and an auto-restart supervisor mode.

## Non‑negotiables (must follow)
- **Documentation:** use **FORD style only**.
  - Use FORD-friendly doc comments (e.g., `!>` / `!!`) and keep docs compatible with FORD.
  - Do not introduce other documentation systems (Doxygen, Sphinx, etc.).
- **No code duplication:** prefer refactoring/shared utilities over copy‑paste.
- **Keep it simple:** code should be user-friendly, readable, and maintainable.
  - Prefer small, focused procedures/modules and clear naming.
  - Keep error messages actionable and consistent.
- **Prefer pure logic where possible:**
  - Use `pure` and `elemental` procedures when feasible (side‑effect‑free utilities, parsing helpers, transformations).
  - Keep I/O, process execution, and OS interactions in small impure wrappers around pure logic.
- **Don’t reimplement fpm:**
  - Use `fpm` as the dependency (already in `fpm.toml`) for model/manifest understanding.
  - Avoid writing custom TOML parsers or rebuilding fpm’s logic in this repo.
  - For build/test/run execution, invoke `fpm` rather than recreating its behavior.
- **Portability:** keep behavior cross‑platform (Linux/macOS/Windows) and compiler-friendly (gfortran + ifx in CI).
- **Dependencies:** do not add new dependencies unless explicitly required and justified.

## Setup / “golden” commands (do not guess)
These match current repo + CI behavior.

### Build
- Debug:
  - `fpm build --profile debug --verbose`
- Release:
  - `fpm build --profile release --verbose`

### Test
- `fpm test`

### Install locally
- Typical local install:
  - `fpm install --profile release`
- CI-style install to local prefix (creates `./bin/fpm-watch`):
  - `fpm install --prefix .`

### Run (examples)
- `fpm-watch build`
- `fpm-watch test`
- `fpm-watch run`
- Everything after `<build|test|run>` is passed to `fpm`, e.g.:
  - `fpm-watch --watch-low-cpu --watch-auto-restart run --example my_example`

### CI parity smoke test (important)
CI uses a one-shot watcher run:
- macOS/Linux:
  - `./bin/fpm-watch --watch-low-cpu --watch-once test`
- Windows:
  - `.\bin\fpm-watch.exe --watch-low-cpu --watch-once test`

### Lint (if available locally)
CI uses Fortitude. If installed:
- `fortitude check`

### Documentation (FORD only)
CI generates docs with FORD using README.md as the project file/config:
- `ford README.md`
Output goes to `doc/`.

## Repo map (where things live)
- `app/fpm-watch.f90`
  - Executable entry point.
- `src/`
  - Core implementation modules:
    - `watch_cli.f90` — CLI flags + dispatch (`build|test|run`) and option parsing.
    - `watch_config.f90` — defaults + reading `[extra.fpm-watch]` from `fpm.toml`.
    - `watch_engine.f90` — polling loop, debounce, rescan, main orchestration.
    - `watch_exec.f90` — constructs and runs the effective `fpm` command.
    - `watch_fpm_graph.f90` — fpm model/dependency graph and watch list building.
    - `watch_filter.f90` / `watch_fingerprint.f90` — include/ignore logic + change detection.
    - `watch_restart.f90` — supervisor/auto-restart mode.
    - `watch_log.f90` — logging utilities.
    - `watch_platform.f90` + `watch_sleep.c` — OS/platform helpers (keep portable).
    - `watch_types.f90`, `watch_util.f90`, `watch_cmdline.f90`, `watch_cmdsplice.f90`, etc. — shared types/utilities.
- `test/check.f90`
  - Minimal `fpm test` target.
- `fpm.toml`
  - Dependencies (including `fpm`), and configuration tables:
    - `[extra.fpm-watch]` default options for users
    - `[extra.ford]` FORD project configuration
    - `[extra.fortitude.check]` linter configuration
- `.github/workflows/CI-CD.yml`
  - CI build/test/docs/lint.

## Behavioral invariants (do not break)
- CLI contract:
  - `fpm-watch [--watch-* options] <build|test|run> [args forwarded to fpm...]`
  - Anything after `<build|test|run>` is forwarded to `fpm`.
- Stop behavior:
  - Ctrl+C should stop the watcher.
  - Creating `.fpm-watch.stop` must stop the loop (documented behavior).
- `--watch-once` must remain CI-friendly (run once, then exit deterministically).
- Keep default behavior stable unless you also update docs/tests.

## When you add/change an option
If you add or change any user-facing flag or config key:
1) Update parsing/behavior in `src/watch_cli.f90` (and relevant modules),
2) Update `[extra.fpm-watch]` handling in `src/watch_config.f90`,
3) Update README (flags + config snippet + examples),
4) Update/extend tests and/or CI smoke test if behavior changes.

## Style guidelines (Fortran)
- Use `implicit none`, `intent(...)`, and clear naming everywhere.
- Prefer `pure` / `elemental` for small utilities and transformations where practical.
- Keep side effects localized:
  - Separate “compute” (pure) from “do” (I/O, executing commands, sleeping).
- Avoid duplication:
  - Consolidate helpers in `watch_util.f90` (or a small new module) instead of copying logic.
- Keep output user-friendly:
  - Minimal but clear status lines; errors should explain what to do next.

## What not to touch (unless explicitly asked)
- CI publishing behavior, secrets/tokens, or workflow permissions.
- Dependency versions in `fpm.toml` (especially `fpm` tag) without clear reason.