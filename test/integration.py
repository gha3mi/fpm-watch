#!/usr/bin/env python3
# End-to-end regressions using temporary fpm projects and the built watcher.
import argparse
import contextlib
from datetime import datetime
import json
import os
from pathlib import Path
import re
import shutil
import signal
import subprocess
import tempfile
import time
import unittest

PARSER = argparse.ArgumentParser()
PARSER.add_argument("--watcher", required=True, type=Path)
ARGS, TEST_ARGS = PARSER.parse_known_args()
WATCHER = ARGS.watcher.resolve()
ANSI = re.compile(r"\x1b\[[0-9;]*m")
LOGFMT = re.compile(r'([A-Za-z_][A-Za-z_0-9.]*)=("(?:\\.|[^"\\])*"|[^\s]+)')


def records(text, format):
    """Parse the full structured stream, rejecting any unframed output."""
    result = []
    for line in text.splitlines():
        if format == "json":
            result.append(json.loads(line))
            continue
        record = {}
        position = 0
        for match in LOGFMT.finditer(line):
            assert match.start() == position, line
            key, value = match.groups()
            assert key not in record, line
            try:
                value = json.loads(value)
            except json.JSONDecodeError:
                pass
            record[key] = value
            position = match.end() + 1
        assert record and position == len(line) + 1, line
        result.append(record)
    return result


def output(path):
    return ANSI.sub("", path.read_text(errors="replace"))


def wait_for(process, log, predicate, timeout=15):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        text = output(log)
        if predicate(text):
            return text
        if process.poll() is not None:
            raise AssertionError(f"watcher exited {process.returncode}:\n{text}")
        time.sleep(0.02)
    raise AssertionError(f"timed out waiting for watcher:\n{output(log)}")


class WatchIntegration(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="fpm-watch-integration-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.project = self.root / "project"
        (self.project / "app").mkdir(parents=True)
        (self.project / "include").mkdir()
        (self.project / "src").mkdir()
        (self.project / "src/fixture.f90").write_text(
            "module watch_fixture\nimplicit none\ninteger, parameter :: offset = 0\nend module watch_fixture\n"
        )
        self.manifest = self.project / "fpm.toml"
        self.manifest_text = 'name="watch-fixture"\nversion="0.1.0"\n'
        self.manifest.write_text(self.manifest_text)
        (self.project / "include/value.inc").write_text("integer, parameter :: value = 1\n")
        (self.project / "app/first.f90").write_text(
            "program first\nuse watch_fixture, only: offset\nimplicit none\ninclude 'value.inc'\n"
            "integer :: i, n\ncharacter(:), allocatable :: a\n"
            "print *, 'WATCH_RESULT', value + offset\n"
            "do i=1, command_argument_count()\n"
            "call get_command_argument(i,length=n)\nallocate(character(n)::a)\n"
            "call get_command_argument(i,a)\nprint '(a)', '<'//a//'>'\n"
            "deallocate(a)\nend do\nend program first\n"
        )
        (self.project / "app/second.f90").write_text(
            "program second\nimplicit none\nprint *, 'WATCH_SECOND'\nend program second\n"
        )

    def once(self, *args):
        return subprocess.run(
            [str(WATCHER), "--watch-low-cpu", "--watch-once", *args],
            cwd=self.project, text=True, capture_output=True, timeout=30,
        )

    @contextlib.contextmanager
    def watch(self, *args, stderr_path=None):
        log = self.root / "watcher.log"
        (self.project / ".fpm-watch.stop").unlink(missing_ok=True)
        with contextlib.ExitStack() as stack:
            stream = stack.enter_context(log.open("w"))
            errors = stream if stderr_path is None else stack.enter_context(stderr_path.open("w"))
            process = subprocess.Popen(
                [str(WATCHER), "--watch-low-cpu", "--watch-no-run-on-start",
                 "--watch-poll", "0.05", "--watch-debounce", "0.1", *args],
                cwd=self.project, stdout=stream, stderr=errors,
            )
            try:
                wait_for(process, log, lambda text: "Ctrl+C or touch" in text)
                yield process, log
                (self.project / ".fpm-watch.stop").touch()
                self.assertEqual(process.wait(timeout=10), 0, output(log))
            finally:
                if process.poll() is None:
                    process.terminate()
                    try:
                        process.wait(timeout=5)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        process.wait()

    def test_arguments_and_empty_values(self):
        args = ["*.toml", "one", "", "three", "trailing  ", "hello;value"]
        direct = subprocess.run(["fpm", "run", "first", "--", *args], cwd=self.project,
                                text=True, capture_output=True, timeout=30)
        result = self.once("run", "first", "--", *args)
        self.assertEqual(direct.returncode, 0, direct.stdout + direct.stderr)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        received = [line[1:-1] for line in result.stdout.splitlines()
                    if line.startswith("<") and line.endswith(">")]
        baseline = [line[1:-1] for line in direct.stdout.splitlines()
                    if line.startswith("<") and line.endswith(">")]
        self.assertEqual(received, baseline)
        self.assertEqual(len(received), len(args))

    def test_structured_logs_separate_fpm_output(self):
        for format in ("json", "logfmt"):
            with self.subTest(format=format):
                result = self.once("--watch-log-format", format, "run", "first")
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                events = records(result.stdout, format)
                self.assertEqual([event["event"] for event in events],
                                 ["watch.ready", "command.started", "command.finished"])
                self.assertNotIn("WATCH_RESULT", result.stdout)
                self.assertIn("WATCH_RESULT", result.stderr)
                self.assertNotIn("\x1b", result.stdout)
                self.assertIsInstance(events[0]["file_count"], int)
                self.assertIs(events[0]["once"], True)
                self.assertEqual(events[-1]["exit_code"], 0)
                self.assertIsInstance(events[-1]["duration_ms"], int)
                for event in events:
                    self.assertIsNotNone(datetime.fromisoformat(event["timestamp"]).tzinfo)

    def test_logging_config_and_cli_override(self):
        self.manifest.write_text(self.manifest_text + '[extra.fpm-watch]\nlog-format="json"\n')
        result = self.once("--watch-no-run-on-start", "build")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(records(result.stdout, "json")[0]["event"], "watch.ready")
        result = self.once("--watch-log-format=logfmt", "--watch-no-run-on-start", "build")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(records(result.stdout, "logfmt")[0]["event"], "watch.ready")

    def test_structured_live_changes_and_manifest_recovery(self):
        for format in ("json", "logfmt"):
            with self.subTest(format=format):
                self.manifest.write_text(self.manifest_text)
                (self.project / "include/value.inc").write_text("integer, parameter :: value = 1\n")
                raw_log = self.root / "fpm.log"
                with self.watch("--watch-log-format", format, "--watch-verbose", "--watch-rescan", "0.3",
                                "run", "first", stderr_path=raw_log) as (process, log):
                    (self.project / "include/value.inc").write_text("integer, parameter :: value = 2\n")
                    wait_for(process, log, lambda text: "command.finished" in text)
                    events = records(output(log), format)
                    changed = next(event for event in events if event["event"] == "watch.changed")
                    paths = changed["paths"] if format == "json" else json.loads(changed["paths"])
                    self.assertIn("include/value.inc", [path.replace("\\", "/") for path in paths])
                    self.assertIsInstance(changed["file_count"], int)
                    self.manifest.write_text("name = [\n")
                    wait_for(process, log, lambda text: "watch.refresh_failed" in text)
                    new_format = "logfmt" if format == "json" else "json"
                    self.manifest.write_text(self.manifest_text + f'[extra.fpm-watch]\nlog-format="{new_format}"\n')
                    wait_for(process, log, lambda text: text.count("command.finished") >= 2)
                    self.manifest.write_text(self.manifest_text + '[extra.fpm-watch]\nlog-format="future-format"\n')
                    wait_for(process, log, lambda text: text.count("command.finished") >= 3)
                events = records(output(log), format)
                self.assertEqual(events[-1]["event"], "watch.stopped")
                self.assertEqual(events[-1]["reason"], "stop_file")
                self.assertTrue(any(event["event"] == "watch.refresh_failed" and event["level"] == "error"
                                    for event in events))
                self.assertNotIn("WATCH_RESULT", output(log))
                self.assertRegex(output(raw_log), r"WATCH_RESULT\s+2")

    def test_structured_silent_fpm(self):
        for format in ("json", "logfmt"):
            with self.subTest(format=format):
                result = self.once("--watch-log-format", format, "--watch-silent-fpm", "run", "first")
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                events = records(result.stdout, format)
                started = next(event for event in events if event["event"] == "command.started")
                self.assertIs(started["silent_fpm"], True)
                self.assertNotIn("WATCH_RESULT", result.stdout + result.stderr)

    def test_structured_log_escaping(self):
        name = "cafe" if os.name == "nt" else "café"
        flag = f'-DNAME="{name}" ' + "\\quoted\tline\nnext  "
        for format in ("json", "logfmt"):
            with self.subTest(format=format):
                result = self.once("--watch-log-format", format, "--watch-no-run-on-start", "build", "--flag", flag)
                self.assertEqual(result.returncode, 0, result.stderr)
                events = records(result.stdout, format)
                self.assertEqual(len(events), 1)
                self.assertIn(flag, events[0]["command"])

    def test_log_formatter_control_characters(self):
        result = subprocess.run(["fpm", "test", "--profile", "debug", "--target", "check_log", "--", "--fixture"],
                                cwd=Path(__file__).resolve().parents[1], capture_output=True,
                                text=True, encoding="utf-8", timeout=60)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        json_line = next(line for line in result.stdout.splitlines() if line.startswith('{"timestamp":'))
        logfmt_line = next(line for line in result.stdout.splitlines() if line.startswith('timestamp='))
        event = records(json_line, "json")[0]
        self.assertEqual(event["count"], 9223372036854775807)
        self.assertIsNone(event["missing"])
        self.assertTrue(event["message"].endswith(''.join(chr(i) for i in range(32))))
        self.assertIn("café", event["message"])
        self.assertEqual(event["invalid_utf8"], "\ufffd\ufffd")
        self.assertEqual(event["unicode"], "café € 😀")
        self.assertEqual(event["paths"], [event["message"]])
        logfmt = records(logfmt_line, "logfmt")[0]
        self.assertEqual(logfmt["msg"], event["message"])
        self.assertEqual(logfmt["invalid_utf8"], event["invalid_utf8"])
        self.assertEqual(logfmt["unicode"], event["unicode"])
        self.assertEqual(json.loads(logfmt["paths"]), event["paths"])

    def test_structured_initialization_error(self):
        self.manifest.write_text("name=[\n")
        for format in ("json", "logfmt"):
            with self.subTest(format=format):
                result = self.once("--watch-log-format", format, "build")
                self.assertNotEqual(result.returncode, 0)
                events = records(result.stdout, format)
                self.assertEqual(events[-1]["event"], "watch.init_failed")
                self.assertEqual(events[-1]["level"], "error")

    def test_logging_invalid_option(self):
        for args in (("--watch-log-format", "csv", "build"),
                     ("--watch-log-format", "json", "--watch-bogus", "build"),
                     ("--watch-log-format",)):
            with self.subTest(args=args):
                result = self.once(*args)
                self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
                if "json" in args:
                    self.assertEqual(records(result.stdout, "json")[-1]["event"], "cli.error")

    def test_quiet_logging_keeps_failure_record(self):
        (self.project / "app/failed.f90").write_text("program failed\nstop 7\nend program failed\n")
        for format in ("json", "logfmt"):
            with self.subTest(format=format):
                result = self.once("--watch-log-format", format, "--watch-quiet", "run", "failed")
                events = records(result.stdout, format)
                self.assertEqual(len(events), 1)
                self.assertEqual(events[0]["event"], "command.finished")
                self.assertEqual(events[0]["level"], "error")
                self.assertNotEqual(events[0]["exit_code"], 0)

    def test_structured_supervisor_failure(self):
        (self.project / "app/failed.f90").write_text("program failed\nstop 7\nend program failed\n")
        result = self.once("--watch-log-format", "json", "--watch-auto-restart",
                           "--watch-restart-max", "1", "--watch-restart-delay", "0", "run", "failed")
        self.assertNotEqual(result.returncode, 0)
        events = records(result.stdout, "json")
        self.assertEqual(sum(event["event"] == "command.finished" for event in events), 2)
        restarts = [event for event in events if event["event"] == "supervisor.restart"]
        self.assertEqual(restarts[0]["restart_count"], 1)
        self.assertEqual(events[-1]["event"], "supervisor.limit_reached")

    def test_debug_logging_has_debug_severity(self):
        result = self.once("--watch-log-format", "json", "--watch-debug", "--watch-no-run-on-start", "build")
        self.assertEqual(result.returncode, 0, result.stderr)
        events = records(result.stdout, "json")
        self.assertTrue(any(event["level"] == "debug" for event in events))
        self.assertNotIn("watch.configuration", [event["event"] for event in events])

    def test_supervisor_preserves_manifest_log_format(self):
        self.manifest.write_text(self.manifest_text + '[extra.fpm-watch]\nlog-format="json"\n')
        (self.project / "app/switch.f90").write_text(
            "program switch\nimplicit none\ninteger :: u\n"
            'open(newunit=u,file="fpm.toml",status="replace")\n'
            'write(u,\'(a)\') \'name="watch-fixture"\'\n'
            'write(u,\'(a)\') \'[extra.fpm-watch]\'\n'
            'write(u,\'(a)\') \'log-format="logfmt"\'\n'
            "close(u)\nstop 7\nend program switch\n"
        )
        result = self.once("--watch-auto-restart", "--watch-restart-max", "1",
                           "--watch-restart-delay", "0", "run", "switch")
        self.assertNotEqual(result.returncode, 0)
        events = records(result.stdout, "json")
        self.assertEqual(sum(event["event"] == "command.finished" for event in events), 2)
        self.assertEqual(events[-1]["event"], "supervisor.limit_reached")

    def test_text_logging_is_compact(self):
        result = self.once("--watch-no-run-on-start", "build")
        self.assertEqual(result.returncode, 0, result.stderr)
        watcher_lines = [line for line in result.stdout.splitlines() if " INFO " in line]
        self.assertEqual(len(watcher_lines), 1)
        self.assertRegex(watcher_lines[0], r"^\d{2}:\d{2}:\d{2} INFO  Watching ")
        self.assertNotIn("\x1b", result.stdout)

    def test_once_failure_status(self):
        (self.project / "app/failed.f90").write_text("program failed\nstop 7\nend program failed\n")
        direct = subprocess.run(["fpm", "run", "failed"], cwd=self.project,
                                capture_output=True, text=True, timeout=30)
        result = self.once("run", "failed")
        self.assertNotEqual(direct.returncode, 0)
        self.assertEqual(result.returncode, direct.returncode, result.stdout + result.stderr)

    def test_comma_separated_targets(self):
        result = self.once("--watch-no-run-on-start", "--watch-print-files", "run", "--target", "first,second")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("app/first.f90", result.stdout)
        self.assertIn("app/second.f90", result.stdout)

    def test_include_change_runs_command(self):
        with self.watch("run", "first") as (process, log):
            (self.project / "include/value.inc").write_text("integer, parameter :: value = 2\n")
            wait_for(process, log, lambda text: re.search(r"WATCH_RESULT\s+2", text))

    def test_manifest_change_runs_command(self):
        with self.watch("run", "first") as (process, log):
            self.manifest.write_text(self.manifest_text + "# changed manifest\n")
            wait_for(process, log, lambda text: re.search(r"WATCH_RESULT\s+1", text))

    def test_invalid_manifest_recovers(self):
        with self.watch("run", "first") as (process, log):
            self.manifest.write_text("name = [\n")
            wait_for(process, log, lambda text: "Watch list refresh failed" in text)
            self.assertIsNone(process.poll())
            self.manifest.write_text(self.manifest_text)
            wait_for(process, log, lambda text: re.search(r"WATCH_RESULT\s+1", text))

    def test_rescan_recovers_pending_manifest_and_options(self):
        self.recover_pending_manifest(rescan=True)

    def test_source_change_recovers_pending_manifest_and_options(self):
        self.recover_pending_manifest(rescan=False)

    def recover_pending_manifest(self, rescan):
        options = ["--watch-rescan", "0.15"] if rescan else []
        with self.watch(*options, "--watch-very-verbose", "build") as (process, log):
            self.manifest.write_text(
                self.manifest_text + '[dependencies]\ndep={path="../missing"}\n'
                + '[extra.fpm-watch]\nignore=["app/second.f90"]\n'
            )
            wait_for(process, log, lambda text: "Watch list refresh failed" in text)
            dep = self.root / "missing"
            (dep / "src").mkdir(parents=True)
            (dep / "fpm.toml").write_text('name="dep"\nversion="0.1.0"\n')
            (dep / "src/dep.f90").write_text("module dep\nimplicit none\nend module dep\n")
            if not rescan:
                source = self.project / "app/first.f90"
                source.write_text(source.read_text() + "! dependency now available\n")
            text = wait_for(process, log, lambda text: re.search(r"(Build|Application) succeeded", text))
            recovered = text.split("Watch list refresh failed", 1)[1]
            updated = recovered.split("INFO  Running ", 1)[0]
            self.assertIn("Watch list updated", updated)
            self.assertNotIn("app/second.f90", updated)
            self.assertIn("app/first.f90", updated)
            run_count = text.count("INFO  Running ")
            time.sleep(0.4)
            self.assertEqual(output(log).count("INFO  Running "), run_count)

    def test_rescan_runs_new_source(self):
        with self.watch("--watch-rescan", "0.15", "run") as (process, log):
            (self.project / "app/third.f90").write_text(
                "program third\nimplicit none\nprint *, 'WATCH_THIRD'\nend program third\n"
            )
            wait_for(process, log, lambda text: "WATCH_THIRD" in text)
            wait_for(process, log, lambda text: re.search(r"(Build|Application) succeeded", text))
            count = output(log).count("WATCH_THIRD")
            time.sleep(0.4)
            self.assertEqual(output(log).count("WATCH_THIRD"), count)

    def test_local_dependency_policy(self):
        dep = self.root / "dep"
        (dep / "src").mkdir(parents=True)
        (dep / "fpm.toml").write_text('name="dep"\nversion="0.1.0"\n')
        (dep / "src/dep.f90").write_text("module dep\nimplicit none\nend module dep\n")
        self.manifest.write_text(self.manifest_text + '[dependencies]\ndep={path="../dep"}\n')
        source = self.project / "app/second.f90"
        source.write_text("program second\nuse dep\nimplicit none\nend program second\n")
        disabled = self.once("--watch-no-deps", "--watch-no-run-on-start", "--watch-print-files", "build")
        enabled = self.once("--watch-deps", "--watch-no-run-on-start", "--watch-print-files", "build")
        self.assertEqual(disabled.returncode, 0, disabled.stdout + disabled.stderr)
        self.assertEqual(enabled.returncode, 0, enabled.stdout + enabled.stderr)
        self.assertNotIn("../dep/src/dep.f90", disabled.stdout)
        self.assertIn("../dep/src/dep.f90", enabled.stdout)
        self.assertIn("../dep/fpm.toml", enabled.stdout)

    def test_feature_dependency_under_build(self):
        dep = self.project / "build/featuredep"
        (dep / "src").mkdir(parents=True)
        (dep / "fpm.toml").write_text('name="dep"\nversion="0.1.0"\n')
        (dep / "src/dep.f90").write_text("module dep\nimplicit none\nend module dep\n")
        self.manifest.write_text(self.manifest_text + '[features.special.dependencies]\ndep={path="build/featuredep"}\n')
        (self.project / "app/second.f90").write_text("program second\nuse dep\nimplicit none\nend program second\n")
        result = self.once("--watch-deps", "--watch-no-run-on-start", "--watch-print-files", "build", "--features", "special")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("build/featuredep/src/dep.f90", result.stdout)
        self.assertIn("build/featuredep/fpm.toml", result.stdout)

    def interrupt_watcher(self, args, ready):
        log = self.root / "interrupt.log"
        with log.open("w") as stream:
            process = subprocess.Popen([str(WATCHER), "--watch-low-cpu", *args],
                                       cwd=self.project, stdout=stream, stderr=stream,
                                       start_new_session=True)
            try:
                wait_for(process, log, lambda text: ready in text)
                os.killpg(process.pid, signal.SIGINT)
                self.assertEqual(process.wait(timeout=10), 130, output(log))
                self.assertNotIn("restart #", output(log))
            finally:
                if process.poll() is None:
                    os.killpg(process.pid, signal.SIGKILL)
                    process.wait()

    @unittest.skipUnless(os.name == "posix", "console groups are checked by platform_regression.py on Windows")
    def test_interrupt_supervised_watcher(self):
        self.interrupt_watcher(["--watch-auto-restart", "--watch-no-run-on-start", "run", "first"],
                               "Ctrl+C or touch")

    @unittest.skipUnless(os.name == "posix", "console groups are checked by platform_regression.py on Windows")
    def test_interrupt_active_command(self):
        (self.project / "app/busy.f90").write_text(
            "program busy\nuse iso_fortran_env, only: output_unit, int64\n"
            "implicit none\ninteger(int64) :: tick\nprint *, 'WATCH_BUSY_READY'\n"
            "flush(output_unit)\ndo\ncall system_clock(tick)\nend do\nend program busy\n"
        )
        self.interrupt_watcher(["run", "busy"], "WATCH_BUSY_READY")


if __name__ == "__main__":
    if not WATCHER.is_file():
        PARSER.error(f"watcher not found: {WATCHER}")
    if not shutil.which("fpm"):
        PARSER.error("fpm must be on PATH")
    unittest.main(argv=[__file__, *TEST_ARGS])
