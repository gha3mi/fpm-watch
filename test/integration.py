#!/usr/bin/env python3
# End-to-end regressions using temporary fpm projects and the built watcher.
import argparse
import contextlib
from datetime import datetime
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import signal
import subprocess
import sys
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
    return ANSI.sub("", path.read_text(encoding="utf-8", errors="replace"))


def run_captured(command, cwd, timeout=30):
    # Compiler and application output may contain bytes outside UTF-8.
    return subprocess.run(command, cwd=cwd, capture_output=True, text=True,
                          encoding="utf-8", errors="replace", timeout=timeout)


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
        return run_captured(
            [str(WATCHER), "--watch-once", *args],
            cwd=self.project,
        )

    def assert_cycle_protocol(self, result, format, expected_cycles):
        events = [event for event in records(result.stdout, format)
                  if event["event"] in ("command.started", "command.finished")]
        self.assertEqual([event["event"] for event in events],
                         ["command.started", "command.finished"] * expected_cycles)
        ids = []
        for started, finished in zip(events[::2], events[1::2]):
            self.assertEqual(started["cycle_id"], finished["cycle_id"])
            ids.append(started["cycle_id"])
        self.assertEqual(len(set(ids)), expected_cycles)
        markers = re.findall(r"^@@fpm-watch:(started|finished):(.+)@@$", result.stderr, re.MULTILINE)
        self.assertEqual(markers, [(event["event"].split(".")[-1], str(event["cycle_id"]))
                                   for event in events])

    @contextlib.contextmanager
    def watch(self, *args, stderr_path=None):
        log = self.root / "watcher.log"
        (self.project / ".fpm-watch.stop").unlink(missing_ok=True)
        with contextlib.ExitStack() as stack:
            stream = stack.enter_context(log.open("w"))
            errors = stream if stderr_path is None else stack.enter_context(stderr_path.open("w"))
            process = subprocess.Popen(
                [str(WATCHER), "--watch-run-on-start=false",
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
        direct = run_captured(["fpm", "run", "first", "--", *args], cwd=self.project)
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

    def test_gui_cycle_protocol_and_executable_config(self):
        executable = shutil.which("fpm")
        self.manifest.write_text(self.manifest_text + '[extra.fpm-watch]\n'
                                 + 'fpm-executable=' + json.dumps(executable) + '\n'
                                 + 'silent-fpm=true\noutput-markers=true\n')
        result = self.once("--watch-log-format=json", "--watch-silent-fpm=false", "run", "first")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assert_cycle_protocol(result, "json", 1)
        events = records(result.stdout, "json")
        started, finished = events[-2:]
        self.assertIs(started["silent_fpm"], False)
        self.assertIn(executable, started["command"])
        lines = result.stderr.splitlines()
        begin = lines.index(f'@@fpm-watch:started:{started["cycle_id"]}@@')
        end = lines.index(f'@@fpm-watch:finished:{finished["cycle_id"]}@@')
        self.assertIn("WATCH_RESULT", "\n".join(lines[begin + 1:end]))
        result = self.once("--watch-log-format=json", "--watch-fpm", executable,
                           "--watch-output-markers=false", "--watch-silent-fpm=false", "run", "first")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertNotIn("@@fpm-watch:", result.stderr)

    def test_gui_cycles_remain_unique_after_supervisor_restart(self):
        (self.project / "app/failed.f90").write_text("program failed\nstop 7\nend program failed\n")
        for format in ("json", "logfmt"):
            with self.subTest(format=format):
                result = self.once("--watch-log-format", format, "--watch-output-markers",
                                   "--watch-auto-restart", "--watch-restart-max", "2",
                                   "--watch-restart-delay", "0", "run", "failed")
                self.assertNotEqual(result.returncode, 0)
                self.assert_cycle_protocol(result, format, 3)

    def test_gui_protocol_with_non_utf8_child_output(self):
        (self.project / "app/raw.f90").write_text(
            "program raw\nuse iso_fortran_env, only: error_unit\nimplicit none\n"
            "print '(a)', achar(139)//'RAW_STDOUT'\n"
            "write(error_unit, '(a)') achar(139)//'RAW_STDERR'\nend program raw\n"
        )
        result = self.once("--watch-log-format=json", "--watch-output-markers", "run", "raw")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assert_cycle_protocol(result, "json", 1)
        self.assertIn("\ufffdRAW_STDOUT", result.stderr)
        self.assertIn("\ufffdRAW_STDERR", result.stderr)

    def test_gui_markers_keep_events_in_quiet_mode(self):
        (self.project / "app/failed.f90").write_text("program failed\nstop 7\nend program failed\n")
        for format in ("json", "logfmt"):
            for target in ("first", "failed"):
                with self.subTest(format=format, target=target):
                    result = self.once("--watch-log-format", format, "--watch-output-markers",
                                       "--watch-quiet", "run", target)
                    self.assertEqual(result.returncode == 0, target == "first", result.stderr)
                    self.assert_cycle_protocol(result, format, 1)
                    self.assertEqual(len(records(result.stdout, format)), 2)
        result = self.once("--watch-output-markers", "--watch-quiet", "run", "first")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertNotIn("@@fpm-watch:", result.stderr)
        self.assertNotIn(" INFO ", result.stdout)

    def test_custom_executable_cli_overrides_manifest(self):
        self.manifest.write_text(self.manifest_text + '[extra.fpm-watch]\nfpm-executable="missing-fpm"\n')
        result = self.once("--watch-fpm=" + shutil.which("fpm"), "run", "first")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("WATCH_RESULT", result.stdout)

    def test_boolean_flags_override_manifest_supervisor_and_startup(self):
        self.manifest.write_text(self.manifest_text + '[extra.fpm-watch]\n'
                                 + 'verbosity=1\nauto-restart=true\nrun-on-start=false\n'
                                 + 'restart-max=1\nrestart-delay=0.0\n')
        result = self.once("--watch-log-format=json", "--watch-run-on-start=true",
                           "--watch-auto-restart=false", "build")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        events = records(result.stdout, "json")
        self.assertFalse(any(event["level"] == "debug" for event in events))
        self.assertNotIn("watch.file", [event["event"] for event in events])
        self.assertFalse(any(event["event"].startswith("supervisor.") for event in events))
        self.assertIn("command.finished", [event["event"] for event in events])

    def test_native_executable_resolution_ignores_argv0_and_old_override(self):
        result = subprocess.run(
            ["missing-watcher-argv0", "--watch-once", "--watch-auto-restart",
             "--watch-run-on-start=false", "--watch-log-format=json", "build"],
            executable=str(WATCHER), cwd=self.project,
            env=os.environ | {"FPM_WATCH_SELF": "missing-watcher-override"},
            capture_output=True, text=True, timeout=15,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        events = records(result.stdout, "json")
        self.assertIn("watch.ready", [event["event"] for event in events])
        self.assertNotIn("supervisor.restart", [event["event"] for event in events])

    def test_effective_configuration_updates_after_manifest_refresh(self):
        self.manifest.write_text(self.manifest_text + '[extra.fpm-watch]\n'
                                 + 'ignore=["generated/*"]\ninclude=["*.f90", "*.inc"]\n')
        with self.watch("--watch-log-format=json", "--watch-verbose=1", "--watch-rescan=0",
                        "build", "--compiler", "gfortran", "--profile", "debug",
                        stderr_path=self.root / "fpm.log") as (process, log):
            text = wait_for(process, log, lambda text: "watch.configuration" in text)
            config = next(event for event in records(text, "json") if event["event"] == "watch.configuration")
            self.assertEqual(config["compiler"], "gfortran")
            self.assertEqual(config["profile"], "debug")
            self.assertEqual(config["poll_ms"], 50)
            self.assertEqual(config["ignore"], ["generated/*"])
            self.assertEqual(config["include"], ["*.f90", "*.inc"])
            self.manifest.write_text(self.manifest_text + '[extra.fpm-watch]\n'
                                     + 'ignore=["new ignored/*"]\ninclude=["*.f90", "*.inc"]\n')
            text = wait_for(process, log, lambda text: '"new ignored/*"' in text
                            and "watch.refreshed" in text and "command.finished" in text)
            configs = [event for event in records(text, "json") if event["event"] == "watch.configuration"]
            self.assertEqual(configs[-1]["ignore"], ["new ignored/*"])
            self.assertEqual(configs[-1]["include"], ["*.f90", "*.inc"])

    def test_logging_config_and_cli_override(self):
        self.manifest.write_text(self.manifest_text + '[extra.fpm-watch]\nlog-format="json"\n')
        result = self.once("--watch-run-on-start=false", "build")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(records(result.stdout, "json")[0]["event"], "watch.ready")
        result = self.once("--watch-log-format=logfmt", "--watch-run-on-start=false", "build")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(records(result.stdout, "logfmt")[0]["event"], "watch.ready")

    def test_removed_wait_options_are_rejected(self):
        for flag in ("--watch-low-cpu", "--watch-low-cpu=false", "--watch-no-low-cpu",
                     "--watch-feature=stats", "--watch-no-deps", "--watch-no-rescan",
                     "--watch-no-run-on-start", "--watch-print-files", "--watch-debug",
                     "--watch-very-verbose", "--watch-self=missing"):
            with self.subTest(flag=flag):
                result = self.once("--watch-log-format=json", flag, "build")
                self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
                event = records(result.stdout, "json")[-1]
                self.assertEqual(event["event"], "cli.error")
                self.assertTrue("unknown watch flag" in event["message"] or "was removed;" in event["message"], event)

        result = self.once("run", "first", "--", "--watch-low-cpu")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("<--watch-low-cpu>", result.stdout)

    def test_nonfinite_manifest_timings_retain_defaults(self):
        self.manifest.write_text(self.manifest_text + '[extra.fpm-watch]\n'
                                 + 'poll=nan\ndebounce=inf\nrescan=nan\n')
        result = self.once("--watch-log-format=json", "--watch-verbose",
                           "--watch-run-on-start=false", "build")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        config = next(event for event in records(result.stdout, "json")
                      if event["event"] == "watch.configuration")
        self.assertEqual(config["poll_ms"], 500)
        self.assertEqual(config["debounce_ms"], 200)
        self.assertEqual(config["rescan_s"], 0)

    def test_explicit_invalid_cli_values_are_rejected(self):
        numeric = ("poll", "debounce", "rescan", "restart-delay", "restart-max", "verbose")
        for option in numeric:
            for value in ("", "broken", "/", ",", "0.1,0.2", "NaN", "Inf", "-1", "1e999"):
                with self.subTest(option=option, value=value):
                    result = self.once(f"--watch-{option}={value}", "build")
                    self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
                    self.assertNotIn("Running ", result.stdout)
        for option in ("deps", "run-on-start", "once", "silent-fpm", "output-markers", "auto-restart"):
            for value in ("", "broken", "true,false"):
                with self.subTest(option=option, value=value):
                    result = self.once(f"--watch-{option}={value}", "build")
                    self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        for option in ("poll", "debounce", "rescan", "restart-delay", "restart-max", "fpm", "log-format"):
            for following in (["build"], ["--watch-run-on-start=false", "build"]):
                with self.subTest(option=option, following=following):
                    result = self.once(f"--watch-{option}", *following)
                    self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
                    self.assertNotIn("Running ", result.stdout)

    def test_stop_marker_ends_supervisor_initialization_retries(self):
        self.manifest.write_text("name = [\n")
        marker = self.project / ".fpm-watch.stop"
        for delay in ("0.05", "30"):
            with self.subTest(delay=delay):
                marker.unlink(missing_ok=True)
                log = self.root / ("restart-" + delay + ".log")
                with log.open("w") as stream:
                    process = subprocess.Popen(
                        [str(WATCHER), "--watch-log-format=json", "--watch-auto-restart",
                         "--watch-restart-delay=" + delay, "build", "-C", str(self.project)],
                        cwd=self.root, stdout=stream, stderr=stream,
                    )
                    try:
                        wait_for(process, log, lambda text: "supervisor.restart" in text)
                        marker.touch()
                        self.assertEqual(process.wait(timeout=5), 0, output(log))
                        self.assertIn('"reason":"stop_file"', output(log))
                    finally:
                        if process.poll() is None:
                            process.terminate()
                            try:
                                process.wait(timeout=3)
                            except subprocess.TimeoutExpired:
                                process.kill()
                                process.wait()

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
                result = self.once("--watch-log-format", format, "--watch-run-on-start=false", "build", "--flag", flag)
                self.assertEqual(result.returncode, 0, result.stderr)
                events = records(result.stdout, format)
                self.assertEqual(len(events), 1)
                quoted_flag = subprocess.list2cmdline([flag]) if os.name == "nt" else shlex.quote(flag)
                self.assertIn(quoted_flag, events[0]["command"])

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

    def test_highest_verbosity_lists_startup_files_and_configuration(self):
        result = self.once("--watch-log-format", "json", "--watch-verbose=2", "--watch-run-on-start=false", "build")
        self.assertEqual(result.returncode, 0, result.stderr)
        events = records(result.stdout, "json")
        self.assertIn("watch.configuration", [event["event"] for event in events])
        paths = [event["path"].replace("\\", "/") for event in events if event["event"] == "watch.file"]
        self.assertIn("app/first.f90", paths)
        self.assertTrue(any(event["level"] == "debug" for event in events))

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
        result = self.once("--watch-run-on-start=false", "build")
        self.assertEqual(result.returncode, 0, result.stderr)
        watcher_lines = [line for line in result.stdout.splitlines() if " INFO " in line]
        self.assertEqual(len(watcher_lines), 1)
        self.assertRegex(watcher_lines[0], r"^\d{2}:\d{2}:\d{2} INFO  Watching ")
        self.assertNotIn("\x1b", result.stdout)

    def test_once_failure_status(self):
        (self.project / "app/failed.f90").write_text("program failed\nstop 7\nend program failed\n")
        direct = run_captured(["fpm", "run", "failed"], cwd=self.project)
        result = self.once("run", "failed")
        self.assertNotEqual(direct.returncode, 0)
        self.assertEqual(result.returncode, direct.returncode, result.stdout + result.stderr)

    def test_comma_separated_targets(self):
        result = self.once("--watch-run-on-start=false", "--watch-verbose=2", "run", "--target", "first,second")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("app/first.f90", result.stdout)
        self.assertIn("app/second.f90", result.stdout)

    def test_version_include_metadata_and_coalesced_edits_run_all_targets(self):
        self.manifest.write_text('name="watch-fixture"\nversion="VERSION"\n'
                                 '[preprocess.cpp]\nmacros=["PACKAGE_VERSION={version}"]\n')
        version = self.project / "VERSION"
        for mode, coalesced in (("run", False), ("run", True), ("test", False), ("test", True)):
            with self.subTest(mode=mode, coalesced=coalesced):
                version.write_text("1\n")
                directory = self.project / ("app" if mode == "run" else "test")
                directory.mkdir(exist_ok=True)
                for name in ("first", "second"):
                    (directory / f"{name}.f90").write_text(
                        f"program {name}\nuse watch_fixture, only: offset\nimplicit none\n"
                        + ('#if 0\n#include "../VERSION"\n#endif\n' if name == "first" else "") +
                        f"print *, 'METADATA_{name}', PACKAGE_VERSION + offset\nend program {name}\n"
                    )
                errors = self.root / "metadata-output.log"
                with self.watch("--watch-log-format=json", "--watch-debounce=0.2", mode,
                                stderr_path=errors) as (process, log):
                    version.write_text("2\n")
                    if coalesced:
                        source = directory / "first.f90"
                        source.write_text(source.read_text() + "! coalesced source edit\n")
                    text = wait_for(process, log, lambda text: '"event":"command.finished"' in text)
                    events = records(text, "json")
                    changed = next(event for event in events if event["event"] == "watch.changed")
                    self.assertIn("VERSION", changed["paths"])
                    self.assertEqual(f"{directory.name}/first.f90" in changed["paths"], coalesced)
                    started = next(event for event in events if event["event"] == "command.started")
                    self.assertEqual(started["command"], "fpm " + mode)
                    results = output(errors)
                    self.assertRegex(results, r"METADATA_first\s+2")
                    self.assertRegex(results, r"METADATA_second\s+2")

    def test_include_change_runs_command(self):
        with self.watch("run", "first") as (process, log):
            (self.project / "include/value.inc").write_text("integer, parameter :: value = 2\n")
            wait_for(process, log, lambda text: re.search(r"WATCH_RESULT\s+2", text))

    def test_renamed_target_uses_the_full_command_after_refresh(self):
        for mode in ("run", "test"):
            with self.subTest(mode=mode):
                directory = self.project / ("app" if mode == "run" else "test")
                directory.mkdir(exist_ok=True)
                for name in ("first", "second"):
                    (directory / f"{name}.f90").write_text(
                        f"program {name}\nuse watch_fixture, only: offset\nimplicit none\n"
                        f"print *, 'RENAMED_{name}', offset\nend program {name}\n"
                    )
                errors = self.root / "rename-output.log"
                with self.watch("--watch-log-format=json", mode, stderr_path=errors) as (process, log):
                    (directory / "first.f90").rename(directory / "renamed.f90")
                    text = wait_for(process, log, lambda text: '"event":"command.finished"' in text)
                    started = next(event for event in records(text, "json") if event["event"] == "command.started")
                    self.assertEqual(started["command"], "fpm " + mode)
                    self.assertIn("RENAMED_first", output(errors))
                    self.assertIn("RENAMED_second", output(errors))
                    self.assertNotIn("No executables to run", output(errors))

    def test_unchanged_target_mapping_keeps_affected_target_selection(self):
        errors = self.root / "selection-output.log"
        with self.watch("--watch-log-format=json", "run", stderr_path=errors) as (process, log):
            source = self.project / "app/first.f90"
            source.write_text(source.read_text() + "! source-only edit\n")
            text = wait_for(process, log, lambda text: '"event":"command.finished"' in text)
            started = next(event for event in records(text, "json") if event["event"] == "command.started")
            self.assertNotEqual(started["command"], "fpm run")
            self.assertIn("first", started["command"])
            self.assertNotIn("WATCH_SECOND", output(errors))
            self.assertIn("WATCH_RESULT", output(errors))

    def test_large_same_size_edit_runs_command(self):
        source = self.project / "src/fixture.f90"
        padding = "! " + "x" * 97 + "\n"
        source.write_text(padding * 80 + source.read_text() + padding * 280)
        with self.watch("run", "first") as (process, log):
            source.write_text(source.read_text().replace("offset = 0", "offset = 4"))
            wait_for(process, log, lambda text: re.search(r"WATCH_RESULT\s+5", text))

    def test_cpp_and_nested_include_changes_run_command(self):
        source = self.project / "app/first.F90"
        (self.project / "app/first.f90").rename(source)
        source.write_text(source.read_text().replace("include 'value.inc'", '#include "outer.inc"'))
        (self.project / "include/outer.inc").write_text("include 'value.inc'\n")
        self.manifest.write_text(self.manifest_text + "[preprocess.cpp]\n")
        with self.watch("run", "first") as (process, log):
            (self.project / "include/value.inc").write_text("integer, parameter :: value = 7\n")
            wait_for(process, log, lambda text: re.search(r"WATCH_RESULT\s+7", text))
            (self.project / "include/alternate.inc").write_text("integer, parameter :: value = 8\n")
            (self.project / "include/outer.inc").write_text("include 'alternate.inc'\n")
            wait_for(process, log, lambda text: re.search(r"WATCH_RESULT\s+8", text))
            (self.project / "include/alternate.inc").write_text("integer, parameter :: value = 9\n")
            # fpm owns include-cache invalidation; verify our detected edit invokes it.
            wait_for(process, log, lambda text: "Changed include/alternate.inc" in text
                     and text.count("INFO  Application succeeded") >= 3)

    def test_empty_include_removal_and_recreation(self):
        empty = self.project / "include/empty.inc"
        empty.touch()
        source = self.project / "app/first.f90"
        source.write_text(source.read_text().replace("include 'value.inc'", "include 'value.inc'\ninclude 'empty.inc'"))
        with self.watch("run", "first") as (process, log):
            empty.unlink()
            wait_for(process, log, lambda text: "Application failed" in text)
            empty.touch()
            wait_for(process, log, lambda text: re.search(r"WATCH_RESULT\s+1", text))

    def test_source_edit_refreshes_include_membership(self):
        source = self.project / "app/first.f90"
        added = self.project / "include/added.inc"
        added.write_text("integer, parameter :: value = 3\n")
        with self.watch("run", "first") as (process, log):
            source.write_text(source.read_text().replace("value.inc", "added.inc"))
            wait_for(process, log, lambda text: re.search(r"WATCH_RESULT\s+3", text))
            added.write_text("integer, parameter :: value = 6\n")
            wait_for(process, log, lambda text: "Changed include/added.inc" in text
                     and text.count("INFO  Application succeeded") >= 2)

    def test_edit_during_command_survives_dependency_refresh(self):
        source = self.project / "app/first.f90"
        added = self.project / "include/added.inc"
        added.write_text("integer, parameter :: value = 3\n")
        marker = self.root / "started"
        release = self.root / "release"
        helper = self.root / "command.py"
        helper.write_text(
            "from pathlib import Path\nimport time\n"
            f"marker = Path({str(marker)!r})\nrelease = Path({str(release)!r})\n"
            "with marker.open('a') as stream:\n    stream.write('started\\n')\n"
            "deadline = time.monotonic() + 10\n"
            "while not release.exists() and time.monotonic() < deadline:\n    time.sleep(0.02)\n"
        )
        command = self.root / ("command.cmd" if os.name == "nt" else "command.sh")
        if os.name == "nt":
            command.write_text("@" + subprocess.list2cmdline([sys.executable, str(helper)]) + "\n")
        else:
            command.write_text("#!/bin/sh\nexec " + shlex.join([sys.executable, str(helper)]) + "\n")
            command.chmod(0o755)
        with self.watch("--watch-fpm", str(command), "run", "first") as (process, log):
            source.write_text(source.read_text().replace("value.inc", "added.inc"))
            wait_for(process, log, lambda text: marker.exists())
            added.write_text("integer, parameter :: value = 6\n")
            release.touch()
            wait_for(process, log, lambda text: marker.read_text().count("started") >= 2
                     and text.count("INFO  Command succeeded") >= 2)
            time.sleep(0.25)
            self.assertEqual(marker.read_text().count("started"), 2, output(log))

    def test_directory_options_use_selected_project_and_configuration(self):
        self.manifest.write_text(self.manifest_text + '[extra.fpm-watch]\nlog-format="json"\n')
        # Include a valid but different parent manifest to detect accidental cwd use.
        (self.root / "fpm.toml").write_text('name="unrelated-parent"\n')
        for options in (("--directory", "project"), ("-C", "project"),
                        ("--directory=project",), ("-C=project",)):
            with self.subTest(options=options):
                result = run_captured([str(WATCHER), "--watch-once", "--watch-run-on-start=false",
                                       "--watch-verbose=2", "run", "first", *options], cwd=self.root)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                files = [event["path"] for event in records(result.stdout, "json")
                         if event["event"] == "watch.file"]
                self.assertTrue(any(path.endswith("app/first.f90") for path in files))
                self.assertTrue(all("unrelated" not in path for path in files))
        (self.root / "fpm.toml").unlink()
        result = run_captured([str(WATCHER), "--watch-once", "run", "first",
                               "--directory", "project"], cwd=self.root)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertRegex(result.stderr, r"WATCH_RESULT\s+1")

    def test_directory_changes_and_stop_file_use_selected_project(self):
        log = self.root / "directory.log"
        with log.open("w") as stream:
            process = subprocess.Popen([str(WATCHER), "--watch-run-on-start=false", "--watch-poll=0.05",
                                        "--watch-debounce=0.05", "run", "first", "-C", "project"],
                                       cwd=self.root, stdout=stream, stderr=stream)
            try:
                wait_for(process, log, lambda text: "Ctrl+C or touch" in text)
                (self.project / "include/value.inc").write_text("integer, parameter :: value = 4\n")
                wait_for(process, log, lambda text: re.search(r"WATCH_RESULT\s+4", text))
                (self.project / ".fpm-watch.stop").touch()
                self.assertEqual(process.wait(timeout=10), 0, output(log))
            finally:
                if process.poll() is None:
                    process.terminate()
                    process.wait(timeout=5)

    @unittest.skipUnless(Path("/proc/self/fd").is_dir(), "descriptor accounting uses Linux procfs")
    def test_model_refresh_contains_upstream_empty_include_leak(self):
        self.manifest.write_text(self.manifest_text + "[preprocess.cpp]\n")
        source = self.project / "app/first.f90"
        source.write_text(source.read_text().replace("include 'value.inc'", "include 'value.inc'\ninclude 'empty.inc'"))
        (self.project / "include/empty.inc").touch()
        with self.watch("--watch-verbose", "build") as (process, log):
            descriptors = Path(f"/proc/{process.pid}/fd")
            baseline = len(list(descriptors.iterdir()))
            for iteration in range(12):
                self.manifest.write_text(self.manifest_text + f"# refresh {iteration}\n[preprocess.cpp]\n")
                wait_for(process, log, lambda text: text.count("INFO  Build succeeded") >= iteration + 1)
                self.assertLessEqual(len(list(descriptors.iterdir())), baseline + 1, output(log))

    @unittest.skipUnless(os.name == "posix", "descriptor limits use the POSIX resource module")
    def test_many_empty_sources_under_descriptor_limit(self):
        import resource

        def limit_descriptors():
            _, hard = resource.getrlimit(resource.RLIMIT_NOFILE)
            soft = 256 if hard == resource.RLIM_INFINITY else min(256, hard)
            resource.setrlimit(resource.RLIMIT_NOFILE, (soft, hard))

        for index in range(300):
            (self.project / "src" / f"empty_{index}.f90").touch()
        result = subprocess.run(
            [str(WATCHER), "--watch-once", "--watch-run-on-start=false", "--watch-log-format=json", "build"],
            cwd=self.project, capture_output=True, text=True, encoding="utf-8", errors="replace",
            timeout=30, preexec_fn=limit_descriptors,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        ready = next(event for event in records(result.stdout, "json") if event["event"] == "watch.ready")
        self.assertGreaterEqual(ready["file_count"], 300)

    def test_model_workspace_cleanup_after_success_and_failure(self):
        temporary = self.root / "model-temp"
        temporary.mkdir()
        env = os.environ | {key: str(temporary) for key in ("TMPDIR", "TMP", "TEMP")}
        for invalid in (False, True):
            with self.subTest(invalid=invalid):
                if invalid:
                    self.manifest.write_text("name = [\n")
                result = subprocess.run([str(WATCHER), "--watch-once", "--watch-run-on-start=false", "build"],
                                        cwd=self.project, env=env, capture_output=True, text=True, timeout=15)
                self.assertEqual(result.returncode, int(invalid), result.stdout + result.stderr)
                self.assertEqual(list(temporary.iterdir()), [], result.stdout + result.stderr)

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
        with self.watch(*options, "--watch-verbose=2", "build") as (process, log):
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
        disabled = self.once("--watch-deps=false", "--watch-run-on-start=false", "--watch-verbose=2", "build")
        enabled = self.once("--watch-deps", "--watch-run-on-start=false", "--watch-verbose=2", "build")
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
        result = self.once("--watch-deps", "--watch-run-on-start=false", "--watch-verbose=2", "build", "--features", "special")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("build/featuredep/src/dep.f90", result.stdout)
        self.assertIn("build/featuredep/fpm.toml", result.stdout)

    def interrupt_watcher(self, args, ready):
        log = self.root / "interrupt.log"
        with log.open("w") as stream:
            process = subprocess.Popen([str(WATCHER), *args],
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
        self.interrupt_watcher(["--watch-auto-restart", "--watch-run-on-start=false", "run", "first"],
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
