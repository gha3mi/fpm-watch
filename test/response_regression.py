#!/usr/bin/env python3
"""Exercise options-only fpm responses through the built watcher in temporary projects."""
import argparse
import json
import os
from pathlib import Path
import shlex
import subprocess
import tempfile
import time
import unittest


PARSER = argparse.ArgumentParser()
PARSER.add_argument("--watcher", required=True, type=Path)
ARGS, TEST_ARGS = PARSER.parse_known_args()
WATCHER = ARGS.watcher.resolve()


class ResponseRegression(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="fpm-watch-response-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.project = self.root / "project"
        (self.project / "app").mkdir(parents=True)
        (self.project / "src").mkdir()
        (self.project / "src/fixture.f90").write_text(
            'module response_fixture\nimplicit none\nend module response_fixture\n'
        )
        self.manifest = self.project / "fpm.toml"
        self.manifest.write_text('name="response-fixture"\nversion="0.1.0"\n')
        for name in ("first", "second"):
            (self.project / f"app/{name}.f90").write_text(
                f'program {name}\nuse response_fixture\nimplicit none\n'
                f'print *,"RESPONSE_{name}"\nend program {name}\n'
            )
        self.response = self.root / "args.rsp"
        self.response.write_text('options run\noptions --directory project\n')

    def once(self, *forwarded, cwd=None, env=None, watch_options=(), subcommand="run", format="json"):
        return subprocess.run(
            [str(WATCHER), "--watch-once", "--watch-no-run-on-start",
             "--watch-log-format=" + format, "--watch-print-files", *watch_options,
             subcommand, *(forwarded or ("@args",))],
            cwd=cwd or self.root, env=env, capture_output=True, text=True,
            encoding="utf-8", errors="replace", timeout=30,
        )

    def records(self, text, format="json"):
        if format == "json":
            return [json.loads(line) for line in text.splitlines()]
        return [dict(field.split("=", 1) for field in shlex.split(line))
                for line in text.splitlines()]

    def events(self, result, format="json", status=0):
        self.assertEqual(result.returncode, status, result.stdout + result.stderr)
        return self.records(result.stdout, format)

    def files(self, result):
        return [event["path"].replace("\\", "/") for event in self.events(result)
                if event["event"] == "watch.file"]

    def test_named_response_keeps_sources_and_original_forwarded_command(self):
        result = self.once()
        paths = self.files(result)
        self.assertTrue(any(path.endswith("app/first.f90") for path in paths), paths)
        self.assertTrue(any(path.endswith("app/second.f90") for path in paths), paths)
        ready = next(event for event in self.events(result) if event["event"] == "watch.ready")
        self.assertEqual(ready["root_count"], 2)
        self.assertIn("@args", ready["command"])
        self.assertNotIn("--directory", ready["command"])
        self.assertTrue(any(path.endswith("args.rsp") for path in paths), paths)

    def test_response_name_trimming_preserves_original_forwarded_argument(self):
        self.response.write_text('options run\noptions --directory project\noptions --target first\n')
        direct = subprocess.run(["fpm", "run", "@args "], cwd=self.root,
                                capture_output=True, text=True, timeout=30)
        self.assertEqual(direct.returncode, 0, direct.stdout + direct.stderr)
        self.assertIn("RESPONSE_first", direct.stdout + direct.stderr)
        self.assertNotIn("RESPONSE_second", direct.stdout + direct.stderr)
        result = self.once("@args ")
        paths = self.files(result)
        self.assertTrue(any(path.endswith("app/first.f90") for path in paths), paths)
        self.assertFalse(any(path.endswith("app/second.f90") for path in paths), paths)
        self.assertTrue(any(path.endswith("args.rsp") for path in paths), paths)
        ready = next(event for event in self.events(result) if event["event"] == "watch.ready")
        self.assertEqual(ready["root_count"], 1)
        self.assertIn("@args ", ready["command"])

    def test_response_target_profile_directory_and_project_configuration(self):
        self.response.write_text(
            'options run\noptions --directory "project"\n'
            'options --profile release\noptions --target first\n'
        )
        self.manifest.write_text(self.manifest.read_text() + '[extra.fpm-watch]\npoll=0.17\n')
        result = self.once()
        paths = self.files(result)
        self.assertTrue(any(path.endswith("app/first.f90") for path in paths), paths)
        self.assertFalse(any(path.endswith("app/second.f90") for path in paths), paths)
        config = next(event for event in self.events(result) if event["event"] == "watch.configuration")
        self.assertEqual(config["profile"], "release")
        self.assertAlmostEqual(config["poll_ms"], 170, delta=0.01)

    def test_response_preserves_the_common_fpm_verbose_option(self):
        paths = self.files(self.once("@args", "--verbose"))
        self.assertTrue(any(path.endswith("app/first.f90") for path in paths), paths)
        self.assertTrue(any(path.endswith("app/second.f90") for path in paths), paths)

    def add_test_program(self):
        (self.project / "test").mkdir()
        (self.project / "test/check.f90").write_text(
            'program check\nuse response_fixture\nimplicit none\nprint *,"RESPONSE_TEST"\nend program check\n'
        )

    def test_build_and_test_responses_use_the_native_model_scope(self):
        self.add_test_program()
        for command in ("build", "test"):
            with self.subTest(command=command):
                self.response.write_text(f'options {command}\noptions --directory project\n' +
                                         ('options --target check\n' if command == "test" else ''))
                result = self.once(subcommand=command)
                paths = self.files(result)
                ready = next(event for event in self.events(result) if event["event"] == "watch.ready")
                self.assertEqual(ready["mode"], command)
                self.assertTrue(any(path.endswith("test/check.f90") for path in paths), paths)
                if command == "test":
                    self.assertFalse(any(path.endswith("app/first.f90") for path in paths), paths)

    def test_response_subcommand_override_matches_native_fpm_execution(self):
        self.add_test_program()
        self.response.write_text('options test\noptions --directory project\noptions --target check\n')
        direct = subprocess.run(["fpm", "run", "@args"], cwd=self.root,
                                capture_output=True, text=True, timeout=30)
        self.assertEqual(direct.returncode, 0, direct.stdout + direct.stderr)
        self.assertIn("RESPONSE_TEST", direct.stdout + direct.stderr)
        result = subprocess.run([str(WATCHER), "--watch-once", "--watch-log-format=json",
                                 "run", "@args"], cwd=self.root, capture_output=True,
                                text=True, timeout=30)
        ready = next(event for event in self.events(result) if event["event"] == "watch.ready")
        self.assertEqual(ready["mode"], "test")
        self.assertIn("RESPONSE_TEST", result.stderr)
        self.assertNotIn("RESPONSE_first", result.stderr)

    def test_response_subcommand_overrides_without_explicit_targets(self):
        self.add_test_program()
        for original, effective in (("run", "test"), ("test", "run"),
                                    ("build", "run"), ("build", "test"),
                                    ("run", "build"), ("test", "build")):
            with self.subTest(original=original, effective=effective):
                self.response.write_text(f'options {effective}\noptions --directory project\n')
                result = self.once(subcommand=original)
                paths = self.files(result)
                ready = next(event for event in self.events(result) if event["event"] == "watch.ready")
                self.assertEqual(ready["mode"], effective)
                self.assertEqual(ready["root_count"], {"run": 2, "test": 1, "build": 0}[effective])
                self.assertEqual(any(path.endswith("app/first.f90") for path in paths), effective != "test")
                self.assertEqual(any(path.endswith("app/second.f90") for path in paths), effective != "test")
                self.assertEqual(any(path.endswith("test/check.f90") for path in paths), effective != "run")
                self.assertEqual(ready["command"].split()[:2], ["fpm", original])
                self.assertIn("@args", ready["command"])

    def test_response_override_retains_positional_target_names(self):
        self.add_test_program()
        self.response.write_text('options test\noptions --directory project\n')
        paths = self.files(self.once("@args", "check", subcommand="run"))
        self.assertTrue(any(path.endswith("test/check.f90") for path in paths), paths)
        self.assertFalse(any(path.endswith("app/first.f90") for path in paths), paths)

    def test_response_features_populate_the_typed_fpm_model(self):
        dependency = self.project / "deps/selected"
        (dependency / "src").mkdir(parents=True)
        (dependency / "fpm.toml").write_text('name="selected"\n')
        (dependency / "src/selected.f90").write_text('module selected\nimplicit none\nend module\n')
        (self.project / "app/first.f90").write_text(
            'program first\nuse response_fixture\nuse selected\nimplicit none\nend program\n'
        )
        self.manifest.write_text(self.manifest.read_text() +
                                 '[features.special.dependencies]\nselected={path="deps/selected"}\n')
        self.response.write_text('options run\noptions --directory project\n'
                                 'options --features special\noptions --target first\n')
        paths = self.files(self.once(watch_options=("--watch-deps",)))
        self.assertTrue(any(path.endswith("deps/selected/src/selected.f90") for path in paths), paths)
        self.assertFalse(any(path.endswith("app/second.f90") for path in paths), paths)

    def test_explicit_response_path(self):
        self.response.write_text('options run\noptions --directory project\noptions --target first\n')
        paths = self.files(self.once("@./args.rsp"))
        self.assertTrue(any(path.endswith("app/first.f90") for path in paths), paths)
        self.assertFalse(any(path.endswith("app/second.f90") for path in paths), paths)

    def test_named_response_uses_upstream_os_sections(self):
        self.response.write_text('options run\noptions --directory project\n'
                                 '@ReviewOS\noptions run\noptions --directory project\n'
                                 'options --target first\noptions --profile release\n')
        result = self.once("@args", env=os.environ | {"OSTYPE": "ReviewOS"})
        paths = self.files(result)
        self.assertTrue(any(path.endswith("app/first.f90") for path in paths), paths)
        self.assertFalse(any(path.endswith("app/second.f90") for path in paths), paths)

    def test_upstream_response_search_path(self):
        search = self.root / "responses"
        search.mkdir()
        self.response.replace(search / "args.rsp")
        paths = self.files(self.once(env=os.environ | {"CLI_RESPONSE_PATH": "responses"}))
        self.assertTrue(any(path.endswith("app/first.f90") for path in paths), paths)
        self.assertTrue(any(path.endswith("responses/args.rsp") for path in paths), paths)
        self.assertIn(str(self.root / "args.rsp").replace("\\", "/"), paths)

    def test_unused_lower_priority_response_does_not_reject_the_selected_file(self):
        search = self.root / "responses"
        search.mkdir()
        (search / "args.rsp").write_text('system echo unused\n')
        paths = self.files(self.once(env=os.environ | {"CLI_RESPONSE_PATH": "responses"}))
        self.assertTrue(any(path.endswith("app/first.f90") for path in paths), paths)

    def test_program_response_like_arguments_remain_program_arguments(self):
        result = self.once("--", "@does-not-exist", cwd=self.project)
        paths = self.files(result)
        self.assertTrue(any(path.endswith("app/first.f90") for path in paths), paths)
        self.assertTrue(any(path.endswith("app/second.f90") for path in paths), paths)

    def test_procedural_response_records_have_no_side_effects(self):
        marker = self.root / "unexpected.txt"
        for record in (f'system echo unexpected > "{marker}"',
                       f'! echo unexpected > "{marker}"',
                       'print UNEXPECTED_RESPONSE_PRINT', 'stop UNEXPECTED_RESPONSE_STOP'):
            with self.subTest(record=record):
                self.response.write_text('options run\noptions --directory project\n' + record + '\n')
                result = self.once()
                self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
                self.assertIn("unsupported record", result.stdout + result.stderr)
                self.assertFalse(marker.exists())

    def test_response_preflight_errors_honor_structured_log_formats(self):
        for format in ("json", "logfmt"):
            for response in ("@args", "@missing"):
                with self.subTest(format=format, response=response):
                    self.response.write_text('options run\noptions --directory project\nprint unexpected\n')
                    result = self.once(response, format=format)
                    events = self.events(result, format, status=2)
                    self.assertEqual(len(events), 1, result.stdout + result.stderr)
                    self.assertEqual(events[0]["event"], "cli.error")
                    self.assertEqual(events[0]["level"], "error")
                    self.assertNotIn("Use fpm-watch --watch-help", result.stderr)

    def test_response_getter_diagnostics_do_not_contaminate_structured_stdout(self):
        # This M_CLI2 version reports invalid logical values and returns false without a status output.
        self.response.write_text('options run\noptions --directory project\noptions --example bogus\n')
        for format in ("json", "logfmt"):
            with self.subTest(format=format):
                result = self.once(format=format)
                events = self.events(result, format)
                self.assertTrue(any(event["event"] == "watch.ready" for event in events))
                self.assertNotIn("bad logical expression", result.stdout)
                self.assertIn("bad logical expression", result.stderr)

    def test_compound_response_fallback_is_rejected(self):
        self.response.unlink()
        for name in ("fpm.rsp", "fpm-watch.rsp"):
            (self.root / name).write_text('@args\noptions run\noptions --directory project\n')
        result = self.once()
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertIn("compound response files are unsupported", result.stdout + result.stderr)

    def test_response_parser_preflights_raw_watcher_values_and_program_tokens(self):
        marker = self.root / "unexpected.txt"
        (self.root / "evil.rsp").write_text('options run\n'
                                           f'system echo unexpected > "{marker}"\n')
        for options in (("--watch-fpm", "@evil"), ("--watch-ignore", "@evil"), ("--watch-debug=@evil",)):
            with self.subTest(options=options):
                result = self.once(watch_options=options)
                self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
                self.assertIn("unsupported record", result.stdout + result.stderr)
                self.assertFalse(marker.exists())
        result = self.once("@args", "--", "@missing-program-input")
        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertIn("named options-only file", result.stdout + result.stderr)

    def wait_for(self, process, log, predicate):
        deadline = time.monotonic() + 20
        while time.monotonic() < deadline:
            text = log.read_text(encoding="utf-8", errors="replace")
            if predicate(text):
                return text
            if process.poll() is not None:
                self.fail(f"watcher exited {process.returncode}:\n{text}")
            time.sleep(0.025)
        self.fail(f"timed out waiting for watcher:\n{log.read_text()}")

    def test_response_edit_refreshes_target_selection_and_invalid_edit_recovers(self):
        log = self.root / "watch.log"
        marker = self.root / "unexpected.txt"
        with log.open("w") as stream:
            process = subprocess.Popen(
                [str(WATCHER), "--watch-no-run-on-start", "--watch-log-format=json",
                 "--watch-poll=0.05", "--watch-debounce=0.1", "run", "@args"],
                cwd=self.root, stdout=stream, stderr=stream,
            )
            try:
                self.wait_for(process, log, lambda text: '"event":"watch.ready"' in text)
                self.response.write_text('options run\noptions --directory project\n'
                                         f'system echo unexpected > "{marker}"\n')
                self.wait_for(process, log, lambda text: '"event":"watch.refresh_failed"' in text)
                self.assertFalse(marker.exists())
                self.assertIsNone(process.poll())
                self.response.write_text('options run\noptions --directory project\noptions --target first\n')
                text = self.wait_for(process, log, lambda text: '"event":"command.finished"' in text)
                self.assertIn("RESPONSE_first", text)
                self.assertNotIn("RESPONSE_second", text)
                (self.project / ".fpm-watch.stop").touch()
                self.assertEqual(process.wait(timeout=5), 0, log.read_text())
            finally:
                if process.poll() is None:
                    process.terminate()
                    process.wait(timeout=5)

    def test_failed_response_refresh_retains_project_and_structured_errors(self):
        for format in ("json", "logfmt"):
            with self.subTest(format=format):
                self.response.write_text('options run\noptions --directory project\noptions --target first\n')
                stop = self.project / ".fpm-watch.stop"
                stop.unlink(missing_ok=True)
                log = self.root / (format + "-refresh.log")
                errors = self.root / (format + "-refresh.stderr")
                with log.open("w") as stream, errors.open("w") as diagnostics:
                    process = subprocess.Popen(
                        [str(WATCHER), "--watch-no-run-on-start", "--watch-log-format=" + format,
                         "--watch-poll=0.05", "--watch-debounce=0.1", "run", "@args"],
                        cwd=self.root, stdout=stream, stderr=diagnostics,
                    )
                    try:
                        self.wait_for(process, log, lambda text: "watch.ready" in text)
                        self.response.write_text('options run\noptions --directory missing-project\n'
                                                 'print unsupported refresh record\n')
                        self.wait_for(process, log, lambda text: "watch.refresh_failed" in text)
                        self.assertIsNone(process.poll())
                        stop.touch()
                        self.assertEqual(process.wait(timeout=5), 0, log.read_text())
                        events = self.records(log.read_text(), format)
                        failures = [event for event in events if event["event"] == "watch.refresh_failed"]
                        self.assertEqual(len(failures), 1)
                        self.assertEqual(failures[0]["level"], "error")
                        self.assertFalse(any(event["event"] == "command.started" for event in events))
                    finally:
                        if process.poll() is None:
                            process.terminate()
                            process.wait(timeout=5)


if __name__ == "__main__":
    unittest.main(argv=[__file__, *TEST_ARGS])
