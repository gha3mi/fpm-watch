#!/usr/bin/env python3
"""Exercise release preparation with isolated Git/GitHub fakes and no publishing."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
FAKE_TOOL = r'''#!/usr/bin/env python3
import json
import os
from pathlib import Path
import sys

args = sys.argv[1:]
tool = Path(sys.argv[0]).name
with open(os.environ["RELEASE_TEST_CALLS"], "a", encoding="utf-8") as stream:
    stream.write(json.dumps([tool, *args]) + "\n")
if tool == "git":
    if args == ["rev-parse", "--show-toplevel"]:
        print(os.environ["RELEASE_TEST_ROOT"])
    elif args[:1] == ["describe"]:
        if os.environ["RELEASE_TEST_NO_TAGS"] == "1":
            sys.exit(128)
        print(os.environ["RELEASE_TEST_TAG"])
    elif args[:1] == ["log"]:
        print(os.environ["RELEASE_TEST_MESSAGE"] if "%s" in args[-1] else "abcdef123456")
    elif args[:1] == ["show"]:
        print("fix: repair watcher" if "%s" in args[2] else "Reviewer")
    elif args[:2] == ["rev-parse", "--short"]:
        print("abcdef1")
    elif args[:1] not in (["add"], ["commit"]):
        raise SystemExit("unexpected Git command: " + repr(args))
elif tool == "gh":
    if args[:2] == ["auth", "status"]:
        pass
    elif args[:2] == ["repo", "view"]:
        print("fixture/watch")
    elif args[:1] == ["api"] and "/pulls?" in args[1]:
        print("[]")
    elif args[:1] == ["api"] and "/commits/" in args[1]:
        print("reviewer")
    else:
        raise SystemExit("unexpected GitHub command: " + repr(args))
'''


@unittest.skipUnless(os.name == "posix" and shutil.which("bash") and shutil.which("jq"),
                     "release helper requires Bash and jq")
class ReleaseRegression(unittest.TestCase):
    def run_release(self, mode, no_tags=False, tag="v0.1.0", version="0.1.0",
                    message="fix: repair watcher", expected_status=0):
        temporary = tempfile.TemporaryDirectory(prefix="fpm-watch-release-")
        self.addCleanup(temporary.cleanup)
        project = Path(temporary.name)
        tools = project / "tools"
        tools.mkdir()
        for name in ("git", "gh"):
            executable = tools / name
            executable.write_text(FAKE_TOOL, encoding="utf-8")
            executable.chmod(0o755)
        shutil.copyfile(ROOT / "fpm.toml", project / "fpm.toml")
        if version is not None:
            (project / "VERSION").write_text(version + "\n", encoding="utf-8")
        log = project / "calls.jsonl"
        env = os.environ | {"PATH": str(tools) + os.pathsep + os.environ["PATH"],
                            "RELEASE_TEST_ROOT": str(project), "RELEASE_TEST_CALLS": str(log),
                            "RELEASE_TEST_NO_TAGS": str(int(no_tags)), "RELEASE_TEST_TAG": tag,
                            "RELEASE_TEST_MESSAGE": message}
        result = subprocess.run(["bash", str(ROOT / "release.sh"), mode], cwd=project, env=env,
                                input="y\ny\n", capture_output=True, text=True, timeout=15)
        self.assertEqual(result.returncode, expected_status, result.stdout + result.stderr)
        calls = [json.loads(line) for line in log.read_text().splitlines()]
        self.assertFalse(any(call[1] in ("push", "tag", "release") for call in calls), calls)
        return result, project, calls

    def test_local_release_uses_shared_version(self):
        result, project, _ = self.run_release("--local")
        self.assertIn("Next version: v0.1.1", result.stdout)
        self.assertEqual((project / "VERSION").read_text(), "0.1.1\n")
        # fpm resolves this reference (also exercised by check_version.f90).
        self.assertRegex((project / "fpm.toml").read_text(), r'version\s*=\s*"VERSION"')
        self.assertIn("v0.1.1", (project / "CHANGELOG.md").read_text())

    def test_first_release_without_tags(self):
        result, project, calls = self.run_release("--dry-run", no_tags=True)
        self.assertIn("Next version: v0.1.1", result.stdout)
        self.assertIn("/commits/v0.1.1", result.stdout)
        self.assertFalse(any("v0.0.0.." in arg for call in calls for arg in call))
        self.assertEqual((project / "VERSION").read_text(), "0.1.0\n")
        self.assertFalse((project / "CHANGELOG.md").exists())

    def test_first_local_release_does_not_downgrade_version(self):
        result, project, _ = self.run_release("--local", no_tags=True, version="2.8.9")
        self.assertIn("Next version: v2.8.10", result.stdout)
        self.assertEqual((project / "VERSION").read_text(), "2.8.10\n")

    def test_first_release_promotes_the_version_file_prerelease(self):
        result, project, _ = self.run_release("--local", no_tags=True, version="2.8.9-rc.1")
        self.assertIn("Next version: v2.8.9\n", result.stdout)
        self.assertEqual((project / "VERSION").read_text(), "2.8.9\n")

    def test_prerelease_and_build_versions(self):
        cases = (
            ("v0.1.0-rc.1", "fix: repair watcher", "v0.1.0"),
            ("v1.2.3-rc.8+build.4", "feat: add feature", "v1.3.0"),
            ("v1.2.3-alpha+sha", "fix!: change interface", "v2.0.0"),
            ("v1.2.3+build.4", "fix: repair watcher", "v1.2.4"),
            ("v1.2.99999999999999999999", "fix: repair watcher", "v1.2.100000000000000000000"),
        )
        for tag, message, expected in cases:
            with self.subTest(tag=tag, message=message):
                result, project, _ = self.run_release("--dry-run", tag=tag, message=message)
                self.assertIn("Next version: " + expected + "\n", result.stdout)
                self.assertNotIn("arithmetic", result.stderr)
                self.assertEqual((project / "VERSION").read_text(), "0.1.0\n")
                self.assertFalse((project / "CHANGELOG.md").exists())

    def test_invalid_or_missing_baseline_fails_before_mutations(self):
        cases = (
            dict(tag="v01.2.3"), dict(tag="v1.2.3-01"), dict(tag="v1.2"),
            dict(tag="v1.2.3-rc..1"), dict(no_tags=True, version=""),
            dict(no_tags=True, version=None),
        )
        for options in cases:
            with self.subTest(options=options):
                result, project, calls = self.run_release("--local", expected_status=1, **options)
                self.assertIn("❌", result.stderr)
                self.assertFalse(any(call[1] in ("add", "commit") for call in calls), calls)
                self.assertFalse((project / "CHANGELOG.md").exists())


if __name__ == "__main__":
    unittest.main()
