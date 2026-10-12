"""Exercise supervisor behavior against a built fpm-watch executable."""

import argparse
import json
import os
from pathlib import Path
import shlex
import signal
import subprocess
import sys
import tempfile
import time


def child():
    with open(os.environ["WATCH_TEST_LOG"], "a", encoding="utf-8") as stream:
        stream.write(json.dumps(sys.argv[2:]) + "\n")
    print("WATCH_PLATFORM_CHILD_OUTPUT", flush=True)
    print("WATCH_PLATFORM_CHILD_ERROR", file=sys.stderr, flush=True)
    if os.environ.get("WATCH_TEST_STDIN") == "1":
        print("WATCH_PLATFORM_STDIN:" + sys.stdin.readline().rstrip("\n"), flush=True)
    if "WATCH_TEST_BACKGROUND" in os.environ:
        subprocess.Popen([sys.executable, str(Path(__file__).resolve()), "--background"],
                         stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if os.environ.get("WATCH_TEST_WAIT") == "1":
        while True:
            time.sleep(1)
    return int(os.environ["WATCH_TEST_EXIT"])


def make_child(directory):
    script = str(Path(__file__).resolve())
    if os.name == "nt":
        executable = directory / "child.cmd"
        executable.write_text(
            "@" + subprocess.list2cmdline([sys.executable, script, "--child"]) + " %*\n",
            encoding="utf-8",
        )
    else:
        executable = directory / "child.sh"
        executable.write_text(
            "#!/bin/sh\nexec "
            + shlex.join([sys.executable, script, "--child"])
            + ' "$@"\n',
            encoding="utf-8",
        )
        executable.chmod(0o755)
    return executable


def invocations(log):
    return [json.loads(line) for line in log.read_text(encoding="utf-8").splitlines()]


def ensure_console():
    if os.name != "nt":
        return None
    import ctypes

    kernel = ctypes.WinDLL("kernel32", use_last_error=True)
    processes = (ctypes.c_ulong * 1)()
    if kernel.GetConsoleProcessList(processes, 1):
        return None
    if not kernel.AllocConsole():
        raise ctypes.WinError(ctypes.get_last_error())
    return kernel


def run_regressions(watcher):
    with tempfile.TemporaryDirectory(prefix="fpm-watch-platform-") as temporary:
        directory = Path(temporary)
        executable = make_child(directory)
        log = directory / "invocations.jsonl"
        env = os.environ.copy()
        env["WATCH_TEST_LOG"] = str(log)
        forwarded = ["run", "--", "--watch-restart-max", "12", "--watch-self", "unchanged"]
        command = [
            str(watcher), "--watch-ignore", "run", "--watch-auto-restart",
            "--watch-restart-max", "1", "--watch-restart-delay", "0",
            "--watch-self", str(executable), *forwarded,
        ]
        expected = [[f"--watch-child={attempt}", "--watch-log-format", "text", "--watch-ignore", "run", *forwarded]
                    for attempt in range(2)]
        for code in (2, 130):
            env["WATCH_TEST_EXIT"] = str(code)
            log.unlink(missing_ok=True)
            result = subprocess.run(command, cwd=directory, env=env, capture_output=True, text=True, timeout=10)
            assert result.returncode == code, result.stderr
            assert invocations(log) == expected, (invocations(log), result.stderr)
            assert result.stdout.count("WATCH_PLATFORM_CHILD_OUTPUT") == 2, result.stdout
            assert result.stderr.count("WATCH_PLATFORM_CHILD_ERROR") == 2, result.stderr

        log.unlink()
        env["WATCH_TEST_EXIT"] = "0"
        env["WATCH_TEST_STDIN"] = "1"
        result = subprocess.run(command, cwd=directory, env=env, capture_output=True,
                                input="forwarded-input\n", text=True, timeout=10)
        assert result.returncode == 0, result.stderr
        assert "WATCH_PLATFORM_STDIN:forwarded-input" in result.stdout, result.stdout
        del env["WATCH_TEST_STDIN"]

        for code in (0, 2):
            marker = directory / f"background-{code}.txt"
            env["WATCH_TEST_BACKGROUND"] = str(marker)
            env["WATCH_TEST_EXIT"] = str(code)
            result = subprocess.run(command, cwd=directory, env=env, capture_output=True, text=True, timeout=10)
            assert result.returncode == code, result.stderr
            deadline = time.monotonic() + 5
            expected_children = 1 if code == 0 else 2
            while len(list(directory.glob(marker.name + ".*.done"))) < expected_children:
                assert time.monotonic() < deadline, "normal command exit terminated its background child"
                time.sleep(0.02)
        del env["WATCH_TEST_BACKGROUND"]

        log.unlink()
        env["WATCH_TEST_WAIT"] = "1"
        allocated_console = ensure_console()
        options = {"creationflags": subprocess.CREATE_NEW_PROCESS_GROUP} if os.name == "nt" else {"start_new_session": True}
        process = subprocess.Popen(command, cwd=directory, env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, **options)
        try:
            deadline = time.monotonic() + 10
            while not log.exists() or log.stat().st_size == 0:
                if process.poll() is not None or time.monotonic() >= deadline:
                    raise AssertionError("supervised child did not start")
                time.sleep(0.02)
            if os.name == "nt":
                process.send_signal(signal.CTRL_BREAK_EVENT)
            else:
                os.killpg(process.pid, signal.SIGINT)
            _, stderr = process.communicate(timeout=5)
            assert process.returncode == 130, (process.returncode, stderr)
            assert len(invocations(log)) == 1, "watcher restarted after Ctrl+C"
            assert "restart #" not in stderr, stderr
        finally:
            if process.poll() is None:
                if os.name == "nt":
                    process.kill()
                else:
                    os.killpg(process.pid, signal.SIGKILL)
                process.communicate()
            if allocated_console is not None:
                allocated_console.FreeConsole()
    print("Supervisor platform regressions passed")


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--background":
        time.sleep(0.3)
        marker = Path(os.environ["WATCH_TEST_BACKGROUND"] + f".{os.getpid()}.tmp")
        marker.write_text("alive", encoding="utf-8")
        marker.replace(marker.with_suffix(".done"))
        sys.exit(0)
    if len(sys.argv) > 1 and sys.argv[1] == "--child":
        sys.exit(child())
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("watcher", type=Path, help="path to the built fpm-watch executable")
    args = parser.parse_args()
    run_regressions(args.watcher.resolve())
