"""Exercise real watcher supervision and portable external-command execution."""

import argparse
import faulthandler
import itertools
import json
import os
from pathlib import Path
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import time


def write_pid(path, pid=None):
    """Publish the complete PID before a controller can observe its marker."""
    temporary = path.with_name(path.name + f".{os.getpid()}.tmp")
    temporary.write_text(str(os.getpid() if pid is None else pid), encoding="ascii")
    temporary.replace(path)


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
    if "WATCH_TEST_TREE" in os.environ:
        marker = Path(os.environ["WATCH_TEST_TREE"])
        write_pid(marker.with_suffix(".child.pid"))
        subprocess.Popen([sys.executable, str(Path(__file__).resolve()), "--stubborn"])
    if os.environ.get("WATCH_TEST_WAIT") == "1":
        while True:
            time.sleep(1)
    return int(os.environ["WATCH_TEST_EXIT"])


def make_child(directory, mode="--child", name="child"):
    script = str(Path(__file__).resolve())
    if os.name == "nt":
        executable = directory / f"{name}.cmd"
        executable.write_text(
            "@" + subprocess.list2cmdline([sys.executable, script, mode]) + " %*\n",
            encoding="utf-8",
        )
    else:
        executable = directory / f"{name}.sh"
        executable.write_text(
            "#!/bin/sh\nexec "
            + shlex.join([sys.executable, script, mode])
            + ' "$@"\n',
            encoding="utf-8",
        )
        executable.chmod(0o755)
    return executable


def make_project(directory):
    (directory / "src").mkdir(parents=True)
    (directory / "src/m.f90").write_text("module m\nend module m\n", encoding="ascii")
    (directory / "app").mkdir()
    (directory / "app/main.f90").write_text(
        "program main\nuse m\nend program main\n", encoding="ascii")
    (directory / "fpm.toml").write_text('name="platform-fixture"\n', encoding="ascii")


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



def live_process(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    # An orphan may briefly remain as a zombie until the system reaps it.
    state = subprocess.run(["ps", "-o", "stat=", "-p", str(pid)], capture_output=True, text=True, timeout=3).stdout.strip()
    return bool(state) and not state.startswith("Z")


def descendant_processes(parent):
    rows = subprocess.check_output(["ps", "-axo", "pid=,ppid="], text=True, timeout=3)
    relationships = [tuple(map(int, row.split())) for row in rows.splitlines() if row.strip()]
    descendants = {parent}
    while True:
        expanded = descendants | {pid for pid, owner in relationships if owner in descendants}
        if expanded == descendants:
            return descendants - {parent}
        descendants = expanded


def kill_process_groups(known):
    """Kill only known trial groups, excluding the calling driver's group."""
    driver_group = os.getpgrp()
    groups = set()
    for pid in known:
        try:
            group = os.getpgid(pid)
        except ProcessLookupError:
            continue
        if group != driver_group:
            groups.add(group)
    for group in groups:
        try:
            os.killpg(group, signal.SIGKILL)
        except ProcessLookupError:
            pass


def cleanup_process_tree(process, descendants):
    """Kill only this trial's groups before collecting pipes with a bounded wait."""
    failure = sys.exc_info()[1]
    known = set(descendants) | {process.pid}
    known.update(descendant_processes(process.pid))
    kill_process_groups(known)
    if process.poll() is None:
        process.kill()
    try:
        process.communicate(timeout=5)
    except subprocess.TimeoutExpired:
        for stream in (process.stdout, process.stderr):
            if stream is not None:
                stream.close()
        message = "process-tree cleanup timed out; an unknown descendant still holds captured output"
        if failure is None:
            raise AssertionError(message) from None
        failure.add_note(message)


def direct_signal_regressions(command, directory, env):
    for nested in (False, True):
        invocation = list(command)
        if nested:
            bridge = directory / "nested.sh"
            inner = [command[0], "--watch-auto-restart", "--watch-once", "--watch-fpm",
                     command[command.index("--watch-fpm") + 1], "build"]
            bridge.write_text("#!/bin/sh\nexec " + shlex.join(inner) + "\n", encoding="utf-8")
            bridge.chmod(0o755)
            invocation[invocation.index("--watch-fpm") + 1] = str(bridge)
        for cancellation in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
            marker = directory / f"tree-{nested}-{cancellation}"
            trial_env = env | {"WATCH_TEST_TREE": str(marker), "WATCH_TEST_WAIT": "1", "WATCH_TEST_EXIT": "0"}
            process = subprocess.Popen(invocation, cwd=directory, env=trial_env,
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, start_new_session=True)
            descendants = []
            try:
                deadline = time.monotonic() + 10
                child = marker.with_suffix(".child.pid")
                descendant = marker.with_suffix(".descendant.pid")
                while not child.exists() or not descendant.exists():
                    assert process.poll() is None and time.monotonic() < deadline, "process tree did not start"
                    time.sleep(0.02)
                descendants = [int(path.read_text(encoding="ascii")) for path in (child, descendant)]
                process.send_signal(cancellation)
                _, stderr = process.communicate(timeout=5)
                assert process.returncode == 128 + cancellation, (process.returncode, stderr)
                assert "restart #" not in stderr, stderr
                assert all(not live_process(pid) for pid in descendants), "cancellation left a live descendant"
            finally:
                descendants.extend(int(path.read_text(encoding="ascii"))
                                   for path in (child, descendant) if path.exists())
                cleanup_process_tree(process, descendants)


def terminal_case(action, command):
    """Run one PTY trial in a fresh interpreter before any subprocess calls."""
    import pty
    import select

    faulthandler.register(signal.SIGUSR1)
    pid, terminal = pty.fork()
    if pid == 0:
        os.execve(command[0], command, os.environ)
    os.set_blocking(terminal, False)
    transcript = bytearray()
    reaped = False
    try:
        deadline = time.monotonic() + 10

        def read_until(marker, message):
            while marker not in transcript:
                assert time.monotonic() < deadline, (message, transcript.decode(errors="replace"))
                if not select.select([terminal], [], [], 0.1)[0]:
                    continue
                try:
                    chunk = os.read(terminal, 65536)
                except BlockingIOError:
                    continue
                assert chunk, ("terminal closed before expected output", message)
                transcript.extend(chunk)

        read_until(b"WATCH_PLATFORM_CHILD_OUTPUT", "terminal child did not start")
        if action == "close":
            os.close(terminal)
            terminal = -1
        else:
            os.write(terminal, b"terminal-input\n" if action == "input" else b"\x03")
        if action == "input":
            read_until(b"WATCH_PLATFORM_STDIN:terminal-input", "foreground child could not read its terminal")
        while True:
            waited, status = os.waitpid(pid, os.WNOHANG)
            if waited:
                reaped = True
                expected = {"input": 0, "interrupt": 130, "close": 129}[action]
                actual = os.waitstatus_to_exitcode(status)
                assert actual == expected, (action, actual, expected, transcript.decode(errors="replace"))
                break
            assert time.monotonic() < deadline, "terminal supervisor did not finish"
            time.sleep(0.02)
    finally:
        if not reaped:
            kill_process_groups({pid} | descendant_processes(pid))
            deadline = time.monotonic() + 3
            while not os.waitpid(pid, os.WNOHANG)[0]:
                assert time.monotonic() < deadline, "terminal child cleanup timed out"
                time.sleep(0.02)
        if terminal >= 0:
            os.close(terminal)


def terminal_regression(command, directory, env):
    """Contain macOS fork/PTY operations, including close, within a bounded child."""
    script = str(Path(__file__).resolve())
    for action in ("input", "interrupt", "close"):
        print(f"Terminal regression: {action}", flush=True)
        process = subprocess.Popen([sys.executable, script, "--terminal-case", action, *command],
                                   cwd=directory, env=env | {"WATCH_TEST_STDIN": "1", "WATCH_TEST_EXIT": "0"},
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, start_new_session=True)
        try:
            try:
                stdout, stderr = process.communicate(timeout=20)
            except subprocess.TimeoutExpired as error:
                # No watchdog thread is created before the helper's fork. Ask its
                # signal handler for the native-blocking Python frame instead.
                if process.poll() is None:
                    process.send_signal(signal.SIGUSR1)
                try:
                    _, diagnostic = process.communicate(timeout=0.5)
                except subprocess.TimeoutExpired as timeout:
                    diagnostic = timeout.stderr or b""
                if isinstance(diagnostic, bytes):
                    diagnostic = diagnostic.decode(errors="replace")
                print(diagnostic, file=sys.stderr, flush=True)
                raise AssertionError(f"terminal {action} helper timed out") from error
            assert process.returncode == 0, (action, stdout, stderr)
        finally:
            cleanup_process_tree(process, [])


def worker_cancellation_regression(watcher, directory, supervisor_depth, stubborn):
    project = directory / f"graph-cancel-{supervisor_depth}-{stubborn}"
    project.mkdir()
    (project / "src").mkdir()
    (project / "src" / "m.f90").write_text("module m\nend module m\n", encoding="ascii")
    (project / "fpm.toml").write_text('name="graph-cancel"\n', encoding="ascii")
    temporary = project / "temporary"
    temporary.mkdir()
    marker = project / "compiler.pid"
    command_marker = project / "command.started"
    outside = project / "outside"
    outside.mkdir()
    sentinel = outside / "keep"
    sentinel.write_text("keep", encoding="ascii")
    compiler = project / "slow-mpifort"
    compiler.write_text("#!/bin/sh\nexec " + shlex.join([
        sys.executable, str(Path(__file__).resolve()), "--slow-compiler"]) + ' "$@"\n', encoding="utf-8")
    compiler.chmod(0o755)
    fpm = project / "fpm.sh"
    fpm.write_text('#!/bin/sh\n: > "$WATCH_TEST_COMMAND_MARKER"\n', encoding="utf-8")
    fpm.chmod(0o755)
    env = os.environ | {"TMPDIR": str(temporary), "WATCH_TEST_COMPILER_PID": str(marker),
                        "WATCH_TEST_COMMAND_MARKER": str(command_marker), "WATCH_TEST_LINK_TARGET": str(outside),
                        "WATCH_TEST_COMPILER_STUBBORN": "1" if stubborn else "0"}
    invocation = [str(watcher), "--watch-once", "--watch-log-format=json", "--watch-fpm", str(fpm),
                  "build", "--compiler", str(compiler)]
    if supervisor_depth:
        invocation.insert(1, "--watch-auto-restart")
    process = subprocess.Popen(invocation, cwd=project, env=env,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, start_new_session=True)
    descendants = set()
    try:
        deadline = time.monotonic() + 10
        while not marker.exists():
            assert process.poll() is None and time.monotonic() < deadline, "graph worker never queried the slow compiler"
            time.sleep(0.02)
        # fpm recognizes an MPI-wrapper filename and queries -show while building
        # the initial model. Snapshot its whole tree before cancelling its parent.
        descendants.update(descendant_processes(process.pid))
        descendants.add(int(marker.read_text(encoding="ascii")))
        process.send_signal(signal.SIGINT)
        stdout, stderr = process.communicate(timeout=5)
        assert process.returncode == 130, (process.returncode, stdout, stderr)
        assert all(not live_process(pid) for pid in descendants), "graph cancellation left a live descendant"
        assert not command_marker.exists(), "a build command started after graph cancellation"
        events = [json.loads(line) for line in stdout.splitlines() if line.strip()]
        assert all(event["event"] != "command.started" for event in events), stdout
        assert all(event["event"] != "supervisor.restart" for event in events), stdout
        assert not list(temporary.iterdir()), "graph cancellation left temporary workspace contents"
        assert sentinel.read_text(encoding="ascii") == "keep", "workspace cleanup followed a symlink outside its directory"
    finally:
        if marker.exists():
            descendants.add(int(marker.read_text(encoding="ascii")))
        cleanup_process_tree(process, descendants)


def background_watcher_launcher():
    project = Path(os.environ["WATCH_TEST_BACKGROUND_PROJECT"])
    (project / "inherited-root.txt").write_text(
        os.environ.get("FPM_WATCH_WORKSPACE_ROOT", "ABSENT"), encoding="utf-8")
    with (project / "background.log").open("w", encoding="utf-8") as stream:
        process = subprocess.Popen(
            [os.environ["WATCH_TEST_BACKGROUND_WATCHER"], "--watch-run-on-start=false",
             "--watch-log-format=json", "--watch-poll=0.05", "--watch-debounce=0.05",
             "--watch-fpm", os.environ["WATCH_TEST_BACKGROUND_FPM"], "build"],
            stdin=subprocess.DEVNULL, stdout=stream, stderr=stream,
        )
    write_pid(project / "background.pid", process.pid)
    if os.environ["WATCH_TEST_BACKGROUND_READY"] == "1":
        wait_for_background(project, lambda text: '"event":"watch.ready"' in text)


def wait_for_background(project, predicate):
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        log = project / "background.log"
        text = log.read_text(encoding="utf-8", errors="replace") if log.exists() else ""
        if predicate(text):
            return text
        time.sleep(0.02)
    raise AssertionError("background watcher did not reach the expected state:\n" + text)


def background_workspace_regressions(watcher, directory):
    for ready_before_exit in (False, True):
        project = directory / f"background-workspace-{ready_before_exit}"
        (project / "src").mkdir(parents=True)
        temporary = project / "temporary"
        temporary.mkdir()
        source = project / "src/m.f90"
        source.write_text("module m\nimplicit none\nend module\n", encoding="ascii")
        (project / "fpm.toml").write_text('name="background-workspace"\n', encoding="ascii")
        launcher = make_child(project, "--background-watcher-launcher", "launcher")
        command = make_child(project)
        env = os.environ | {
            "TMPDIR": str(temporary), "TMP": str(temporary), "TEMP": str(temporary),
            "WATCH_TEST_BACKGROUND_PROJECT": str(project),
            "WATCH_TEST_BACKGROUND_WATCHER": str(watcher),
            "WATCH_TEST_BACKGROUND_FPM": str(command),
            "WATCH_TEST_BACKGROUND_READY": "1" if ready_before_exit else "0",
            "WATCH_TEST_LOG": str(project / "commands.jsonl"), "WATCH_TEST_EXIT": "0",
        }
        pid = None
        stopped = False
        try:
            result = subprocess.run(
                [str(watcher), "--watch-auto-restart", "--watch-once", "--watch-log-format=json",
                 "--watch-fpm", str(launcher), "build"], cwd=project, env=env,
                capture_output=True, text=True, timeout=20,
            )
            assert result.returncode == 0, (result.stdout, result.stderr)
            pid = int((project / "background.pid").read_text(encoding="ascii"))
            assert (project / "inherited-root.txt").read_text(encoding="utf-8") == "ABSENT", \
                "external command inherited the supervisor's workspace ownership"
            wait_for_background(project, lambda text: '"event":"watch.ready"' in text)
            source.write_text(source.read_text(encoding="ascii") + "! edit after owner exits\n", encoding="ascii")
            text = wait_for_background(project, lambda text: '"event":"command.finished"' in text)
            assert '"event":"watch.refresh_failed"' not in text, text
            assert "WATCH_PLATFORM_CHILD_OUTPUT" in text, text
            assert len(invocations(project / "commands.jsonl")) == 1, text
            assert not list(temporary.iterdir()), "background refresh left temporary workspaces"
            (project / ".fpm-watch.stop").touch()
            wait_for_background(project, lambda text: '"event":"watch.stopped"' in text)
            stopped = True
        finally:
            (project / ".fpm-watch.stop").touch()
            if not stopped:
                if pid is None and (project / "background.pid").exists():
                    pid = int((project / "background.pid").read_text(encoding="ascii"))
                if pid is not None:
                    try:
                        os.kill(pid, signal.SIGTERM)
                    except OSError:
                        pass


def check_forwarded_arguments(command, literals, directory, env, log, expected):
    log.unlink(missing_ok=True)
    result = subprocess.run([*command, *literals], cwd=directory, env=env,
                            capture_output=True, text=True, timeout=10)
    assert result.returncode == 0, (literals, result.stdout, result.stderr)
    assert invocations(log) == [expected + literals], (literals, invocations(log), result.stderr)
    assert not any(line.strip() == "WATCH_INJECTED" for line in result.stdout.splitlines()), (literals, result.stdout)


def windows_batch_regressions(watcher, command, literals, directory, env, log, expected):
    # A quote in one argument must not expose a later argument to cmd parsing.
    vectors = [[value] for value in literals]
    vectors.extend(list(pair) for pair in itertools.product(literals, repeat=2))
    for values in vectors:
        check_forwarded_arguments(command, values, directory, env, log, expected)
    script_index = command.index("--watch-fpm") + 1
    script = Path(command[script_index])
    batch = script.with_suffix(".bat")
    shutil.copyfile(script, batch)
    for alternate in (script.with_suffix(""), batch, str(script) + ".", str(script) + " "):
        invocation = list(command)
        invocation[script_index] = str(alternate)
        for values in [[value] for value in literals] + [literals]:
            check_forwarded_arguments(invocation, values, directory, env, log, expected)

    # Extensionless selection must prefer a native executable, even if invalid.
    project = directory / "native-batch-preference"
    make_project(project)
    native = project / "native.exe"
    shutil.copyfile(watcher, native)
    native.with_suffix(".cmd").write_text("@exit /b 99\n", encoding="ascii")
    fpm = make_child(project, name="command")
    with (project / "fpm.toml").open("a", encoding="utf-8") as manifest:
        manifest.write("[extra.fpm-watch]\nonce = true\nfpm-executable = " + json.dumps(str(fpm)) + "\n")
    native_log = project / "commands.jsonl"
    native_env = env | {"WATCH_TEST_LOG": str(native_log)}
    invocation = [str(watcher), "--watch-auto-restart", "--watch-restart-max=1", "--watch-restart-delay=0",
                  "--watch-once", "--watch-fpm", str(native.with_suffix("")), "build"]
    result = subprocess.run(invocation, cwd=project, env=native_env, capture_output=True, text=True, timeout=20)
    assert result.returncode == 0, (result.stdout, result.stderr)
    assert invocations(native_log) == [["build"]], "extensionless batch script shadowed a native executable"
    native_log.unlink()
    native.write_bytes(b"not a native executable\n")
    result = subprocess.run(invocation, cwd=project, env=native_env, capture_output=True, text=True, timeout=10)
    assert result.returncode == 127, (result.returncode, result.stdout, result.stderr)
    assert not native_log.exists(), "invalid native executable fell through to a batch script"
    invocation[invocation.index("--watch-fpm") + 1] = str(project / "missing.cmd")
    result = subprocess.run(invocation, cwd=project, env=native_env, capture_output=True, text=True, timeout=10)
    assert result.returncode == 127, (result.returncode, result.stdout, result.stderr)
    assert not native_log.exists(), "missing batch script was executed as shell text"


def run_regressions(watcher):
    with tempfile.TemporaryDirectory(prefix="fpm-watch-platform-") as temporary:
        directory = Path(temporary)
        make_project(directory)
        executable = make_child(directory)
        log = directory / "invocations.jsonl"
        env = os.environ.copy()
        env["WATCH_TEST_LOG"] = str(log)
        command = [
            str(watcher), "--watch-ignore=run", "--watch-auto-restart", "--watch-once",
            "--watch-restart-max", "1", "--watch-restart-delay", "0",
            "--watch-fpm", str(executable), "build",
        ]
        expected = [["build"], ["build"]]
        print("Supervisor regression: restart limits and exit status", flush=True)
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
        print("Supervisor regression: forwarded stdin", flush=True)
        result = subprocess.run(command, cwd=directory, env=env, capture_output=True,
                                input="forwarded-input\n", text=True, timeout=10)
        assert result.returncode == 0, result.stderr
        assert "WATCH_PLATFORM_STDIN:forwarded-input" in result.stdout, result.stdout
        del env["WATCH_TEST_STDIN"]

        log.unlink()
        literals = ["", " ", "tail  ", "%PATH%", "!PATH!", 'double"quote',
                    'quote" & echo WATCH_INJECTED & rem "', "a&b|c<d>e^f(g)", "dir\\", 'slash\\"quote']
        forwarded = ["run", "--", "--watch-restart-max", "12", "--watch-fpm", "unchanged"]
        argument_command = [*command[:-1], *forwarded]
        print("Supervisor regression: literal arguments", flush=True)
        check_forwarded_arguments(argument_command, literals, directory, env, log, forwarded)
        if os.name == "nt":
            windows_batch_regressions(watcher, argument_command, literals, directory, env, log, forwarded)

        if os.name == "posix":
            terminal_regression(command, directory, env)
            print("Supervisor regression: direct and nested signals", flush=True)
            direct_signal_regressions(command, directory, env)
            for supervisor_depth in (0, 1):
                for stubborn in (False, True):
                    print(f"Supervisor regression: graph cancellation depth={supervisor_depth} stubborn={stubborn}", flush=True)
                    worker_cancellation_regression(watcher, directory, supervisor_depth, stubborn)

        print("Supervisor regression: surviving background commands", flush=True)
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
        print("Supervisor regression: background workspace ownership", flush=True)
        background_workspace_regressions(watcher, directory)

        print("Supervisor regression: console interrupt", flush=True)
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
            if os.name == "posix":
                cleanup_process_tree(process, [])
            elif process.poll() is None:
                process.kill()
                process.communicate(timeout=5)
            if allocated_console is not None:
                allocated_console.FreeConsole()
    print("Supervisor platform regressions passed", flush=True)


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--terminal-case":
        terminal_case(sys.argv[2], sys.argv[3:])
        sys.exit(0)
    if len(sys.argv) > 1 and sys.argv[1] == "--background-watcher-launcher":
        background_watcher_launcher()
        sys.exit(0)
    if len(sys.argv) > 1 and sys.argv[1] == "--slow-compiler":
        if os.environ.get("WATCH_TEST_COMPILER_STUBBORN") == "1":
            signal.signal(signal.SIGINT, signal.SIG_IGN)
            signal.signal(signal.SIGTERM, signal.SIG_IGN)
        scratch = Path(os.environ["TMPDIR"])
        (scratch / "nested").mkdir()
        (scratch / "nested" / "native.tmp").write_text("scratch", encoding="ascii")
        (scratch / "outside-link").symlink_to(os.environ["WATCH_TEST_LINK_TARGET"], target_is_directory=True)
        write_pid(Path(os.environ["WATCH_TEST_COMPILER_PID"]))
        while True:
            time.sleep(1)
    if len(sys.argv) > 1 and sys.argv[1] == "--stubborn":
        signal.signal(signal.SIGINT, signal.SIG_IGN)
        signal.signal(signal.SIGTERM, signal.SIG_IGN)
        write_pid(Path(os.environ["WATCH_TEST_TREE"]).with_suffix(".descendant.pid"))
        while True:
            time.sleep(1)
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
    # PTY trials fork only in fresh helper interpreters, never in this process
    # after creating faulthandler's watchdog thread or calling subprocess APIs.
    faulthandler.enable()
    faulthandler.dump_traceback_later(600 if os.name == "nt" else 180, exit=True)
    try:
        run_regressions(args.watcher.resolve())
    finally:
        faulthandler.cancel_dump_traceback_later()
