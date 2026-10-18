"""Stress native POSIX process completion and caller-owned resources."""

import os
from pathlib import Path
import shlex
import subprocess
import tempfile


HARNESS = r"""
#define _POSIX_C_SOURCE 200809L
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>

int fpm_watch_execute(const char *, int, int *, int *);
void fpm_watch_launch_operation(char *, size_t);

static void caller_handler(int number) { (void)number; }

int main(void) {
    struct sigaction caller = {0}, original, after;
    sigset_t blocked, original_mask, expected_mask, actual_mask;
    const int signals[] = {SIGHUP, SIGINT, SIGQUIT, SIGTERM, SIGCHLD, SIGUSR1};
    const char *commands[] = {"true", "exit 2", "exit 130"};
    const int statuses[] = {0, 2, 130};
    char operation[64];
    int descriptor, expected_descriptor, interrupted, error, status;

    caller.sa_handler = caller_handler;
    caller.sa_flags = SA_RESTART;
    sigemptyset(&caller.sa_mask);
    sigemptyset(&blocked);
    sigaddset(&blocked, SIGCHLD);
    sigaddset(&blocked, SIGUSR1);
    if (sigaction(SIGCHLD, &caller, &original) != 0 ||
        sigprocmask(SIG_BLOCK, &blocked, &original_mask) != 0 ||
        sigprocmask(SIG_SETMASK, NULL, &expected_mask) != 0) return 1;
    expected_descriptor = open("/dev/null", O_RDONLY);
    if (expected_descriptor < 0) return 1;
    close(expected_descriptor);

    for (int iteration = 0; iteration < 3000; ++iteration) {
        int index = iteration % 3;
        status = fpm_watch_execute(commands[index], 0, &interrupted, &error);
        fpm_watch_launch_operation(operation, sizeof(operation));
        if (error || interrupted || status != statuses[index] || operation[0] != '\0') {
            fprintf(stderr, "iteration=%d status=%d interrupted=%d os_error=%d operation=%s\n",
                    iteration, status, interrupted, error, operation);
            return 1;
        }
        if (sigaction(SIGCHLD, NULL, &after) != 0 || after.sa_handler != caller_handler ||
            !(after.sa_flags & SA_RESTART) || sigprocmask(SIG_SETMASK, NULL, &actual_mask) != 0) return 1;
        for (size_t i = 0; i < sizeof(signals) / sizeof(signals[0]); ++i) {
            if (sigismember(&actual_mask, signals[i]) != sigismember(&expected_mask, signals[i])) {
                fprintf(stderr, "caller signal mask changed at iteration %d\n", iteration);
                return 1;
            }
        }
    }
    descriptor = open("/dev/null", O_RDONLY);
    if (descriptor != expected_descriptor) {
        fprintf(stderr, "descriptor leak: first=%d final=%d\n", expected_descriptor, descriptor);
        return 1;
    }
    close(descriptor);
    sigaction(SIGCHLD, &original, NULL);
    sigprocmask(SIG_SETMASK, &original_mask, NULL);
    puts("3000 native commands passed: statuses, signal state, and descriptors preserved");
    return 0;
}
"""


if __name__ == "__main__":
    if os.name != "posix":
        raise SystemExit("native POSIX regression requires a POSIX host")
    repository = Path(__file__).resolve().parents[1]
    compiler = shlex.split(os.environ.get("FPM_CC", "cc"))
    with tempfile.TemporaryDirectory(prefix="fpm-watch-native-") as temporary:
        directory = Path(temporary).resolve()
        source = directory / "process.c"
        executable = directory / "process"
        source.write_text(HARNESS, encoding="ascii")
        subprocess.run([*compiler, "-O2", "-Wall", "-Wextra", "-Werror", str(source),
                        str(repository / "src/watch_process.c"), "-o", str(executable)], check=True)
        subprocess.run([str(executable)], check=True, timeout=90)
