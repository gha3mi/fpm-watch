#ifndef _WIN32
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#endif
#include <stdio.h>

enum { LAUNCH_OPERATION_CAPACITY = 64 };
static char last_launch_operation[LAUNCH_OPERATION_CAPACITY] = "";

static void copy_launch_operation(char *buffer, size_t capacity, const char *operation) {
    size_t i = 0;
    if (capacity == 0) return;
    while (i < capacity - 1 && operation[i] != '\0') {
        buffer[i] = operation[i];
        ++i;
    }
    buffer[i] = '\0';
}

/* Retain operation details without changing the established execution ABI. */
void fpm_watch_launch_operation(char *buffer, size_t capacity) {
    copy_launch_operation(buffer, capacity, last_launch_operation);
}

static int launch_failure(int error, int *launch_error, const char *operation) {
    *launch_error = error;
    copy_launch_operation(last_launch_operation, sizeof(last_launch_operation), operation);
    return 127;
}

#ifdef _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <errno.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdint.h>
#define watch_dup2 _dup2
#define watch_close _close

/* Preserve watcher options and forwarded patterns during MinGW startup. */
#if defined(__MINGW64_VERSION_MAJOR)
int _dowildcard = 0;
#elif defined(__MINGW32__)
int _CRT_glob = 0;
#endif

char *fpm_watch_command_executable(const char *command);
char *fpm_watch_batch_command(const char *command, const char *script);

static volatile LONG interrupt_status = 0;

static BOOL WINAPI watch_console_handler(DWORD event) {
    if (event != CTRL_C_EVENT && event != CTRL_BREAK_EVENT) return FALSE;
    InterlockedExchange(&interrupt_status, 130);
    return TRUE;
}

static HANDLE inherited_standard_handle(DWORD identifier, DWORD access, int discard) {
    HANDLE source = discard ? NULL : GetStdHandle(identifier), inherited = NULL;
    SECURITY_ATTRIBUTES attributes;

    if (source != NULL && source != INVALID_HANDLE_VALUE) {
        if (!DuplicateHandle(GetCurrentProcess(), source, GetCurrentProcess(), &inherited,
                             0, TRUE, DUPLICATE_SAME_ACCESS)) return INVALID_HANDLE_VALUE;
        return inherited;
    }
    attributes.nLength = sizeof(attributes);
    attributes.lpSecurityDescriptor = NULL;
    attributes.bInheritHandle = TRUE;
    return CreateFileA("NUL", access, FILE_SHARE_READ | FILE_SHARE_WRITE, &attributes,
                       OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
}

/* Only existing batch scripts go through cmd; missing executables and invalid
 * native binaries remain launch errors. Resolve the script before dispatch. */
static int resolve_batch_script(const char *command, char *resolved, DWORD capacity, int allow_extensionless) {
    char *executable = fpm_watch_command_executable(command);
    const char *filename, *extension, *slash, *end;
    DWORD length = 0;
    int error = 0;
    if (executable == NULL) return errno == ENOMEM ? ERROR_NOT_ENOUGH_MEMORY : ERROR_INVALID_PARAMETER;
    filename = strrchr(executable, '\\');
    slash = strrchr(executable, '/');
    if (filename == NULL || (slash != NULL && slash > filename)) filename = slash;
    if (filename == NULL) filename = executable; else ++filename;
    /* Win32 resolves trailing periods/spaces before deciding the file type.
     * Classify that suffix too, retaining the original path for lookup. */
    end = executable + strlen(executable);
    while (end > filename && (end[-1] == '.' || end[-1] == ' ')) --end;
    extension = end;
    while (extension > filename && extension[-1] != '.') --extension;
    if (extension > filename) --extension; else extension = NULL;
    if (extension != NULL) {
        if (end - extension == 4 &&
            ((tolower((unsigned char)extension[1]) == 'c' && tolower((unsigned char)extension[2]) == 'm' &&
              tolower((unsigned char)extension[3]) == 'd') ||
             (tolower((unsigned char)extension[1]) == 'b' && tolower((unsigned char)extension[2]) == 'a' &&
              tolower((unsigned char)extension[3]) == 't'))) {
            length = SearchPathA(NULL, executable, NULL, capacity, resolved, NULL);
        }
    } else if (allow_extensionless) {
        length = SearchPathA(NULL, executable, ".cmd", capacity, resolved, NULL);
        if (length == 0) length = SearchPathA(NULL, executable, ".bat", capacity, resolved, NULL);
    }
    if (length == 0) error = ERROR_FILE_NOT_FOUND;
    else if (length >= capacity) error = ERROR_FILENAME_EXCED_RANGE;
    free(executable);
    return error;
}

int fpm_watch_execute(const char *command, int silent, int *interrupted, int *launch_error) {
    char *line = (char *)malloc(strlen(command) + 1);
    STARTUPINFOEXA startup_info;
    STARTUPINFOA *startup = &startup_info.StartupInfo;
    PROCESS_INFORMATION process;
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
    HANDLE job = NULL, handles[3];
    SIZE_T attribute_size = 0;
    DWORD status = 127, error = 0, waited;
    DWORD creation_flags = CREATE_SUSPENDED | EXTENDED_STARTUPINFO_PRESENT;
    int handler_installed = 0, attributes_initialized = 0, batch = 0, batch_error;
    char command_processor[MAX_PATH + 9], script[MAX_PATH];

    *interrupted = 0;
    *launch_error = 0;
    last_launch_operation[0] = '\0';
    if (line == NULL) return launch_failure(ERROR_NOT_ENOUGH_MEMORY, launch_error, "allocate(command)");
    strcpy(line, command);
    ZeroMemory(&startup_info, sizeof(startup_info));
    ZeroMemory(&process, sizeof(process));
    ZeroMemory(&limits, sizeof(limits));
    startup->cb = sizeof(startup_info);
    startup->dwFlags = STARTF_USESTDHANDLES;
    startup->hStdInput = inherited_standard_handle(STD_INPUT_HANDLE, GENERIC_READ, 0);
    if (startup->hStdInput == INVALID_HANDLE_VALUE) {
        error = GetLastError();
        goto cleanup;
    }
    startup->hStdOutput = inherited_standard_handle(silent == 2 ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE,
                                                  GENERIC_WRITE, silent == 1);
    if (startup->hStdOutput == INVALID_HANDLE_VALUE) {
        error = GetLastError();
        goto cleanup;
    }
    startup->hStdError = inherited_standard_handle(STD_ERROR_HANDLE, GENERIC_WRITE, silent == 1);
    if (startup->hStdError == INVALID_HANDLE_VALUE) {
        error = GetLastError();
        goto cleanup;
    }
    /* Give the child only these duplicates, not the original stdio handles or
     * unrelated inheritable handles owned by the watcher or its caller. */
    handles[0] = startup->hStdInput;
    handles[1] = startup->hStdOutput;
    handles[2] = startup->hStdError;
    InitializeProcThreadAttributeList(NULL, 1, 0, &attribute_size);
    if (attribute_size == 0) { error = GetLastError(); goto cleanup; }
    startup_info.lpAttributeList = malloc(attribute_size);
    if (startup_info.lpAttributeList == NULL) { error = ERROR_NOT_ENOUGH_MEMORY; goto cleanup; }
    if (!InitializeProcThreadAttributeList(startup_info.lpAttributeList, 1, 0, &attribute_size)) {
        error = GetLastError();
        goto cleanup;
    }
    attributes_initialized = 1;
    if (!UpdateProcThreadAttribute(startup_info.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
                                   handles, sizeof(handles), NULL, NULL)) {
        error = GetLastError();
        goto cleanup;
    }
    job = CreateJobObjectA(NULL, NULL);
    if (job == NULL) {
        error = GetLastError();
        goto cleanup;
    }
    limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if (!SetInformationJobObject(job, JobObjectExtendedLimitInformation, &limits, sizeof(limits))) {
        error = GetLastError();
        goto cleanup;
    }
    InterlockedExchange(&interrupt_status, 0);
    if (!SetConsoleCtrlHandler(watch_console_handler, TRUE)) {
        error = GetLastError();
        if (error != ERROR_INVALID_HANDLE) goto cleanup;
        error = 0;
        creation_flags |= CREATE_NO_WINDOW;
    } else {
        handler_installed = 1;
    }
    /* Windows can implicitly launch .cmd/.bat through cmd even when
     * CreateProcess succeeds. Select the explicit batch encoding first so
     * CRT-quoted argument contents never reach that implicit shell path. */
    batch_error = resolve_batch_script(command, script, MAX_PATH, 0);
    if (batch_error == 0) batch = 1;
    else if (batch_error != ERROR_FILE_NOT_FOUND) { error = (DWORD)batch_error; goto cleanup; }
    if (!batch && !CreateProcessA(NULL, line, NULL, NULL, TRUE, creation_flags, NULL, NULL, startup, &process)) {
        error = GetLastError();
        if (error != ERROR_FILE_NOT_FOUND) goto cleanup;
        batch_error = resolve_batch_script(command, script, MAX_PATH, 1);
        if (batch_error != 0) {
            if (batch_error != ERROR_FILE_NOT_FOUND) error = (DWORD)batch_error;
            goto cleanup;
        }
        batch = 1;
    }
    if (batch) {
        free(line);
        line = fpm_watch_batch_command(command, script);
        if (line == NULL) {
            error = errno == ENOMEM ? ERROR_NOT_ENOUGH_MEMORY : ERROR_INVALID_PARAMETER;
            goto cleanup;
        }
        if (strlen(line) > 8191) { error = ERROR_FILENAME_EXCED_RANGE; goto cleanup; }
        {
            UINT length = GetSystemDirectoryA(command_processor, MAX_PATH);
            if (length == 0) { error = GetLastError(); goto cleanup; }
            if (length >= MAX_PATH) { error = ERROR_FILENAME_EXCED_RANGE; goto cleanup; }
            strcat(command_processor, "\\cmd.exe");
        }
        if (!CreateProcessA(command_processor, line, NULL, NULL, TRUE, creation_flags, NULL, NULL, startup, &process)) {
            error = GetLastError();
            goto cleanup;
        }
        error = 0;
    }
    if (!AssignProcessToJobObject(job, process.hProcess)) {
        error = GetLastError();
        goto cleanup;
    }
    if (ResumeThread(process.hThread) == (DWORD)-1) {
        error = GetLastError();
        goto cleanup;
    }
    while ((waited = WaitForSingleObject(process.hProcess, 50)) == WAIT_TIMEOUT) {
        if (InterlockedCompareExchange(&interrupt_status, 0, 0) != 0) {
            TerminateJobObject(job, 130);
        }
    }
    if (waited == WAIT_FAILED || !GetExitCodeProcess(process.hProcess, &status)) {
        error = GetLastError();
    }

cleanup:
    *interrupted = handler_installed && InterlockedCompareExchange(&interrupt_status, 0, 0) != 0;
    if (!error && !*interrupted && process.hProcess != NULL) {
        limits.BasicLimitInformation.LimitFlags = 0;
        if (!SetInformationJobObject(job, JobObjectExtendedLimitInformation, &limits, sizeof(limits))) {
            error = GetLastError();
        }
    }
    *interrupted = *interrupted || (handler_installed && InterlockedCompareExchange(&interrupt_status, 0, 0) != 0);
    if (*interrupted && job != NULL) TerminateJobObject(job, 130);
    if (error && process.hProcess != NULL) {
        TerminateJobObject(job, 127);
        TerminateProcess(process.hProcess, 127);
    }
    if (*interrupted) status = 130;
    if (process.hThread != NULL) CloseHandle(process.hThread);
    if (process.hProcess != NULL) CloseHandle(process.hProcess);
    if (job != NULL) CloseHandle(job);
    if (handler_installed) SetConsoleCtrlHandler(watch_console_handler, FALSE);
    if (startup->hStdInput != NULL && startup->hStdInput != INVALID_HANDLE_VALUE) CloseHandle(startup->hStdInput);
    if (startup->hStdOutput != NULL && startup->hStdOutput != INVALID_HANDLE_VALUE) CloseHandle(startup->hStdOutput);
    if (startup->hStdError != NULL && startup->hStdError != INVALID_HANDLE_VALUE) CloseHandle(startup->hStdError);
    if (attributes_initialized) DeleteProcThreadAttributeList(startup_info.lpAttributeList);
    free(startup_info.lpAttributeList);
    free(line);
    return error ? launch_failure((int)error, launch_error, "CreateProcess/job setup") : (int)status;
}

#else
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#define watch_dup2 dup2
#define watch_close close

extern char **environ;

static volatile sig_atomic_t interrupt_signal = 0;
static volatile sig_atomic_t child_group = 0;
static volatile sig_atomic_t ancestor_group = 0;
static volatile sig_atomic_t monitor_parent = 0;
static const char group_variable[] = "FPM_WATCH_PROCESS_GROUP=";
static const int watched_signals[] = {SIGHUP, SIGINT, SIGQUIT, SIGTERM};
enum { WATCHED_SIGNAL_COUNT = sizeof(watched_signals) / sizeof(watched_signals[0]) };

/* SIGCHLD wakes pselect; waitpid owns both completion and job-control status. */
static void child_status_handler(int signal_number) {
    (void)signal_number;
}

/* Forward once: managed groups can include another watcher and its monitor. */
static void watch_signal_handler(int signal_number) {
    int saved_errno = errno;
    if (interrupt_signal == 0) {
        interrupt_signal = signal_number;
        if (child_group > 0) kill(-(pid_t)child_group, signal_number);
        if (ancestor_group > 0) kill(-(pid_t)ancestor_group, signal_number);
    }
    errno = saved_errno;
}

/* The foreground monitor lets terminal interrupts reach a waiting supervisor. */
static void monitor_signal_handler(int signal_number) {
    int saved_errno = errno;
    if (interrupt_signal == 0) {
        interrupt_signal = signal_number;
        kill((pid_t)monitor_parent, signal_number);
    }
    errno = saved_errno;
}

static int close_on_exec_pipe(int descriptors[2]) {
    descriptors[0] = descriptors[1] = -1;
    if (pipe(descriptors) != 0) return -1;
    if (fcntl(descriptors[0], F_SETFD, FD_CLOEXEC) < 0 ||
        fcntl(descriptors[1], F_SETFD, FD_CLOEXEC) < 0) {
        int error = errno;
        close(descriptors[0]);
        close(descriptors[1]);
        descriptors[0] = descriptors[1] = -1;
        errno = error;
        return -1;
    }
    return 0;
}

static int terminal_foreground(int terminal, pid_t group) {
    sigset_t blocked, previous;
    int result, error;
    sigemptyset(&blocked);
    sigaddset(&blocked, SIGTTOU);
    if (sigprocmask(SIG_BLOCK, &blocked, &previous) != 0) return -1;
    result = tcsetpgrp(terminal, group);
    error = errno;
    sigprocmask(SIG_SETMASK, &previous, NULL);
    errno = error;
    return result;
}

static double monotonic_seconds(void) {
    struct timespec now;
    if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return -1.0;
    return (double)now.tv_sec + (double)now.tv_nsec / 1000000000.0;
}

/* Both private pipes transfer this small record atomically (below PIPE_BUF). */
struct command_result { int status; int error; char operation[LAUNCH_OPERATION_CAPACITY]; };

static void write_command_result(int descriptor, int status, int error, const char *operation) {
    struct command_result result = {0};
    result.status = status;
    result.error = error;
    copy_launch_operation(result.operation, sizeof(result.operation), operation);
    while (write(descriptor, &result, sizeof(result)) < 0 && errno == EINTR) {}
}

static void finish_monitor(int descriptor, int status, int error, const char *operation) {
    write_command_result(descriptor, status, error, operation);
    _exit(0);
}

/* Build the inherited group marker without allocation after fork. */
static void format_group(char *marker, pid_t group) {
    char digits[32];
    size_t n = 0, i = sizeof(group_variable) - 1;
    unsigned long value = (unsigned long)group;
    memcpy(marker, group_variable, i);
    do { digits[n++] = (char)('0' + value % 10); value /= 10; } while (value != 0);
    while (n > 0) marker[i++] = digits[--n];
    marker[i] = '\0';
}

static void run_monitor(const char *command, int gate, int result_descriptor,
                        const sigset_t *mask, char **environment, char *marker, pid_t managed_group) {
    struct sigaction action;
    char ready;
    int exec_error[2], status = 0, error = 0, i;
    struct command_result execution = {0};
    const char *operation = "";
    pid_t command_pid;
    ssize_t released, bytes;

    child_group = ancestor_group = 0;
    interrupt_signal = 0;
    monitor_parent = (sig_atomic_t)getppid();
    if (managed_group == 0 && setpgid(0, 0) != 0)
        finish_monitor(result_descriptor, 0, errno, "setpgid(monitor)");
    action.sa_handler = monitor_signal_handler;
    action.sa_flags = 0;
    sigemptyset(&action.sa_mask);
    for (i = 0; i < WATCHED_SIGNAL_COUNT; ++i) {
        if (sigaction(watched_signals[i], &action, NULL) != 0)
            finish_monitor(result_descriptor, 0, errno, "sigaction(monitor)");
    }
    /* Parent first gives this group terminal ownership, then releases the gate. */
    do { released = read(gate, &ready, 1); } while (released < 0 && errno == EINTR);
    if (released != 1) finish_monitor(result_descriptor, 0, released < 0 ? errno : ECANCELED, "read(gate)");
    close(gate);
    if (sigprocmask(SIG_SETMASK, mask, NULL) != 0)
        finish_monitor(result_descriptor, 0, errno, "sigprocmask(monitor)");
    if (interrupt_signal != 0) finish_monitor(result_descriptor, 0, 0, "");
    if (close_on_exec_pipe(exec_error) != 0) finish_monitor(result_descriptor, 0, errno, "pipe(exec_error)");
    format_group(marker, getpgrp());
    command_pid = fork();
    if (command_pid < 0) finish_monitor(result_descriptor, 0, errno, "fork(command)");
    if (command_pid == 0) {
        char *arguments[] = {"sh", "-c", (char *)command, NULL};
        close(exec_error[0]);
        action.sa_handler = SIG_DFL;
        for (i = 0; i < WATCHED_SIGNAL_COUNT; ++i) {
            if (sigaction(watched_signals[i], &action, NULL) != 0) {
                write_command_result(exec_error[1], 0, errno, "sigaction(command)");
                _exit(127);
            }
        }
        execve("/bin/sh", arguments, environment);
        error = errno;
        write_command_result(exec_error[1], 0, error, "execve(/bin/sh)");
        _exit(127);
    }
    close(exec_error[1]);
    do { bytes = read(exec_error[0], &execution, sizeof(execution)); } while (bytes < 0 && errno == EINTR);
    if (bytes < 0) {
        error = errno;
        operation = "read(exec_error)";
    } else if (bytes == (ssize_t)sizeof(execution)) {
        error = execution.error;
        operation = execution.operation;
    } else if (bytes != 0) {
        error = EIO;
        operation = "read(exec_error)";
    }
    close(exec_error[0]);
    while (waitpid(command_pid, &status, 0) < 0) {
        if (errno != EINTR) {
            if (error == 0) { error = errno; operation = "waitpid(command)"; }
            break;
        }
    }
    finish_monitor(result_descriptor, status, error, operation);
}

int fpm_watch_execute(const char *command, int silent, int *interrupted, int *launch_error) {
    struct sigaction action, previous[WATCHED_SIGNAL_COUNT], previous_child;
    sigset_t blocked, previous_mask, wait_mask;
    struct command_result result;
    struct timespec pause = {0, 50000000};
    pid_t pid = -1, waited, original_group = getpgrp(), managed_group = 0;
    int gate[2] = {-1, -1}, results[2] = {-1, -1};
    int status = 0, error = 0, installed = 0, i, reaped = 0;
    int terminal = -1, terminal_given = 0, child_handler_installed = 0;
    double deadline = 0.0;
    size_t nenv = 0, used = 0;
    char **environment = NULL, marker[sizeof(group_variable) + 32];
    const char *inherited = getenv("FPM_WATCH_PROCESS_GROUP");
    const char *operation = "";

    (void)silent;
    *interrupted = 0;
    *launch_error = 0;
    last_launch_operation[0] = '\0';
    interrupt_signal = child_group = ancestor_group = 0;
    if (inherited != NULL) {
        char *end;
        long group = strtol(inherited, &end, 10);
        if (*end == '\0' && group > 0 && group == (long)original_group && kill((pid_t)group, 0) == 0) ancestor_group = (sig_atomic_t)group;
    }
    while (environ[nenv] != NULL) ++nenv;
    environment = malloc((nenv + 2) * sizeof(*environment));
    if (environment == NULL) return launch_failure(ENOMEM, launch_error, "allocate(environment)");
    for (size_t j = 0; j < nenv; ++j) {
        if (strncmp(environ[j], group_variable, sizeof(group_variable) - 1) != 0) environment[used++] = environ[j];
    }
    environment[used++] = marker;
    environment[used] = NULL;
    sigemptyset(&blocked);
    for (i = 0; i < WATCHED_SIGNAL_COUNT; ++i) sigaddset(&blocked, watched_signals[i]);
    sigaddset(&blocked, SIGCHLD);
    if (sigprocmask(SIG_BLOCK, &blocked, &previous_mask) != 0) {
        error = errno;
        free(environment);
        return launch_failure(error, launch_error, "sigprocmask(block)");
    }
    action.sa_handler = watch_signal_handler;
    action.sa_flags = 0;
    sigemptyset(&action.sa_mask);
    for (i = 0; i < WATCHED_SIGNAL_COUNT; ++i) {
        if (sigaction(watched_signals[i], &action, &previous[i]) != 0) {
            error = errno; operation = "sigaction(parent)"; goto restore;
        }
        ++installed;
    }
    action.sa_handler = child_status_handler;
    if (sigaction(SIGCHLD, &action, &previous_child) != 0) {
        error = errno; operation = "sigaction(SIGCHLD)"; goto restore;
    }
    child_handler_installed = 1;
    wait_mask = previous_mask;
    sigdelset(&wait_mask, SIGCHLD);
    if (close_on_exec_pipe(gate) != 0) { error = errno; operation = "pipe(gate)"; goto restore; }
    if (close_on_exec_pipe(results) != 0) { error = errno; operation = "pipe(results)"; goto restore; }
    terminal = open("/dev/tty", O_RDWR | O_CLOEXEC);
    pid = fork();
    if (pid < 0) { error = errno; operation = "fork(monitor)"; goto restore; }
    if (pid == 0) {
        close(gate[1]);
        close(results[0]);
        if (terminal >= 0) close(terminal);
        run_monitor(command, gate[0], results[1], &previous_mask, environment, marker,
                    ancestor_group > 0 ? original_group : 0);
    }
    close(gate[0]); gate[0] = -1;
    close(results[1]); results[1] = -1;
    managed_group = ancestor_group > 0 ? original_group : pid;
    child_group = (sig_atomic_t)managed_group;
    if (managed_group == pid && setpgid(pid, pid) != 0) {
        error = errno; operation = "setpgid(parent)"; goto cancel;
    }
    if (managed_group == pid && terminal >= 0 && tcgetpgrp(terminal) == original_group) {
        if (terminal_foreground(terminal, pid) != 0) {
            error = errno; operation = "terminal_foreground(start)"; goto cancel;
        }
        terminal_given = 1;
    }
    {
        struct sigaction ignored, old_pipe;
        ssize_t released;
        ignored.sa_handler = SIG_IGN;
        ignored.sa_flags = 0;
        sigemptyset(&ignored.sa_mask);
        if (sigaction(SIGPIPE, &ignored, &old_pipe) != 0) {
            error = errno; operation = "sigaction(SIGPIPE)"; goto cancel;
        }
        do { released = write(gate[1], "x", 1); } while (released < 0 && errno == EINTR);
        if (released != 1) { error = errno; operation = "write(gate)"; }
        if (sigaction(SIGPIPE, &old_pipe, NULL) != 0 && error == 0) {
            error = errno; operation = "sigaction(SIGPIPE restore)";
        }
        if (error) goto cancel;
    }
    close(gate[1]); gate[1] = -1;
    if (sigprocmask(SIG_SETMASK, &previous_mask, NULL) != 0) {
        error = errno; operation = "sigprocmask(wait)"; goto cancel;
    }
    for (;;) {
        /* Block before checking state: pselect atomically releases these signals
         * while waiting, so a fast child cannot finish between the check and sleep. */
        if (sigprocmask(SIG_BLOCK, &blocked, NULL) != 0) {
            error = errno; operation = "sigprocmask(wait block)"; goto cancel;
        }
        if (!reaped) {
            waited = waitpid(pid, &status, WNOHANG | WUNTRACED);
            if (waited == pid) {
                if (WIFSTOPPED(status)) {
                    if (terminal_given) terminal_foreground(terminal, original_group);
                    terminal_given = 0;
                    kill(getpid(), WSTOPSIG(status));
                    if (terminal >= 0 && tcgetpgrp(terminal) == original_group) {
                        if (terminal_foreground(terminal, managed_group) != 0) {
                            error = errno; operation = "terminal_foreground(resume)"; goto cancel;
                        }
                        terminal_given = 1;
                    }
                    kill(-managed_group, SIGCONT);
                    continue;
                }
                reaped = 1;
            } else if (waited < 0 && errno != EINTR) {
                error = errno; operation = "waitpid(monitor)"; goto cancel;
            }
        }
        if (interrupt_signal != 0) {
            /* A nested caller is itself in the managed group. Once its monitor
               finishes, unwind so it can clean its graph workspace and exit.
               The outer caller, outside this group, owns forced escalation. */
            if (ancestor_group > 0) {
                if (reaped) break;
            } else {
                if (deadline == 0.0) deadline = monotonic_seconds() + 1.0;
                if (kill(-managed_group, 0) != 0 && errno == ESRCH) break;
                if (monotonic_seconds() < 0.0 || monotonic_seconds() >= deadline) {
                    kill(-managed_group, SIGKILL);
                    break;
                }
            }
        } else if (reaped) break;
        {
            struct timespec interval = pause;
            const struct timespec *timeout = interrupt_signal != 0 && ancestor_group == 0 ? &interval : NULL;
            if (pselect(0, NULL, NULL, NULL, timeout, &wait_mask) < 0 && errno != EINTR) {
                error = errno;
                operation = "pselect";
                goto cancel;
            }
        }
    }
    if (!reaped) {
        while (waitpid(pid, &status, 0) < 0) {
            if (errno != EINTR) { error = errno; operation = "waitpid(monitor)"; break; }
        }
        reaped = 1;
    }
    if (!error) {
        ssize_t bytes;
        do { bytes = read(results[0], &result, sizeof(result)); } while (bytes < 0 && errno == EINTR);
        if (bytes == (ssize_t)sizeof(result)) {
            status = result.status;
            error = result.error;
            operation = result.operation;
        } else if (bytes != 0) {
            error = bytes < 0 ? errno : EIO;
            operation = "read(results)";
        }
    }
    goto restore;

cancel:
    kill(-managed_group, SIGKILL);
    kill(pid, SIGKILL);
    while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {}
restore:
    sigprocmask(SIG_BLOCK, &blocked, NULL);
    child_group = ancestor_group = 0;
    if (terminal_given) terminal_foreground(terminal, original_group);
    if (terminal >= 0) close(terminal);
    for (i = 0; i < 2; ++i) {
        if (gate[i] >= 0) close(gate[i]);
        if (results[i] >= 0) close(results[i]);
    }
    free(environment);
    *interrupted = interrupt_signal != 0;
    if (*interrupted) status = 128 + interrupt_signal;
    else if (!error && WIFEXITED(status)) status = WEXITSTATUS(status);
    else if (!error && WIFSIGNALED(status)) status = 128 + WTERMSIG(status);
    for (i = 0; i < installed; ++i) sigaction(watched_signals[i], &previous[i], NULL);
    if (child_handler_installed) sigaction(SIGCHLD, &previous_child, NULL);
    sigprocmask(SIG_SETMASK, &previous_mask, NULL);
    return error ? launch_failure(error, launch_error, operation) : status;
}
#endif

int fpm_watch_redirect_stdout(void) {
    int saved;
    fflush(stdout);
#ifdef _WIN32
    {
        HANDLE duplicate;
        if (!DuplicateHandle(GetCurrentProcess(), (HANDLE)_get_osfhandle(1), GetCurrentProcess(),
                             &duplicate, 0, FALSE, DUPLICATE_SAME_ACCESS)) return -(int)GetLastError();
        saved = _open_osfhandle((intptr_t)duplicate, _O_WRONLY | _O_NOINHERIT);
        if (saved < 0) {
            int error = errno;
            CloseHandle(duplicate);
            return -error;
        }
    }
#else
    saved = fcntl(1, F_DUPFD_CLOEXEC, 0);
#endif
    if (saved < 0) return -errno;
    if (watch_dup2(2, 1) < 0) {
        int error = errno;
        watch_close(saved);
        return -error;
    }
#ifdef _WIN32
    if (!SetStdHandle(STD_OUTPUT_HANDLE, (HANDLE)_get_osfhandle(1))) {
        int error = (int)GetLastError();
        watch_dup2(saved, 1);
        watch_close(saved);
        return -error;
    }
#endif
    return saved;
}

int fpm_watch_restore_stdout(int saved) {
    int error = 0;
    fflush(stdout);
    if (watch_dup2(saved, 1) < 0) error = errno;
    watch_close(saved);
#ifdef _WIN32
    if (!SetStdHandle(STD_OUTPUT_HANDLE, (HANDLE)_get_osfhandle(1)) && error == 0) error = (int)GetLastError();
#endif
    return error;
}
