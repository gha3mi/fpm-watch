#ifndef _WIN32
#define _POSIX_C_SOURCE 200809L
#endif
#include <stdio.h>

static int launch_failure(int error, int *launch_error) {
    *launch_error = error;
    return 127;
}

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <errno.h>
#define watch_dup _dup
#define watch_dup2 _dup2
#define watch_close _close

/* Preserve watcher options and forwarded patterns during MinGW startup. */
#if defined(__MINGW64_VERSION_MAJOR)
int _dowildcard = 0;
#elif defined(__MINGW32__)
int _CRT_glob = 0;
#endif

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

int fpm_watch_execute(const char *command, int silent, int *interrupted, int *launch_error) {
    const char *prefix = "cmd.exe /d /v:off /s /c \"";
    char *line = (char *)malloc(strlen(command) + 1);
    STARTUPINFOA startup;
    PROCESS_INFORMATION process;
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
    HANDLE job = NULL;
    DWORD status = 127, error = 0, waited;
    DWORD creation_flags = CREATE_SUSPENDED;
    int handler_installed = 0;

    *interrupted = 0;
    *launch_error = 0;
    if (line == NULL) return launch_failure(ERROR_NOT_ENOUGH_MEMORY, launch_error);
    strcpy(line, command);
    ZeroMemory(&startup, sizeof(startup));
    ZeroMemory(&process, sizeof(process));
    ZeroMemory(&limits, sizeof(limits));
    startup.cb = sizeof(startup);
    startup.dwFlags = STARTF_USESTDHANDLES;
    startup.hStdInput = inherited_standard_handle(STD_INPUT_HANDLE, GENERIC_READ, 0);
    if (startup.hStdInput == INVALID_HANDLE_VALUE) {
        error = GetLastError();
        goto cleanup;
    }
    startup.hStdOutput = inherited_standard_handle(silent == 2 ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE,
                                                  GENERIC_WRITE, silent == 1);
    if (startup.hStdOutput == INVALID_HANDLE_VALUE) {
        error = GetLastError();
        goto cleanup;
    }
    startup.hStdError = inherited_standard_handle(STD_ERROR_HANDLE, GENERIC_WRITE, silent == 1);
    if (startup.hStdError == INVALID_HANDLE_VALUE) {
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
    if (!CreateProcessA(NULL, line, NULL, NULL, TRUE, creation_flags, NULL, NULL, &startup, &process)) {
        error = GetLastError();
        if (error != ERROR_BAD_EXE_FORMAT && error != ERROR_FILE_NOT_FOUND) goto cleanup;
        free(line);
        line = (char *)malloc(strlen(prefix) + strlen(command) + 2);
        if (line == NULL) {
            error = ERROR_NOT_ENOUGH_MEMORY;
            goto cleanup;
        }
        strcpy(line, prefix);
        strcat(line, command);
        strcat(line, "\"");
        if (!CreateProcessA(NULL, line, NULL, NULL, TRUE, creation_flags, NULL, NULL, &startup, &process)) {
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
    if (startup.hStdInput != NULL && startup.hStdInput != INVALID_HANDLE_VALUE) CloseHandle(startup.hStdInput);
    if (startup.hStdOutput != NULL && startup.hStdOutput != INVALID_HANDLE_VALUE) CloseHandle(startup.hStdOutput);
    if (startup.hStdError != NULL && startup.hStdError != INVALID_HANDLE_VALUE) CloseHandle(startup.hStdError);
    free(line);
    return error ? launch_failure((int)error, launch_error) : (int)status;
}

#else
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#define watch_dup dup
#define watch_dup2 dup2
#define watch_close close

static volatile sig_atomic_t interrupt_signal = 0;
static volatile sig_atomic_t child_pid = 0;

static void watch_signal_handler(int signal_number) {
    int saved_errno = errno;
    interrupt_signal = signal_number;
    if (child_pid > 0) kill((pid_t)child_pid, signal_number);
    errno = saved_errno;
}

int fpm_watch_execute(const char *command, int silent, int *interrupted, int *launch_error) {
    const int signals[] = {SIGINT, SIGQUIT, SIGTERM};
    struct sigaction action, previous[3], default_action;
    sigset_t blocked, previous_mask;
    pid_t pid;
    int status = 0, error = 0, installed = 0, i;

    *interrupted = 0;
    *launch_error = 0;
    (void)silent;
    interrupt_signal = 0;
    child_pid = 0;
    sigemptyset(&blocked);
    for (i = 0; i < 3; ++i) sigaddset(&blocked, signals[i]);
    if (sigprocmask(SIG_BLOCK, &blocked, &previous_mask) != 0) return launch_failure(errno, launch_error);
    action.sa_handler = watch_signal_handler;
    action.sa_flags = 0;
    sigemptyset(&action.sa_mask);
    for (i = 0; i < 3; ++i) {
        if (sigaction(signals[i], &action, &previous[i]) != 0) {
            error = errno;
            goto restore;
        }
        ++installed;
    }
    pid = fork();
    if (pid < 0) {
        error = errno;
        goto restore;
    }
    if (pid == 0) {
        default_action.sa_handler = SIG_DFL;
        default_action.sa_flags = 0;
        sigemptyset(&default_action.sa_mask);
        for (i = 0; i < 3; ++i) sigaction(signals[i], &default_action, NULL);
        sigprocmask(SIG_SETMASK, &previous_mask, NULL);
        execl("/bin/sh", "sh", "-c", command, (char *)NULL);
        _exit(127);
    }
    child_pid = (sig_atomic_t)pid;
    sigprocmask(SIG_SETMASK, &previous_mask, NULL);
    while (waitpid(pid, &status, 0) < 0) {
        if (errno == EINTR) continue;
        error = errno;
        break;
    }
    sigprocmask(SIG_BLOCK, &blocked, NULL);
    child_pid = 0;
    *interrupted = interrupt_signal != 0;
    if (*interrupted) status = 128 + interrupt_signal;
    else if (!error && WIFEXITED(status)) status = WEXITSTATUS(status);
    else if (!error && WIFSIGNALED(status)) status = 128 + WTERMSIG(status);

restore:
    for (i = 0; i < installed; ++i) sigaction(signals[i], &previous[i], NULL);
    sigprocmask(SIG_SETMASK, &previous_mask, NULL);
    return error ? launch_failure(error, launch_error) : status;
}
#endif

int fpm_watch_redirect_stdout(void) {
    int saved;
    fflush(stdout);
    saved = watch_dup(1);
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
