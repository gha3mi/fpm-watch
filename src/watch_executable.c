/* Resolve the running image independently of argv[0], PATH, and the working directory. */
#ifndef _WIN32
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#ifdef __APPLE__
#define _DARWIN_C_SOURCE
#endif
#endif
#include <errno.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <unistd.h>
#ifdef __APPLE__
#include <mach-o/dyld.h>
#include <stdint.h>
#endif
#endif

int fpm_watch_current_executable(char *path, size_t capacity) {
    if (capacity < 2) return ERANGE;
#ifdef _WIN32
    DWORD length = GetModuleFileNameA(NULL, path, (DWORD)capacity);
    if (length == 0) return (int)GetLastError();
    if (length >= capacity) return ERROR_INSUFFICIENT_BUFFER;
#elif defined(__APPLE__)
    uint32_t size = (uint32_t)capacity;
    char *resolved;
    if (_NSGetExecutablePath(path, &size) != 0) return ERANGE;
    resolved = realpath(path, NULL);
    if (resolved == NULL) return errno;
    if (strlen(resolved) >= capacity) {
        free(resolved);
        return ERANGE;
    }
    strcpy(path, resolved);
    free(resolved);
#else
    ssize_t length = readlink("/proc/self/exe", path, capacity - 1);
    if (length < 0) return errno;
    if ((size_t)length >= capacity - 1) return ERANGE;
    path[length] = '\0';
#endif
    return 0;
}
