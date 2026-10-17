#ifndef _WIN32
#if defined(__APPLE__) && !defined(_DARWIN_C_SOURCE)
#define _DARWIN_C_SOURCE
#endif
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 700
#endif
#endif

#include <errno.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>

static volatile LONG workspace_counter = 0;

int fpm_watch_create_temp_directory(char *path, size_t capacity) {
    char directory[MAX_PATH + 1], absolute[MAX_PATH + 1], created[MAX_PATH + 1];
    DWORD length, error;
    LARGE_INTEGER clock;
    int attempt, written;
    if (capacity == 0) return ERROR_INSUFFICIENT_BUFFER;
    path[0] = '\0';
    length = GetEnvironmentVariableA("FPM_WATCH_WORKSPACE_ROOT", directory, (DWORD)sizeof(directory));
    if (length == 0) {
        length = GetTempPathA((DWORD)sizeof(directory), directory);
        if (length == 0) return (int)GetLastError();
    }
    if (length >= sizeof(directory)) return ERROR_FILENAME_EXCED_RANGE;
    length = GetFullPathNameA(directory, (DWORD)sizeof(absolute), absolute, NULL);
    if (length == 0) return (int)GetLastError();
    if (length >= sizeof(absolute)) return ERROR_FILENAME_EXCED_RANGE;
    for (attempt = 0; attempt < 64; ++attempt) {
        if (!QueryPerformanceCounter(&clock)) return ERROR_GEN_FAILURE;
        written = snprintf(created, sizeof(created), "%s%sfpm-watch-%08lx-%08lx-%016llx", absolute,
                           length > 0 && (absolute[length - 1] == '\\' || absolute[length - 1] == '/') ? "" : "\\",
                           (unsigned long)GetCurrentProcessId(), (unsigned long)InterlockedIncrement(&workspace_counter),
                           (unsigned long long)clock.QuadPart);
        if (written < 0 || (size_t)written >= sizeof(created)) return ERROR_FILENAME_EXCED_RANGE;
        if ((size_t)written >= capacity) return ERROR_INSUFFICIENT_BUFFER;
        /* Exclusive creation never accepts an existing file, directory, or link. */
        if (CreateDirectoryA(created, NULL)) {
            memcpy(path, created, (size_t)written + 1);
            return 0;
        }
        error = GetLastError();
        if (error != ERROR_ALREADY_EXISTS && error != ERROR_FILE_EXISTS) return (int)error;
    }
    return ERROR_ALREADY_EXISTS;
}

static char *child_path(const char *parent, const char *name) {
    size_t length = strlen(parent), extra = strlen(name);
    char *path;
    if (length > 32765 || extra > 32765 - length) { SetLastError(ERROR_FILENAME_EXCED_RANGE); return NULL; }
    path = malloc(length + extra + 2);
    if (path == NULL) { SetLastError(ERROR_NOT_ENOUGH_MEMORY); return NULL; }
    memcpy(path, parent, length);
    path[length] = '\\';
    memcpy(path + length + 1, name, extra + 1);
    return path;
}

int fpm_watch_remove_temp_directory(const char *path) {
    HANDLE directory, search;
    BY_HANDLE_FILE_INFORMATION info;
    WIN32_FIND_DATAA entry;
    char *pattern, *child;
    DWORD error, first_error = 0;
    if (path[0] == '\0') return 0;
    /* Open the entry itself and deny modification/rename while traversing it.
     * Junctions and all other reparse points are removed without enumeration. */
    directory = CreateFileA(path, FILE_READ_ATTRIBUTES, FILE_SHARE_READ, NULL, OPEN_EXISTING,
                            FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
    if (directory == INVALID_HANDLE_VALUE) {
        error = GetLastError();
        return error == ERROR_FILE_NOT_FOUND || error == ERROR_PATH_NOT_FOUND ? 0 : (int)error;
    }
    if (!GetFileInformationByHandle(directory, &info)) {
        error = GetLastError();
        CloseHandle(directory);
        return (int)error;
    }
    if ((info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0 &&
        (info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) == 0) {
        pattern = child_path(path, "*");
        if (pattern == NULL) first_error = GetLastError();
        else {
            search = FindFirstFileA(pattern, &entry);
            free(pattern);
            if (search == INVALID_HANDLE_VALUE) {
                error = GetLastError();
                if (error != ERROR_FILE_NOT_FOUND) first_error = error;
            } else {
                do {
                    if (strcmp(entry.cFileName, ".") == 0 || strcmp(entry.cFileName, "..") == 0) continue;
                    child = child_path(path, entry.cFileName);
                    if (child == NULL) error = GetLastError();
                    else { error = (DWORD)fpm_watch_remove_temp_directory(child); free(child); }
                    if (error != 0 && first_error == 0) first_error = error;
                } while (FindNextFileA(search, &entry));
                error = GetLastError();
                if (error != ERROR_NO_MORE_FILES && first_error == 0) first_error = error;
                FindClose(search);
            }
        }
    }
    CloseHandle(directory);
    if ((info.dwFileAttributes & FILE_ATTRIBUTE_READONLY) &&
        !(info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT)) {
        if (!SetFileAttributesA(path, info.dwFileAttributes & ~FILE_ATTRIBUTE_READONLY) && first_error == 0)
            first_error = GetLastError();
    }
    if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
        if (!RemoveDirectoryA(path) && first_error == 0) first_error = GetLastError();
    } else if (!DeleteFileA(path) && first_error == 0) first_error = GetLastError();
    return (int)first_error;
}

int fpm_watch_set_temp_directory(const char *path) {
    DWORD attributes = GetFileAttributesA(path);
    int crt_error;
    if (attributes == INVALID_FILE_ATTRIBUTES) return (int)GetLastError();
    if (!(attributes & FILE_ATTRIBUTE_DIRECTORY) || (attributes & FILE_ATTRIBUTE_REPARSE_POINT)) return ERROR_DIRECTORY;
    /* fpm's tempnam reads the CRT environment; child processes inherit Win32's.
     * Keep both views synchronized instead of updating only the OS block. */
    crt_error = _putenv_s("TMPDIR", path);
    if (crt_error == 0) crt_error = _putenv_s("TMP", path);
    if (crt_error == 0) crt_error = _putenv_s("TEMP", path);
    if (crt_error != 0) return crt_error == ENOMEM ? ERROR_NOT_ENOUGH_MEMORY : ERROR_INVALID_PARAMETER;
    if (!SetEnvironmentVariableA("TMPDIR", path) || !SetEnvironmentVariableA("TMP", path) ||
        !SetEnvironmentVariableA("TEMP", path)) return (int)GetLastError();
    return 0;
}

int fpm_watch_set_workspace_root(const char *path, int defined) {
    int error = _putenv_s("FPM_WATCH_WORKSPACE_ROOT", defined ? path : "");
    if (error != 0) return error == ENOMEM ? ERROR_NOT_ENOUGH_MEMORY : ERROR_INVALID_PARAMETER;
    if (!SetEnvironmentVariableA("FPM_WATCH_WORKSPACE_ROOT", defined ? path : NULL))
        return (int)GetLastError();
    return 0;
}

#else
#include <dirent.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

int fpm_watch_create_temp_directory(char *path, size_t capacity) {
    static const char suffix[] = "/fpm-watch-XXXXXX";
    const char *configured = getenv("FPM_WATCH_WORKSPACE_ROOT");
    char *directory, *created;
    size_t length;
    int error = 0;
    if (capacity == 0) return ENAMETOOLONG;
    path[0] = '\0';
    if (configured == NULL || configured[0] == '\0') configured = getenv("TMPDIR");
    if (configured == NULL || configured[0] == '\0') configured = "/tmp";
    directory = realpath(configured, NULL);
    if (directory == NULL) return errno;
    length = strlen(directory);
    if (length > 0 && directory[length - 1] == '/') --length;
    if (length >= capacity || sizeof(suffix) > capacity - length) {
        free(directory);
        return ENAMETOOLONG;
    }
    created = malloc(length + sizeof(suffix));
    if (created == NULL) { free(directory); return ENOMEM; }
    memcpy(created, directory, length);
    memcpy(created + length, suffix, sizeof(suffix));
    free(directory);
    if (mkdtemp(created) == NULL) error = errno;
    else if (chmod(created, S_IRWXU) != 0) { error = errno; rmdir(created); }
    else memcpy(path, created, length + sizeof(suffix));
    free(created);
    return error;
}

int fpm_watch_set_workspace_root(const char *path, int defined) {
    int result = defined ? setenv("FPM_WATCH_WORKSPACE_ROOT", path, 1) :
                          unsetenv("FPM_WATCH_WORKSPACE_ROOT");
    return result == 0 ? 0 : errno;
}

static int remove_entry(int parent, const char *name) {
    struct stat attributes;
    struct dirent *entry;
    DIR *directory;
    int descriptor, error, first_error = 0;
    if (fstatat(parent, name, &attributes, AT_SYMLINK_NOFOLLOW) != 0) return errno == ENOENT ? 0 : errno;
    if (!S_ISDIR(attributes.st_mode)) {
        if (unlinkat(parent, name, 0) == 0 || errno == ENOENT) return 0;
        return errno;
    }
    descriptor = openat(parent, name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (descriptor < 0) {
        error = errno;
        if (error == ENOENT) return 0;
        /* The directory may have been replaced by a link after fstatat. */
        if (error == ELOOP || error == ENOTDIR) {
            if (unlinkat(parent, name, 0) == 0 || errno == ENOENT) return 0;
            return errno;
        }
        return error;
    }
    directory = fdopendir(descriptor);
    if (directory == NULL) { error = errno; close(descriptor); return error; }
    for (;;) {
        errno = 0;
        entry = readdir(directory);
        if (entry == NULL) {
            if (errno != 0 && first_error == 0) first_error = errno;
            break;
        }
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) continue;
        error = remove_entry(descriptor, entry->d_name);
        if (error != 0 && first_error == 0) first_error = error;
    }
    if (closedir(directory) != 0 && first_error == 0) first_error = errno;
    if (unlinkat(parent, name, AT_REMOVEDIR) != 0 && errno != ENOENT && first_error == 0) first_error = errno;
    return first_error;
}

int fpm_watch_remove_temp_directory(const char *path) {
    if (path[0] == '\0') return 0;
    return remove_entry(AT_FDCWD, path);
}

int fpm_watch_set_temp_directory(const char *path) {
    struct stat attributes;
    if (path[0] != '/') return EINVAL;
    if (lstat(path, &attributes) != 0) return errno;
    if (!S_ISDIR(attributes.st_mode)) return ENOTDIR;
    if (setenv("TMPDIR", path, 1) != 0 || setenv("TMP", path, 1) != 0 || setenv("TEMP", path, 1) != 0) return errno;
    return 0;
}
#endif
