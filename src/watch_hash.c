#ifndef _WIN32
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#ifndef _FILE_OFFSET_BITS
#define _FILE_OFFSET_BITS 64
#endif
#endif

#include <stdint.h>
#include <string.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

/* Stream every byte using the same FNV fingerprint as the Fortran interface.
 * Unsigned multiplication wraps modulo 2**64 without signed-overflow tricks.
 * Each call owns its handle and bounded buffer; no shared scratch state. */
int64_t fpm_watch_file_fingerprint(const char *path) {
    unsigned char buffer[65536];
    uint64_t hash = UINT64_C(1469598103934665603), remaining, original_size;
    int64_t result = 2;
    size_t count, i;
#ifdef _WIN32
    LARGE_INTEGER size, final_size;
    HANDLE file = CreateFileA(path, GENERIC_READ,
                             FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                             NULL, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL);
    if (file == INVALID_HANDLE_VALUE) {
        DWORD error = GetLastError();
        return error == ERROR_FILE_NOT_FOUND || error == ERROR_PATH_NOT_FOUND || error == ERROR_DIRECTORY ? 0 : 2;
    }
    if (!GetFileSizeEx(file, &size) || size.QuadPart < 0) goto cleanup;
    original_size = (uint64_t)size.QuadPart;
#else
    struct stat size, final_size;
    int file = open(path, O_RDONLY | O_CLOEXEC);
    if (file < 0) return errno == ENOENT || errno == ENOTDIR ? 0 : 2;
    if (fstat(file, &size) != 0 || size.st_size < 0) goto cleanup;
    original_size = (uint64_t)size.st_size;
#endif
    remaining = original_size;
    while (remaining > 0) {
        size_t requested = remaining < sizeof(buffer) ? (size_t)remaining : sizeof(buffer);
#ifdef _WIN32
        DWORD received;
        if (!ReadFile(file, buffer, (DWORD)requested, &received, NULL) || received == 0) goto cleanup;
        count = (size_t)received;
#else
        ssize_t received;
        do { received = read(file, buffer, requested); } while (received < 0 && errno == EINTR);
        if (received <= 0) goto cleanup;
        count = (size_t)received;
#endif
        for (i = 0; i < count; ++i) {
            hash ^= buffer[i];
            hash *= UINT64_C(1099511628211);
        }
        remaining -= count;
    }
#ifdef _WIN32
    if (!GetFileSizeEx(file, &final_size) || final_size.QuadPart != size.QuadPart) goto cleanup;
#else
    if (fstat(file, &final_size) != 0 || final_size.st_size != size.st_size) goto cleanup;
#endif
    if (original_size == 0) result = 1;
    else {
        hash |= UINT64_C(1) << 63;
        memcpy(&result, &hash, sizeof(result));
    }
cleanup:
#ifdef _WIN32
    CloseHandle(file);
#else
    close(file);
#endif
    return result;
}
