#include <errno.h>
#include <stdlib.h>
#include <string.h>

/* Decode the CRT serialization used by watch_cmdline before encoding for cmd.
 * Native executables keep their original command line and do not use this path. */
static char **decode_arguments(const char *command, char **storage, size_t *count) {
    size_t n = strlen(command);
    char **arguments = malloc((n / 2 + 2) * sizeof(*arguments));
    char *out;
    const char *in = command;
    *storage = malloc(n + 1);
    *count = 0;
    if (arguments == NULL || *storage == NULL) {
        free(arguments);
        free(*storage);
        *storage = NULL;
        errno = ENOMEM;
        return NULL;
    }
    out = *storage;
    while (*in != '\0') {
        int quoted = 0;
        while (*in == ' ' || *in == '\t') ++in;
        if (*in == '\0') break;
        arguments[(*count)++] = out;
        while (*in != '\0' && (quoted || (*in != ' ' && *in != '\t'))) {
            size_t slashes = 0;
            while (*in == '\\') { ++slashes; ++in; }
            if (*in == '"') {
                size_t i;
                for (i = 0; i < slashes / 2; ++i) *out++ = '\\';
                if (slashes % 2 != 0) { *out++ = *in++; continue; }
                if (quoted && in[1] == '"') { *out++ = '"'; in += 2; }
                else { quoted = !quoted; ++in; }
            } else {
                while (slashes-- > 0) *out++ = '\\';
                if (*in != '\0' && (quoted || (*in != ' ' && *in != '\t'))) *out++ = *in++;
            }
        }
        if (quoted) {
            free(arguments);
            free(*storage);
            *storage = NULL;
            errno = EINVAL;
            return NULL;
        }
        *out++ = '\0';
    }
    arguments[*count] = NULL;
    return arguments;
}

char *fpm_watch_command_executable(const char *command) {
    char *storage = NULL, *executable = NULL;
    size_t count;
    char **arguments = decode_arguments(command, &storage, &count);
    if (arguments == NULL) return NULL;
    if (count != 0 && arguments[0][0] != '\0') {
        executable = malloc(strlen(arguments[0]) + 1);
        if (executable != NULL) strcpy(executable, arguments[0]);
        else errno = ENOMEM;
    } else errno = EINVAL;
    free(arguments);
    free(storage);
    return executable;
}

/* cmd and the CRT have different quote rules. Double embedded quotes for both
 * parsers; expand a zero-length CD substring between percent signs so cmd does
 * not expand the caller's %VARIABLE% text. Command extensions must be enabled.
 * This is the batch escaping strategy also used by Rust's standard library:
 * https://github.com/rust-lang/rust/blob/master/library/std/src/sys/args/windows.rs */
static char *append_batch_argument(char *out, const char *argument) {
    const char *in = argument;
    *out++ = '"';
    while (*in != '\0') {
        size_t slashes = 0;
        while (*in == '\\') { ++slashes; ++in; }
        if (*in == '\0' || *in == '"') slashes *= 2;
        while (slashes-- > 0) *out++ = '\\';
        if (*in == '\0') break;
        if (*in == '"') *out++ = '"';
        if (*in == '%') {
            memcpy(out, "%%cd:~,%", 8);
            out += 8;
        } else *out++ = *in;
        ++in;
    }
    *out++ = '"';
    return out;
}

/* Returned storage belongs to the caller. Reject line breaks rather than
 * allowing cmd to truncate or execute an additional command. */
char *fpm_watch_batch_command(const char *command, const char *script) {
    static const char prefix[] = "cmd.exe /d /e:on /v:off /s /c \"";
    char *storage = NULL, *line, *out;
    char **arguments;
    size_t count, n = strlen(command) + strlen(script), i;
    if (strchr(command, '\r') != NULL || strchr(command, '\n') != NULL ||
        strchr(script, '\r') != NULL || strchr(script, '\n') != NULL || strchr(script, '"') != NULL) {
        errno = EINVAL; return NULL;
    }
    arguments = decode_arguments(command, &storage, &count);
    if (arguments == NULL) return NULL;
    if (count == 0 || arguments[0][0] == '\0' || strchr(arguments[0], '"') != NULL) {
        free(arguments);
        free(storage);
        errno = EINVAL;
        return NULL;
    }
    line = malloc(n * 8 + sizeof(prefix) + 4);
    if (line != NULL) {
        memcpy(line, prefix, sizeof(prefix) - 1);
        out = line + sizeof(prefix) - 1;
        for (i = 0; i < count; ++i) {
            if (i != 0) *out++ = ' ';
            out = append_batch_argument(out, i == 0 ? script : arguments[i]);
        }
        *out++ = '"';
        *out = '\0';
    }
    free(arguments);
    free(storage);
    if (line == NULL) errno = ENOMEM;
    return line;
}
