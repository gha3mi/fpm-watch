#ifndef _WIN32
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#endif

#include <math.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <time.h>
#include <errno.h>
#endif

#ifdef __cplusplus
extern "C"
#endif
void fpm_watch_sleep_seconds(double s) {
  if (!isfinite(s) || s <= 0.0) return;

  while (s > 0.0) {
    /* Bounded intervals avoid integer overflow and Windows' INFINITE value. */
    double interval = s > 3600.0 ? 3600.0 : s;
#ifdef _WIN32
    double ms_d = interval * 1000.0;
    DWORD ms = (DWORD)ms_d;
    if ((double)ms < ms_d) ++ms;
    Sleep(ms);
#else
    struct timespec req, rem;
    req.tv_sec = (time_t)interval;
    req.tv_nsec = (long)((interval - (double)req.tv_sec) * 1000000000.0);
    while (nanosleep(&req, &rem) != 0) {
      if (errno != EINTR) return;
      req = rem;
    }
#endif
    s -= interval;
  }
}
