#ifdef __cplusplus
extern "C" {
#endif

void fpm_watch_sleep_seconds(double s);

#ifdef _WIN32
  #define WIN32_LEAN_AND_MEAN
  #include <windows.h>
  #include <stdint.h>

  void fpm_watch_sleep_seconds(double s) {
    if (s <= 0.0) return;

    double ms_d = s * 1000.0;
    uint64_t ms64 = (uint64_t)(ms_d + 0.5);

    if (ms64 > 0xFFFFFFFFULL) ms64 = 0xFFFFFFFFULL;
    Sleep((DWORD)ms64);
  }

#else
  #include <time.h>
  #include <errno.h>

  void fpm_watch_sleep_seconds(double s) {
    if (s <= 0.0) return;

    struct timespec req, rem;

    time_t sec = (time_t)s;
    double frac = s - (double)sec;
    if (frac < 0.0) frac = 0.0;

    long nsec = (long)(frac * 1000000000.0);

    if (nsec < 0) nsec = 0;
    if (nsec >= 1000000000L) {
      sec += 1;
      nsec -= 1000000000L;
    }

    req.tv_sec = sec;
    req.tv_nsec = nsec;

    while (nanosleep(&req, &rem) != 0) {
      if (errno == EINTR) {
        req = rem;
        continue;
      }
      break;
    }
  }
#endif

#ifdef __cplusplus
}
#endif