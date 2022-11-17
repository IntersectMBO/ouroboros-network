/*
 * (c) The University of Glasgow 2002
 *
 * Time Runtime Support
 */
#ifndef __TIMEUTILS_H__
#define __TIMEUTILS_H__

#include "HsTimeConfig.h"
// Otherwise these clash with similar definitions from other packages:
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#if HAVE_GETTIMEOFDAY
#  if HAVE_SYS_TIME_H
#   include <sys/time.h>
#  endif
#elif HAVE_GETCLOCK
# if HAVE_SYS_TIMERS_H
#  define POSIX_4D9 1
#  include <sys/timers.h>
# endif
#endif
#if HAVE_TIME_H
#include <time.h>
#endif
#if HAVE_SYS_TIMEB_H && !defined(__FreeBSD__)
#include <sys/timeb.h>
#endif

extern long *__hscore_timezone( void );
extern char **__hscore_tzname( void );

#if HAVE_GETTIMEOFDAY
extern int __hscore_gettimeofday(struct timeval *tp, void *tzp);
#endif

#if HAVE_GMTIME_R
extern struct tm *__hscore_gmtime_r(const time_t *clock, struct tm *result);
#endif

#if HAVE_LOCALTIME_R
extern struct tm *__hscore_localtime_r(const time_t *clock, struct tm *result);
#endif

#endif /* __TIMEUTILS_H__ */
