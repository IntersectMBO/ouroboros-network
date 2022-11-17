#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
/* 
 * (c) The University of Glasgow 2002
 *
 * Time Runtime Support
 */
#include "HsTime.h"

#if HAVE_GETTIMEOFDAY
int __hscore_gettimeofday(struct timeval *tp, void *tzp)
{
    return gettimeofday(tp, tzp);
}
#endif

#if HAVE_GMTIME_R
struct tm *__hscore_gmtime_r(const time_t *clock, struct tm *result)
{
    return gmtime_r(clock, result);
}
#endif

#if HAVE_LOCALTIME_R
struct tm *__hscore_localtime_r(const time_t *clock, struct tm *result)
{
    return localtime_r(clock, result);
}
#endif

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32) /* to the end */

long *__hscore_timezone( void )
{ return &_timezone; }

char **__hscore_tzname( void )
{ return _tzname; }
#endif
#endif
