AC_INIT([Haskell old-time package], [1.0], [libraries@haskell.org], [old-time])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsTime.h])

AC_CONFIG_HEADERS([include/HsTimeConfig.h])

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

# check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([sys/time.h sys/timeb.h time.h])

AC_CHECK_FUNCS([ftime gmtime_r localtime_r gettimeofday])

dnl ** how do we get a timezone name, and UTC offset ?
AC_STRUCT_TIMEZONE

dnl ** do we have altzone?
FP_DECL_ALTZONE

AC_OUTPUT
