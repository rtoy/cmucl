/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/time.c,v 1.1 1993/11/12 19:21:23 wlott Exp $ */

/* Time support routines that are easier to do in C then in lisp. */

#include <stdio.h>
#include <time.h>
#include "lisp.h"

void get_timezone(time_t when, int *minwest, boolean *dst)
{
    struct tm ltm, gtm;

    ltm = *localtime(&when);
    gtm = *gmtime(&when);

    *minwest = ((gtm.tm_hour*60)+gtm.tm_sec) - ((ltm.tm_hour*60)+ltm.tm_sec);
    *dst = ltm.tm_isdst;
}
