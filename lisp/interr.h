/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/interr.h,v 1.1 1992/07/28 20:14:34 wlott Exp $
 */

#ifndef _INTERR_H_
#define _INTERR_H_

#define crap_out(msg) do { write(2, msg, sizeof(msg)); lose(); } while (0)

extern void lose(char *fmt, ...);
extern void set_lossage_handler(void fun(void));
extern void internal_error(struct sigcontext *context);

extern lispobj debug_print(lispobj string);

#endif
