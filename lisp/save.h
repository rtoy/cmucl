/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/save.h,v 1.1 1992/07/28 20:15:30 wlott Exp $
 */

#ifndef _SAVE_H_
#define _SAVE_H_

#include "core.h"

extern void restore(void);
extern boolean save(char *filename);
extern void load(int fd, struct machine_state *ms);

#endif
