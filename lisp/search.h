/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/search.h,v 1.1.20.1 2005/12/19 01:10:15 rtoy Exp $
 */

#ifndef _SEARCH_H_
#define _SEARCH_H_

extern boolean search_for_type(int type, lispobj ** start, int *count);
extern boolean search_for_symbol(char *name, lispobj ** start, int *count);

#endif
