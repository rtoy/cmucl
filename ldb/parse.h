/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/parse.h,v 1.1 1990/02/24 19:37:27 wlott Exp $ */

/* All parse routines take a char ** as their only argument */

boolean more_p();
char *parse_token();
lispobj parse_lispobj();
char *parse_addr();
long parse_number();
