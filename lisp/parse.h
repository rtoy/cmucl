/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/parse.h,v 1.1 1992/07/28 20:15:16 wlott Exp $ */

/* All parse routines take a char ** as their only argument */

extern boolean more_p(char **ptr);
extern char *parse_token(char **ptr);
extern lispobj parse_lispobj(char **ptr);
extern char *parse_addr(char **ptr);
extern long parse_number(char **ptr);
