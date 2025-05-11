/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef PARSE_H
#define PARSE_H


/* All parse routines take a char ** as their only argument */

extern boolean more_p(char **ptr);
extern char *parse_token(char **ptr);
extern lispobj parse_lispobj(char **ptr);
extern char *parse_addr(char **ptr);
extern long parse_number(char **ptr);

#endif /* PARSE_H */
