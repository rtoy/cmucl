/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/vars.h,v 1.1 1990/02/24 19:37:33 wlott Exp $ */


extern void flush_vars();
extern struct var *lookup_by_name(/* name */);
extern struct var *lookup_by_obj(/* obj */);
extern struct var *define_var(/* name, obj, perm */);
extern struct var *define_dynamic_var(/* name, update_fn, perm */);

extern char *var_name();
extern lispobj var_value();
extern long var_clock();
extern void var_setclock();
