/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/ldb.h,v 1.3 1992/03/08 18:43:14 wlott Exp $ */
#ifndef _LDB_H_
#define _LDB_H_

#ifndef NULL
#define NULL 0
#endif

#define boolean int
#define TRUE 1
#define FALSE 0

#define SymbolValue(sym) (((struct symbol *)PTR(sym))->value)
#define SetSymbolValue(sym,val) (((struct symbol *)PTR(sym))->value = (val))

#define funcall(fdefn,argptr,nargs) \
    call_into_lisp((fdefn),(fdefn)->function,argptr,nargs)
#define funcall_sym(sym,argptr,nargs) \
    funcall((struct fdefn *)(SymbolValue(sym)),argptr,nargs)

#define crap_out(msg) do { write(2, msg, sizeof(msg)); exit(-1); } while (0)

#endif _LDB_H_
