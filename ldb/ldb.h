/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/ldb.h,v 1.2 1991/05/24 17:54:14 wlott Exp $ */
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
#define SymbolFunction(sym) (((struct symbol *)PTR(sym))->function)
#define SetSymbolFunction(sym,val) (((struct symbol *)PTR(sym))->function = (val))

#define crap_out(msg) do { write(2, msg, sizeof(msg)); exit(-1); } while (0)

#endif _LDB_H_
