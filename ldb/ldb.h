/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/ldb.h,v 1.1 1990/02/24 19:37:19 wlott Exp $ */
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

#endif _LDB_H_
