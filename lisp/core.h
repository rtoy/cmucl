/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/core.h,v 1.1 1992/07/28 20:14:19 wlott Exp $ */

#ifndef _CORE_H_
#define _CORE_H_

#include "lisp.h"
#include "lispregs.h"

#define CORE_PAGESIZE OS_VM_DEFAULT_PAGESIZE
#define CORE_MAGIC (('C' << 24) | ('O' << 16) | ('R' << 8) | 'E')
#define CORE_END 3840
#define CORE_NDIRECTORY 3861
#define CORE_VALIDATE 3845
#define CORE_VERSION 3860
#define CORE_MACHINE_STATE 3862

#define DYNAMIC_SPACE_ID (1)
#define STATIC_SPACE_ID (2)
#define READ_ONLY_SPACE_ID (3)

struct ndir_entry {
	long identifier;
	long nwords;
	long data_page;
	long address;
	long page_count;
};

struct machine_state {
    lispobj *csp;
    lispobj *cfp;
    long control_stack_page;

#ifdef reg_BSP
    lispobj *bsp;
#endif
    long binding_stack_page;

    char *nsp;
    long number_stack_page;
};

extern boolean load_core_file(char *file);

#endif
