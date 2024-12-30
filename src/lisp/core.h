/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef CORE_H
#define CORE_H

#include "lisp.h"

#define CORE_PAGESIZE OS_VM_DEFAULT_PAGESIZE
#define CORE_MAGIC (('C' << 24) | ('O' << 16) | ('R' << 8) | 'E')
#define CORE_END 3840
#define CORE_NDIRECTORY 3861
#define CORE_VALIDATE 3845
#define CORE_VERSION 3860
#define CORE_MACHINE_STATE 3862
#define CORE_INITIAL_FUNCTION 3863

#define DYNAMIC_SPACE_ID (1)
#define STATIC_SPACE_ID (2)
#define READ_ONLY_SPACE_ID (3)

struct ndir_entry {
#if !(defined(alpha) || defined(__x86_64))
    long identifier;
    long nwords;
    long data_page;
    long address;
    long page_count;
#else
    u32 identifier;
    u32 nwords;
    u32 data_page;
    u32 address;
    u32 page_count;
#endif
};

extern lispobj load_core_file(const char *file, fpu_mode_t *fpu_type);

#endif /* CORE_H */
