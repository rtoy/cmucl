/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/os.h,v 1.1 1990/06/03 22:37:50 ch Exp $
 *
 * OS-dependent header file.
 *
 * This is the Mach version.
 *
 */

#if !defined(_OS_H_INCLUDED_)

#include <mach.h>
#include "ldb.h"

typedef vm_address_t os_vm_address_t;
typedef vm_size_t os_vm_size_t;
typedef vm_offset_t os_vm_offset_t;
typedef vm_prot_t os_vm_prot_t;

extern os_vm_size_t os_vm_page_size;

extern void os_validate();
extern void os_invalidate();
extern void os_zero();
extern void os_map();
extern void os_flush_icache();
extern void os_protect();
extern boolean valid_addr();

#define OS_VM_PROT_READ VM_PROT_READ
#define OS_VM_PROT_WRITE VM_PROT_WRITE
#define OS_VM_PROT_EXECUTE VM_PROT_EXECUTE

#endif
