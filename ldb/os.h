/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/os.h,v 1.2 1991/05/24 17:57:40 wlott Exp $
 *
 * Common interface for os-dependent functions.
 *
 */

#if !defined(_OS_H_INCLUDED_)

#define _OS_H_INCLUDED_

#ifdef MACH
#include "mach-os.h"
#else
#ifdef sun
#include "sunos-os.h"
#endif
#endif

#define OS_VM_PROT_ALL (OS_VM_PROT_READ|OS_VM_PROT_WRITE|OS_VM_PROT_EXECUTE)

extern os_vm_size_t os_vm_page_size;

extern void os_install_interrupt_handlers();

extern os_vm_address_t os_allocate(), os_reallocate();
void os_deallocate();

extern os_vm_address_t os_validate();
extern void os_invalidate();
extern void os_zero();
extern os_vm_address_t os_map();
extern void os_flush_icache();
extern void os_protect();
extern boolean valid_addr();

#define os_trunc_to_page(addr) \
    (os_vm_address_t)((long)addr&~(os_vm_page_size-1))
#define os_round_up_to_page(addr) \
    os_trunc_to_page(addr+(os_vm_page_size-1))

#define os_trunc_size_to_page(size) \
    (os_vm_size_t)((long)size&~(os_vm_page_size-1))
#define os_round_up_size_to_page(size) \
    os_trunc_size_to_page(size+(os_vm_page_size-1))

#endif
