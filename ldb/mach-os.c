/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/mach-os.c,v 1.4 1992/03/02 04:10:36 wlott Exp $
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is the Mach version.
 *
 */

#include <stdio.h>
#include <mach.h>
#include "./signal.h"
#include "ldb.h"
#include "os.h"

#define MAX_SEGS 32

static struct segment {
    vm_address_t start;
    vm_size_t length;
} addr_map[MAX_SEGS];
static int segments = -1;

vm_size_t os_vm_page_size;

void os_init()
{
	os_vm_page_size = vm_page_size;
}

os_vm_address_t os_validate(addr, len)
vm_address_t addr;
vm_size_t len;
{
    kern_return_t res;

#if defined(EXT_PAGER)
    res = pager_vm_allocate(task_self(), &addr, len, addr==NULL);
#else
    res = vm_allocate(task_self(), &addr, len, addr==NULL);
#endif

    if (res != KERN_SUCCESS)
        mach_error("Could not vm_allocate memory: ", res);

    segments = -1;

    vm_protect(task_self(), addr, len, TRUE,
	       VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE);

    return addr;
}

void os_invalidate(addr, len)
vm_address_t addr;
vm_size_t len;
{
    kern_return_t res;

#if defined(EXT_PAGER)
    res = pager_vm_deallocate(task_self(), addr, len);
#else
    res = vm_deallocate(task_self(), addr, len);
#endif

    if (res != KERN_SUCCESS)
        mach_error("Could not vm_allocate memory: ", res);

    segments = -1;
}

vm_address_t os_map(fd, offset, addr, len)
int fd, offset;
vm_address_t addr;
vm_size_t len;
{
    kern_return_t res;

#if defined(EXT_PAGER)
    res = pager_map_fd(fd, offset, &addr, 0, len);
#else
    res = map_fd(fd, offset, &addr, 0, len);
#endif

    if (res != KERN_SUCCESS)
        mach_error("Could not map_fd memory: ", res);

    segments = -1;

    vm_protect(task_self(), addr, len, TRUE,
	       VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE);

    return addr;
}

void os_flush_icache(address, length)
vm_address_t address;
vm_size_t length;
{
#ifdef mips
	vm_machine_attribute_val_t flush;
	kern_return_t kr;

	flush = MATTR_VAL_ICACHE_FLUSH;

	kr = vm_machine_attribute(task_self(), address, length,
				  MATTR_CACHE, &flush);
	if (kr != KERN_SUCCESS)
		mach_error("Could not flush the instruction cache", kr);
#endif
}

void os_protect(address, length, protection)
vm_address_t address;
vm_size_t length;
vm_prot_t protection;
{
	vm_protect(task_self(), address, length, FALSE, protection);
}


boolean valid_addr(test)
vm_address_t test;
{
    vm_address_t addr;
    vm_size_t size;
    long bullshit;
    int curseg;

    if (segments == -1) {
        addr = 0;
        curseg = 0;

        while (1) {
            if (vm_region(task_self(), &addr, &size, &bullshit, &bullshit, &bullshit, &bullshit, &bullshit, &bullshit) != KERN_SUCCESS)
                break;

            if (curseg > 0 && addr_map[curseg-1].start + addr_map[curseg-1].length == addr)
                addr_map[curseg-1].length += size;
            else {
                addr_map[curseg].start = addr;
                addr_map[curseg].length = size;
                curseg++;
            }

            addr += size;
        }

        segments = curseg;
    }
    
    for (curseg = 0; curseg < segments; curseg++)
        if (addr_map[curseg].start <= test && test < addr_map[curseg].start + addr_map[curseg].length)
            return TRUE;
    return FALSE;
}

#ifndef ibmrt
static void sigbus_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    if(!interrupt_maybe_gc(context))
	interrupt_handle_now(signal, code, context);
}
#endif

void os_install_interrupt_handlers()
{
#ifndef ibmrt
    interrupt_install_low_level_handler(SIGBUS,sigbus_handler);
#endif
#ifdef mips
    interrupt_install_low_level_handler(SIGSEGV,sigbus_handler);
#endif
}
