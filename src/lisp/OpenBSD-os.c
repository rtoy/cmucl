/*
 * OpenBSD-os.c.
 * From FreeBSD-os.c 1.6 2000/10/24 13:32:30 dtc Exp
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is the OSF1 version.  By Sean Hallgren.
 * Much hacked by Paul Werkowski
 * GENCGC support by Douglas Crosher, 1996, 1997.
 * Frobbed for OpenBSD by Pierre R. Mai, 2001.
 *
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include <errno.h>
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "lispregs.h"
#include "internals.h"

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/proc.h>
#include "validate.h"
vm_size_t os_vm_page_size;

#if defined GENCGC
#include "gencgc.h"
#endif


void
os_init0(const char *argv[], const char *envp[])
{}

void
os_init(const char *argv[], const char *envp[])
{
    os_vm_page_size = OS_VM_DEFAULT_PAGESIZE;
}

int *
sc_reg(struct sigcontext *c, int offset)
{
    switch (offset) {
      case 0:
	  return &c->sc_eax;
      case 2:
	  return &c->sc_ecx;
      case 4:
	  return &c->sc_edx;
      case 6:
	  return &c->sc_ebx;
      case 8:
	  return &c->sc_esp;
      case 10:
	  return &c->sc_ebp;
      case 12:
	  return &c->sc_esi;
      case 14:
	  return &c->sc_edi;
    }
    return (int *) 0;
}

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_ANON;

    if (addr)
	flags |= MAP_FIXED;

    DPRINTF(0, (stderr, "os_validate %x %d => ", addr, len));

    addr = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);

    if (addr == (os_vm_address_t) - 1) {
	perror("mmap");
	return NULL;
    }

    DPRINTF(0, (stderr, "%x\n", addr));

    return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    DPRINTF(0, (stderr, "os_invalidate %x %d\n", addr, len));

    if (munmap(addr, len) == -1)
	perror("munmap");
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    addr = mmap(addr, len,
		OS_VM_PROT_ALL,
		MAP_PRIVATE | MAP_FILE | MAP_FIXED, fd, (off_t) offset);

    if (addr == (os_vm_address_t) - 1)
	perror("mmap");

    return addr;
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect(address, length, prot) == -1)
	perror("mprotect");
}



static boolean
in_range_p(os_vm_address_t a, lispobj sbeg, size_t slen)
{
    char *beg = (char *) sbeg;
    char *end = (char *) sbeg + slen;
    char *adr = (char *) a;

    return (adr >= beg && adr < end);
}

boolean
valid_addr(os_vm_address_t addr)
{
    if (in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE)
	|| in_range_p(addr, STATIC_SPACE_START, STATIC_SPACE_SIZE)
	|| in_range_p(addr, DYNAMIC_0_SPACE_START, dynamic_space_size)
	|| in_range_p(addr, DYNAMIC_1_SPACE_START, dynamic_space_size)
	|| in_range_p(addr, CONTROL_STACK_START, CONTROL_STACK_SIZE)
	|| in_range_p(addr, BINDING_STACK_START, BINDING_STACK_SIZE))
	return TRUE;
    return FALSE;
}


static void
sigsegv_handler(HANDLER_ARGS)
{
#if defined GENCGC
    caddr_t fault_addr = code->si_addr;

#if SIGSEGV_VERBOSE
    fprintf(stderr, "Signal %d, fault_addr=%x, page_index=%d:\n",
	    signal, fault_addr, page_index);
#endif

    if (gc_write_barrier(code->si_addr))
	return;
#endif

    SAVE_CONTEXT();

    DPRINTF(0, (stderr, "sigsegv:\n"));
    interrupt_handle_now(signal, code, context);
}

static void
sigbus_handler(HANDLER_ARGS)
{
    SAVE_CONTEXT();

    DPRINTF(0, (stderr, "sigbus:\n"));
    interrupt_handle_now(signal, code, context);
}

void
os_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(SIGSEGV, sigsegv_handler);
    interrupt_install_low_level_handler(SIGBUS, sigbus_handler);
}

/*
 * Return a new string containing the path to an OS-dependent location
 * where temporary files/directories can be stored.  If NULL is
 * returned, such a location could not be found or some other error
 * happened.
 *
 * Caller must call free() on the string returned.
 */
char *
os_temporary_directory(void)
{
    /*
     * If the TMP envvar is set, use that as the temporary directory.
     * Otherwise, just assume "/tmp" will work.
     */
    char *tmp_path = getenv("TMP");

    if (tmp_path == NULL) {
	tmp_path = "/tmp";
    }

    return strdup(tmp_path);
}
