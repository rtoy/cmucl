/*
 * Linux-os.c. 
 * From FreeBSD-os.c
 * From osf1-os.c,v 1.1 94/03/27 15:30:51 hallgren Exp $
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is the OSF1 version.  By Sean Hallgren.
 * Much hacked by Paul Werkowski
 * Morfed from the FreeBSD file by Peter Van Eynde (July 1996)
 * GENCGC support by Douglas Crosher, 1996, 1997.
 * Alpha support by Julian Dolby, 1999.
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
#include <sys/socket.h>
#include <sys/utsname.h>

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <link.h>
#include <dlfcn.h>
#include <assert.h>

#include "validate.h"
size_t os_vm_page_size;

#if defined GENCGC
#include "gencgc.h"
#endif


void
os_init(const char *argv[], const char *envp[])
{
    struct utsname name;

    uname(&name);

    /* We need this for mmap */

    if (name.release[0] < '2') {
	printf("Linux version must be later then 2.0.0!\n");
	exit(2);
    }

    os_vm_page_size = getpagesize();
}

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;

    if (addr)
      flags |= MAP_FIXED;

    addr = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);

    if (addr == (os_vm_address_t) - 1) {
	perror("mmap");
	return NULL;
    }

    return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    DPRINTF(0, (stderr, "os_invalidate %p %d\n", addr, len));

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
    if (in_range_p(addr, READ_ONLY_SPACE_START, read_only_space_size)
	|| in_range_p(addr, STATIC_SPACE_START, static_space_size)
	|| in_range_p(addr, DYNAMIC_0_SPACE_START, dynamic_space_size)
	|| in_range_p(addr, DYNAMIC_1_SPACE_START, dynamic_space_size)
	|| in_range_p(addr, CONTROL_STACK_START, control_stack_size)
	|| in_range_p(addr, BINDING_STACK_START, binding_stack_size))
	return TRUE;
    return FALSE;
}



/* Some symbols, most notably stat and lstat, don't appear at all in
   the glibc .so files as a result of preprocessor and linker magic /
   braindamage.  So, try falling back to a stub in linux-stubs.S that
   will call the proper function if it's one of those. */

static void *
dlsym_fallback(void *handle, const char *name)
{
    char newsym[1024];
    void *sym_addr;

    strcpy(newsym, "PVE_stub_");
    strcat(newsym, name);
    sym_addr = dlsym(handle, newsym);
#ifdef DEBUG
    if (sym_addr == 0) {
	fputs(dlerror(), stderr);
    }
#endif
    return sym_addr;
}

void *
os_dlsym(const char *sym_name, lispobj lib_list)
{
    static void *program_handle;
    void *sym_addr = 0;

    if (!program_handle)
	program_handle = dlopen((void *) 0, RTLD_LAZY | RTLD_GLOBAL);
    if (lib_list != NIL) {
	lispobj lib_list_head;

	for (lib_list_head = lib_list;
	     lib_list_head != NIL; lib_list_head = (CONS(lib_list_head))->cdr) {
	    struct cons *lib_cons = CONS(CONS(lib_list_head)->car);
	    struct sap *dlhandle = (struct sap *) PTR(lib_cons->car);

	    sym_addr = dlsym((void *) dlhandle->pointer, sym_name);
	    if (sym_addr)
		return sym_addr;
	}
    }
    sym_addr = dlsym(program_handle, sym_name);
    if (!sym_addr && dlerror()) {
	return dlsym_fallback(program_handle, sym_name);
    } else {
	return sym_addr;
    }
}
