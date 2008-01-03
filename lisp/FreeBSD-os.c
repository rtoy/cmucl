/*
 * FreeBSD-os.c. Maybe could be just BSD-os.c
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
 * GENCGC support by Douglas Crosher, 1996, 1997.
 *
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/FreeBSD-os.c,v 1.22 2008/01/03 11:41:54 cshapiro Exp $
 *
 */

#include "os.h"
#include <sys/file.h>
#include <machine/npx.h>
#include <errno.h>
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "lispregs.h"
#include "internals.h"

#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/proc.h>
#include <dlfcn.h>
#include "validate.h"

#if defined GENCGC
#include "gencgc.h"
#endif

vm_size_t os_vm_page_size;


void
os_init(void)
{
    os_vm_page_size = getpagesize();
}

unsigned long *
os_sigcontext_reg(ucontext_t *scp, int index)
{
    switch (index) {
    case 0:
	return (unsigned long *) &scp->uc_mcontext.mc_eax;
    case 2:
	return (unsigned long *) &scp->uc_mcontext.mc_ecx;
    case 4:
	return (unsigned long *) &scp->uc_mcontext.mc_edx;
    case 6:
	return (unsigned long *) &scp->uc_mcontext.mc_ebx;
    case 8:
	return (unsigned long *) &scp->uc_mcontext.mc_esp;
    case 10:
	return (unsigned long *) &scp->uc_mcontext.mc_ebp;
    case 12:
	return (unsigned long *) &scp->uc_mcontext.mc_esi;
    case 14:
	return (unsigned long *) &scp->uc_mcontext.mc_edi;
    }
    return NULL;
}

unsigned long *
os_sigcontext_pc(ucontext_t *scp)
{
    return (unsigned long *) &scp->uc_mcontext.mc_eip;
}

unsigned char *
os_sigcontext_fpu_reg(ucontext_t *scp, int index)
{
    union savefpu *sv = (union savefpu *) scp->uc_mcontext.mc_fpstate;
    int fpformat = scp->uc_mcontext.mc_fpformat;
    unsigned char *reg = NULL;

    switch (fpformat) {
    case _MC_FPFMT_XMM:
	 reg = sv->sv_xmm.sv_fp[index].fp_acc.fp_bytes;
	 break;
    case _MC_FPFMT_387:
	 reg = sv->sv_87.sv_ac[index].fp_bytes;
	 break;
    case _MC_FPFMT_NODEV:
	 reg = NULL;
	 break;
    }
    return reg;
}

unsigned long
os_sigcontext_fpu_modes(ucontext_t *scp)
{
    union savefpu *sv = (union savefpu *) scp->uc_mcontext.mc_fpstate;
    unsigned long modes;
    int fpformat = scp->uc_mcontext.mc_fpformat;
    unsigned short cw, sw;

    if (fpformat == _MC_FPFMT_XMM) {
	cw = sv->sv_xmm.sv_env.en_cw;
	sw = sv->sv_xmm.sv_env.en_sw;
    } else if (fpformat == _MC_FPFMT_387) {
	cw = sv->sv_87.sv_env.en_cw & 0xffff;
	sw = sv->sv_87.sv_env.en_sw & 0xffff;
    } else {  /* _MC_FPFMT_NODEV */
	cw = 0;
	sw = 0x3f;
    }
    modes = (sw & 0xff) << 16 | cw;
    modes ^= 0x3f;
    return modes;
}

os_vm_address_t os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_ANON;

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
    if (munmap(addr, len) == -1)
	perror("munmap");
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    addr = mmap(addr, len, OS_VM_PROT_ALL,
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

    return adr >= beg && adr < end;
}

boolean valid_addr(os_vm_address_t addr)
{
    os_vm_address_t newaddr;

    newaddr = os_trunc_to_page(addr);

    if (in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE)
	|| in_range_p(addr, STATIC_SPACE_START, STATIC_SPACE_SIZE)
	|| in_range_p(addr, DYNAMIC_0_SPACE_START, dynamic_space_size)
#ifndef GENCGC
	|| in_range_p(addr, DYNAMIC_1_SPACE_START, dynamic_space_size)
#endif
	|| in_range_p(addr, CONTROL_STACK_START, CONTROL_STACK_SIZE)
	|| in_range_p(addr, BINDING_STACK_START, BINDING_STACK_SIZE))
	return TRUE;
    return FALSE;
}


static void
sigbus_handler(int signal, siginfo_t *info, ucontext_t *context)
{
    int page_index;

#ifdef RED_ZONE_HIT
    if (os_control_stack_overflow(info->si_addr, context))
	return;
#endif

#if defined GENCGC
    if (info->si_code == PROTECTION_VIOLATION_CODE) {
	page_index = find_page_index(info->si_addr);

	/* Check if the fault is within the dynamic space. */
	if (page_index != -1) {
	    /* Un-protect the page */

	    /* The page should have been marked write protected */
	    if (!PAGE_WRITE_PROTECTED(page_index))
		fprintf(stderr,
			"*** Sigbus in page not marked as write protected\n");

	    os_protect(page_address(page_index), 4096, OS_VM_PROT_ALL);
	    page_table[page_index].flags &= ~PAGE_WRITE_PROTECTED_MASK;
	    page_table[page_index].flags |= PAGE_WRITE_PROTECT_CLEARED_MASK;

	    return;
	}
    }
#endif /* GENCGC */

    interrupt_handle_now(signal, info, context);
}

/*
 * Restore the exception flags cleared by the kernel.  These bits must
 * be set for Lisp to determine which exception caused the signal.  At
 * present, there is no way to distinguish underflow exceptions from
 * denormalized operand exceptions.  An underflow exception is assumed
 * if the subcode is FPE_FLTUND.
 */
static void
sigfpe_handler(int signal, siginfo_t *info, ucontext_t *context)
{
     union savefpu *sv = (union savefpu *) context->uc_mcontext.mc_fpstate;
     int fpformat = context->uc_mcontext.mc_fpformat;
     int code = info->si_code;
     unsigned char trap = 0;

     switch (code) {
     case FPE_FLTDIV:  /* ZE */
	  trap = 0x04;
	  break;
     case FPE_FLTOVF:  /* OE */
	  trap = 0x08;
	  break;
     case FPE_FLTUND:  /* DE or UE */
	  trap = 0x10;
	  break;
     case FPE_FLTRES:  /* PE */
	  trap = 0x20;
	  break;
     case FPE_FLTINV:  /* IE */
	  trap = 0x01;
	  break;
     }
     switch (fpformat) {
     case _MC_FPFMT_XMM:
	  sv->sv_xmm.sv_env.en_sw |= trap;
	  break;
     case _MC_FPFMT_387:
	  sv->sv_87.sv_env.en_sw |= trap;
	  break;
     }
     interrupt_handle_now(signal, info, context);
}

void
os_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler
	(PROTECTION_VIOLATION_SIGNAL, sigbus_handler);
    interrupt_install_low_level_handler(SIGFPE, sigfpe_handler);
}

void *
os_dlsym(const char *sym_name, lispobj lib_list)
{
    if (lib_list != NIL) {
	lispobj lib_list_head;

	for (lib_list_head = lib_list;
	     lib_list_head != NIL; lib_list_head = CONS(lib_list_head)->cdr) {
	    struct cons *lib_cons = CONS(CONS(lib_list_head)->car);
	    struct sap *dlhandle = (struct sap *) PTR(lib_cons->car);
	    void *sym_addr = dlsym((void *) dlhandle->pointer, sym_name);

	    if (sym_addr)
		return sym_addr;
	}
    }

    return dlsym(RTLD_DEFAULT, sym_name);
}

void
restore_fpu(ucontext_t *scp)
{
    union savefpu *sv = (union savefpu *) scp->uc_mcontext.mc_fpstate;
    int fpformat = scp->uc_mcontext.mc_fpformat;
    unsigned short cw;

    if (fpformat == _MC_FPFMT_XMM) {
	cw = sv->sv_xmm.sv_env.en_cw;
    } else if (fpformat == _MC_FPFMT_387) {
	cw = sv->sv_87.sv_env.en_cw & 0xffff;
    } else {  /* _MC_FPFMT_NODEV */
	return;
    }
    __asm__ __volatile__ ("fldcw %0" : : "m" (*&cw));
}
