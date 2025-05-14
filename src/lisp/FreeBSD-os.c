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
#include <dlfcn.h>
#include "validate.h"
#include <stdio.h>
#include <unistd.h>

#if defined GENCGC
#include "gencgc.h"
#endif

vm_size_t os_vm_page_size;

void
os_init0(const char *argv[], const char *envp[])
{}

void
os_init(const char *argv[], const char *envp[])
{
    os_vm_page_size = getpagesize();
}

unsigned long *
os_sigcontext_reg(ucontext_t *scp, int index)
{
    __register_t *rv;

    switch (index) {
      case 0:
        rv = &scp->uc_mcontext.mc_eax;
        break;
      case 2:
        rv = &scp->uc_mcontext.mc_ecx;
        break;
      case 4:
        rv = &scp->uc_mcontext.mc_edx;
        break;
      case 6:
        rv = &scp->uc_mcontext.mc_ebx;
        break;
      case 8:
        rv = &scp->uc_mcontext.mc_esp;
        break;
      case 10:
        rv = &scp->uc_mcontext.mc_ebp;
        break;
      case 12:
        rv = &scp->uc_mcontext.mc_esi;
        break;
      case 14:
        rv = &scp->uc_mcontext.mc_edi;
        break;
      default:
        rv = NULL;
    }
    
    /* Pre-cast to (void *), to avoid the compiler warning:
     * dereferencing type-punned pointer will break strict-aliasing rules
     */
    return (unsigned long *) (void *) rv;
}

unsigned long *
os_sigcontext_pc(ucontext_t *scp)
{
    return (unsigned long *) (void *) &scp->uc_mcontext.mc_eip;
}

unsigned char *
os_sigcontext_fpu_reg(ucontext_t *scp, int index)
{
    union savefpu *sv = (union savefpu *) scp->uc_mcontext.mc_fpstate;
    int fpformat = scp->uc_mcontext.mc_fpformat;
    unsigned char *reg = NULL;

    switch (fpformat) {
      case _MC_FPFMT_XMM:
          if (index < 8) {
              reg = sv->sv_xmm.sv_fp[index].fp_acc.fp_bytes;
          } else {
              reg = sv->sv_xmm.sv_xmm[index - 8].xmm_bytes;
          }
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

unsigned int
os_sigcontext_fpu_modes(ucontext_t *scp)
{
    unsigned int modes;

    union savefpu *sv = (union savefpu *) scp->uc_mcontext.mc_fpstate;
    int fpformat = scp->uc_mcontext.mc_fpformat;
    struct env87 *env_87 = &sv->sv_87.sv_env;
    struct envxmm *env_xmm = &sv->sv_xmm.sv_env;
    u_int16_t cw;
    u_int16_t sw;

    if (fpformat == _MC_FPFMT_XMM) {
	cw = env_xmm->en_cw;
	sw = env_xmm->en_sw;
    } else if (fpformat == _MC_FPFMT_387) {
	cw = env_87->en_cw & 0xffff;
	sw = env_87->en_sw & 0xffff;
    } else { /* _MC_FPFMT_NODEV */
	cw = 0;
	sw = 0x3f;
    }

    modes = ((cw & 0x3f) << 7) | (sw & 0x3f);

#ifdef FEATURE_SSE2
    if (fpu_mode == SSE2) {
	u_int32_t mxcsr = env_xmm->en_mxcsr;

	DPRINTF(0, (stderr, "SSE2 modes = %08x\n", (int)mxcsr));
	modes |= mxcsr;
    }
#endif
    modes ^= (0x3f << 7);
    return modes;
}

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
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

boolean
valid_addr(os_vm_address_t addr)
{
    if (in_range_p(addr, READ_ONLY_SPACE_START, read_only_space_size)
	|| in_range_p(addr, STATIC_SPACE_START, static_space_size)
	|| in_range_p(addr, DYNAMIC_0_SPACE_START, dynamic_space_size)
#ifndef GENCGC
	|| in_range_p(addr, DYNAMIC_1_SPACE_START, dynamic_space_size)
#endif
	|| in_range_p(addr, control_stack, control_stack_size)
	|| in_range_p(addr, binding_stack, binding_stack_size))
	return TRUE;
    return FALSE;
}


static void
protection_violation_handler(HANDLER_ARGS)
{
#ifdef RED_ZONE_HIT
    if (os_control_stack_overflow(code->si_addr, context))
	return;
#endif

#if defined GENCGC
    if (code->si_code == PROTECTION_VIOLATION_CODE) {
	if (gc_write_barrier(code->si_addr)) {
	    return;
	}
    }
#endif

    interrupt_handle_now(signal, code, context);
}

/*
 * Restore the exception flags cleared by the kernel.  These bits must
 * be set for Lisp to determine which exception caused the signal.  At
 * present, there is no way to distinguish underflow exceptions from
 * denormalized operand exceptions.  An underflow exception is assumed
 * if the subcode is FPE_FLTUND.
 */
static void
sigfpe_handler(HANDLER_ARGS)
{
    ucontext_t *ucontext = (ucontext_t *) context;
    union savefpu *sv = (union savefpu *) ucontext->uc_mcontext.mc_fpstate;
    int fpformat = ucontext->uc_mcontext.mc_fpformat;
    unsigned char trap = 0;

    switch (code->si_code) {
      case FPE_FLTDIV:		/* ZE */
	  trap = 0x04;
	  break;
      case FPE_FLTOVF:		/* OE */
	  trap = 0x08;
	  break;
      case FPE_FLTUND:		/* DE or UE */
	  trap = 0x10;
	  break;
      case FPE_FLTRES:		/* PE */
	  trap = 0x20;
	  break;
      case FPE_FLTINV:		/* IE */
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
    interrupt_handle_now(signal, code, context);
}

void
os_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(PROTECTION_VIOLATION_SIGNAL,
					protection_violation_handler);
    interrupt_install_low_level_handler(SIGFPE, sigfpe_handler);
}

void *
os_dlsym(const char *sym_name, lispobj lib_list)
{
    static void *program_handle;

    if (!program_handle)
	program_handle = dlopen((void *) 0, RTLD_LAZY | RTLD_GLOBAL);

    if (lib_list != NIL) {
	lispobj lib_list_head;

	for (lib_list_head = lib_list;
	     lib_list_head != NIL; lib_list_head = CONS(lib_list_head)->cdr) {
	    struct cons *lib_cons = CONS(CONS(lib_list_head)->car);
	    struct sap *dlhandle = (struct sap *)PTR(lib_cons->car);
	    void *sym_addr = dlsym((void *)dlhandle->pointer, sym_name);

	    if (sym_addr)
		return sym_addr;
	}
    }
    return dlsym(program_handle, sym_name);
}

void
restore_fpu(ucontext_t *scp)
{
    union savefpu *sv = (union savefpu *) scp->uc_mcontext.mc_fpstate;
    int fpformat = scp->uc_mcontext.mc_fpformat;
    struct env87 *env_87 = &sv->sv_87.sv_env;
    struct envxmm *env_xmm = &sv->sv_xmm.sv_env;
    u_int16_t cw;

    if (fpformat == _MC_FPFMT_XMM) {
	cw = env_xmm->en_cw;
    } else if (fpformat == _MC_FPFMT_387) {
	cw = env_87->en_cw & 0xffff;
    } else { /* _MC_FPFMT_NODEV */
	return;
    }
    DPRINTF(0, (stderr, "restore_fpu:  cw = %08x\n", (int)cw));
    __asm__ __volatile__ ("fldcw %0"::"m"(*&cw));

#ifdef FEATURE_SSE2
    if (fpu_mode == SSE2) {
	u_int32_t mxcsr = env_xmm->en_mxcsr;

	DPRINTF(0, (stderr, "restore_fpu:  mxcsr (raw) = %04x\n", mxcsr));
	__asm__ __volatile__ ("ldmxcsr %0"::"m"(*&mxcsr));
    }
#endif
}

#ifdef i386
boolean
os_support_sse2()
{
    return TRUE;
}
#endif
