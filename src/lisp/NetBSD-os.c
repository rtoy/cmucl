/*
 * NetBSD-os.c.
 * From OpenBSD-os.c 1.1 2001/12/06 19:15:44 pmai Exp
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
 * Frobbed for NetBSD by Pierre R. Mai, 2002.
 *
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/proc.h>
#include <sys/sysctl.h>
#include <errno.h>

#include <x86/fpu.h>

#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "lispregs.h"
#include "internals.h"

#include <signal.h>
#include <dlfcn.h>
#include "validate.h"
size_t os_vm_page_size;

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

unsigned long *
os_sigcontext_reg(ucontext_t *scp, int index)
{
#ifdef i386
    switch (index) {
    case 0:
	return (unsigned long *) &scp->uc_mcontext.__gregs[_REG_EAX];
    case 2:
	return (unsigned long *) &scp->uc_mcontext.__gregs[_REG_ECX];
    case 4:
	return (unsigned long *) &scp->uc_mcontext.__gregs[_REG_EDX];
    case 6:
	return (unsigned long *) &scp->uc_mcontext.__gregs[_REG_EBX];
    case 8:
	return (unsigned long *) &scp->uc_mcontext.__gregs[_REG_ESP];
    case 10:
	return (unsigned long *) &scp->uc_mcontext.__gregs[_REG_EBP];
    case 12:
	return (unsigned long *) &scp->uc_mcontext.__gregs[_REG_ESI];
    case 14:
	return (unsigned long *) &scp->uc_mcontext.__gregs[_REG_EDI];
    }
#endif
    return NULL;
}

unsigned long *
os_sigcontext_pc(ucontext_t *scp)
{
#ifdef i386
    return (unsigned long *) &scp->uc_mcontext.__gregs[_REG_EIP];
#endif
}

unsigned char *
os_sigcontext_fpu_reg(ucontext_t *scp, int index)
{
    unsigned char *reg = NULL;

    DPRINTF(0, (stderr, "fpu reg index = %d\n", index));

    if (scp->uc_flags & _UC_FPU) {
	if (scp->uc_flags & _UC_FXSAVE) {
            /*
             * fp_xmm is an array of bytes in the format of the FXSAVE
             * instruction.  The x87 registers are at offset 32 from
             * the start and each entry takes 16 bytes (only 10
             * needed).  The XMM registers are at offset 160 from the
             * start of the array, and each XMM register is 16 bytes
             * long.
             */
            if (index >= 8) {
                reg = &scp->uc_mcontext.__fpregs.__fp_reg_set.__fp_xmm_state.__fp_xmm[160 + 16*(index - 8)];
                DPRINTF(0, (stderr, " sse2 = %g\n", (double) *(double*) reg));
            } else {
                reg = &scp->uc_mcontext.__fpregs.__fp_reg_set.__fp_xmm_state.__fp_xmm[32 + 16*index];
                DPRINTF(0, (stderr, " sse2 x87 = %g\n", (double) *(long double*) reg));
            }
            
	} else {
            /*
             * In this case, we have the FNSAVE format.  The x87
             * registers are located at offset 28 and take 10 bytes
             * each.
             */
	    reg = &scp->uc_mcontext.__fpregs.__fp_reg_set.__fpchip_state.__fp_state[28 + 10*index];
            DPRINTF(0, (stderr, " x87 = %g\n", (double) *(long double*) reg));
	}
    } else {
	reg = NULL;
    }
    return reg;
}

unsigned int
os_sigcontext_fpu_modes(ucontext_t *scp)
{
    unsigned int modes;

    union savefpu *sv = &scp->uc_mcontext.__fpregs.__fp_reg_set;
    struct save87 *env_87 = (struct save87 *) &sv->sv_87;
    struct fxsave *env_xmm = (struct fxsave *) &sv->sv_xmm;
    u_int16_t cw;
    u_int16_t sw;

    if (scp->uc_flags & _UC_FPU) {
	if (scp->uc_flags & _UC_FXSAVE) {
	    cw = env_xmm->fx_cw;
	    sw = env_xmm->fx_sw;
	} else {
	    cw = env_87->s87_cw & 0xffff;
	    sw = env_87->s87_sw & 0xffff;
	}
    } else {
	cw = 0;
	sw = 0x3f;
    }

    modes = ((cw & 0x3f) << 7) | (sw & 0x3f);

#ifdef FEATURE_SSE2
    if (fpu_mode == SSE2) {
	u_int32_t mxcsr = env_xmm->fx_mxcsr;

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

    /*
     * NetBSD 1.5.2 seems to insist on each mmap being less than 128MB.
     * So we mmap in 64MB steps.
     */

    if (addr)
	flags |= MAP_FIXED;

    DPRINTF(0, (stderr, "os_validate %p %d =>", addr, len));

    if (addr) {
	os_vm_address_t curaddr = addr;

	while (len > 0) {
	    os_vm_address_t resaddr;
	    int curlen = MIN(64 * 1024 * 1024, len);

	    resaddr = mmap(curaddr, curlen, OS_VM_PROT_ALL, flags, -1, 0);

	    if (resaddr == (os_vm_address_t) - 1) {
		perror("mmap");

		while (curaddr > addr) {
		    curaddr -= 64 * 1024 * 1024;
		    munmap(curaddr, 64 * 1024 * 1024);
		}

		return NULL;
	    }

	    DPRINTF(0, (stderr, " %p", resaddr));

	    curaddr += curlen;
	    len -= curlen;
	}

	DPRINTF(0, (stderr, "\n"));
    } else {
	addr = mmap(0, len, OS_VM_PROT_ALL, flags, -1, 0);

	if (addr == (os_vm_address_t) - 1) {
	    perror("mmap");
	    return NULL;
	}

	DPRINTF(0, (stderr, " %p\n", addr));
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
#ifndef GENCGC
	|| in_range_p(addr, DYNAMIC_1_SPACE_START, dynamic_space_size)
#endif
	|| in_range_p(addr, control_stack, control_stack_size)
	|| in_range_p(addr, binding_stack, binding_stack_size))
	return TRUE;
    return FALSE;
}


static void
sigsegv_handler(HANDLER_ARGS)
{
#if defined GENCGC
#if SIGSEGV_VERBOSE
    caddr_t fault_addr = code ? code->si_addr : 0;

    fprintf(stderr, "Signal %d, fault_addr=%p, page_index=%d:\n",
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
    union savefpu *sv = (union savefpu *) &ucontext->uc_mcontext.__fpregs.__fp_reg_set;
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

    if (ucontext->uc_flags & _UC_FXSAVE) {
	sv->sv_xmm.fx_sw |= trap;
    } else {
	sv->sv_87.s87_sw |= trap;
    }
    interrupt_handle_now(signal, code, context);
}

void
os_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(SIGSEGV, sigsegv_handler);
    interrupt_install_low_level_handler(SIGBUS, sigbus_handler);
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
    union savefpu *sv = &scp->uc_mcontext.__fpregs.__fp_reg_set;
    struct save87 *env_87 = (struct save87 *) &sv->sv_87;
    struct fxsave *env_xmm = (struct fxsave *) &sv->sv_xmm;
    u_int16_t cw;

    if (scp->uc_flags & _UC_FPU) {
	if (scp->uc_flags & _UC_FXSAVE) {
	    cw = env_xmm->fx_cw;
	} else {
	    cw = env_87->s87_cw & 0xffff;
	}
    } else {
	return;
    }
    DPRINTF(0, (stderr, "restore_fpu:  cw = %08x\n", (int)cw));
    __asm__ __volatile__ ("fldcw %0"::"m"(*&cw));

    if (fpu_mode == SSE2) {
	u_int32_t mxcsr = env_xmm->fx_mxcsr;

	DPRINTF(0, (stderr, "restore_fpu:  mxcsr (raw) = %04x\n", mxcsr));
	__asm__ __volatile__ ("ldmxcsr %0"::"m"(*&mxcsr));
    }
}

#ifdef i386
boolean
os_support_sse2()
{
    int support_sse2;
    size_t len;

    len = sizeof(size_t);
    if (sysctlbyname("machdep.sse2", &support_sse2, &len,
		     NULL, 0) == 0 && support_sse2 != 0)
	return TRUE;
    else
	return FALSE;
}
#endif

/*
 * Return a new string containing the path to an OS-dependent location
 * where temporary files/directories can be stored.  The string must
 * end with a slash.  If NULL is returned, such a location could not
 * be found or some other error happened.
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
    char *tmp;
    size_t len;
    char *result;

    tmp = getenv("TMP");
    if (tmp == NULL) {
	return strdup("/tmp/");
    }
    len = strlen(tmp);
    if (tmp[len] == '/') {
	return strdup(tmp);
    }
    result = malloc(len + 2);
    if (result) {
	sprintf(result, "%s/", tmp);
    }
    return result;
}
