/*
 * Darwin-os.c.
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
 * Frobbed for Darwin by Pierre R. Mai, 2003.
 *
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include <errno.h>
#include <dlfcn.h>
#include <string.h>
#include <assert.h>

#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "lispregs.h"
#include "internals.h"
#ifdef GENCGC
#include "gencgc.h"
#endif

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
/* #include <sys/proc.h> */
/* For timebase info */
#include <sys/sysctl.h>


/* Need this to define ppc_saved_state_t (for 10.4) */
#include <mach/thread_status.h>

#include "validate.h"
vm_size_t os_vm_page_size;



/*
 * This is accessed by Lisp!  Make this a static symbol?
 */
int cycles_per_tick = 1;

/*
 * Compute the conversion factor from the numbef of time base ticks to
 * clock frequency.  The timebase counter on PPC is not a cycle
 * counter; we have to derive the relationship ourselves.
 */

#ifdef __ppc__
void
timebase_init(void)
{
    int mib[2];
    unsigned tbfrequency;
    unsigned cpufrequency;
    unsigned int miblen;
    size_t len;

    mib[0] = CTL_HW;
#ifndef HW_TB_FREQ
    /*
     * Mac OS X 10.2.8 doesn't have this, so we take it from 10.3.8,
     * which does.
     */
#define HW_TB_FREQ	23
#endif
    mib[1] = HW_TB_FREQ;
    miblen = 2;
    len = sizeof(tbfrequency);

    if (sysctl(mib, miblen, &tbfrequency, &len, NULL, 0) == -1) {
	perror("Error getting HW_TB_FREQ from sysctl: ");
    }

    mib[0] = CTL_HW;
    mib[1] = HW_CPU_FREQ;
    miblen = 2;
    len = sizeof(cpufrequency);
    if (sysctl(mib, miblen, &cpufrequency, &len, NULL, 0) == -1) {
	perror("Error getting HW_CPU_FREQ from sysctl: ");
    }

    cycles_per_tick = 0.5 + (cpufrequency / (double) tbfrequency);
}
#endif


void
os_init0(const char *argv[], const char *envp[])
{}

void
os_init(const char *argv[], const char *envp[])
{
    os_vm_page_size = OS_VM_DEFAULT_PAGESIZE;
#ifdef __ppc__
    timebase_init();
#endif
}


#if defined(__ppc__)
#if __DARWIN_UNIX03
  /* Nothing needed for 10.5 */
#else
/* For 10.4 */
#define __ss ss
#define __r0 r0
#define __r1 r1
#define __r2 r2
#define __r3 r3
#define __r4 r4
#define __r5 r5
#define __r6 r6
#define __r7 r7
#define __r8 r8
#define __r9 r9
#define __r10 r10
#define __r11 r11
#define __r12 r12
#define __r13 r13
#define __r14 r14
#define __r15 r15
#define __r16 r16
#define __r17 r17
#define __r18 r18
#define __r19 r19
#define __r20 r20
#define __r21 r21
#define __r22 r22
#define __r23 r23
#define __r24 r24
#define __r25 r25
#define __r26 r26
#define __r27 r27
#define __r28 r28
#define __r29 r29
#define __r30 r30
#define __r31 r31
#define __lr lr
#define __ctr ctr
#define __es es
#define __dar dar
#define __dsisr dsisr
#endif

unsigned int *
sc_reg(os_context_t * context, int offset)
{
    _STRUCT_PPC_THREAD_STATE *state = &context->uc_mcontext->__ss;

    switch (offset) {
      case 0:
	  return &state->__r0;
      case 1:
	  return &state->__r1;
      case 2:
	  return &state->__r2;
      case 3:
	  return &state->__r3;
      case 4:
	  return &state->__r4;
      case 5:
	  return &state->__r5;
      case 6:
	  return &state->__r6;
      case 7:
	  return &state->__r7;
      case 8:
	  return &state->__r8;
      case 9:
	  return &state->__r9;
      case 10:
	  return &state->__r10;
      case 11:
	  return &state->__r11;
      case 12:
	  return &state->__r12;
      case 13:
	  return &state->__r13;
      case 14:
	  return &state->__r14;
      case 15:
	  return &state->__r15;
      case 16:
	  return &state->__r16;
      case 17:
	  return &state->__r17;
      case 18:
	  return &state->__r18;
      case 19:
	  return &state->__r19;
      case 20:
	  return &state->__r20;
      case 21:
	  return &state->__r21;
      case 22:
	  return &state->__r22;
      case 23:
	  return &state->__r23;
      case 24:
	  return &state->__r24;
      case 25:
	  return &state->__r25;
      case 26:
	  return &state->__r26;
      case 27:
	  return &state->__r27;
      case 28:
	  return &state->__r28;
      case 29:
	  return &state->__r29;
      case 30:
	  return &state->__r30;
      case 31:
	  return &state->__r31;
      case 34:
	  /*
	   * Not sure if this is really defined anywhere, but after
	   * r31 is cr, xer, lr, and ctr.  So we let 34 be lr.
	   */
	  return &state->__lr;
      case 35:
	  return &state->__ctr;
      case 41:
	  return (unsigned int *) &context->uc_mcontext->__es.__dar;
      case 42:
	  return (unsigned int *) &context->uc_mcontext->__es.__dsisr;
    }

    return (unsigned int *) 0;
}
#elif defined(__i386__)
#if __DARWIN_UNIX03
  /* Nothing needed for 10.5 */
#else
  /* This is for 10.4 */
#define __ss ss
#define __eax eax
#define __ecx ecx
#define __edx edx
#define __ebx ebx
#define __esp esp
#define __ebp ebp
#define __esi esi
#define __edi edi
#define __eip eip  
#define __fs fs
#define __fpu_stmm0 fpu_stmm0
#define __fpu_stmm1 fpu_stmm1
#define __fpu_stmm2 fpu_stmm2
#define __fpu_stmm3 fpu_stmm3
#define __fpu_stmm4 fpu_stmm4
#define __fpu_stmm5 fpu_stmm5
#define __fpu_stmm6 fpu_stmm6
#define __fpu_stmm7 fpu_stmm7
#define __fpu_fcw   fpu_fcw
#define __fpu_fsw   fpu_fsw
#define __fpu_mxcsr fpu_mxcsr
#ifdef FEATURE_SSE2
#define __fpu_xmm0 fpu_xmm0
#define __fpu_xmm1 fpu_xmm1
#define __fpu_xmm2 fpu_xmm2
#define __fpu_xmm3 fpu_xmm3
#define __fpu_xmm4 fpu_xmm4
#define __fpu_xmm5 fpu_xmm5
#define __fpu_xmm6 fpu_xmm6
#define __fpu_xmm7 fpu_xmm7
#endif
#endif

unsigned long *
os_sigcontext_reg(ucontext_t *scp, int index)
{
    switch (index) {
    case 0:
	return (unsigned long *) &scp->uc_mcontext->__ss.__eax;
    case 2:
	return (unsigned long *) &scp->uc_mcontext->__ss.__ecx;
    case 4:
	return (unsigned long *) &scp->uc_mcontext->__ss.__edx;
    case 6:
	return (unsigned long *) &scp->uc_mcontext->__ss.__ebx;
    case 8:
	return (unsigned long *) &scp->uc_mcontext->__ss.__esp;
    case 10:
	return (unsigned long *) &scp->uc_mcontext->__ss.__ebp;
    case 12:
	return (unsigned long *) &scp->uc_mcontext->__ss.__esi;
    case 14:
	return (unsigned long *) &scp->uc_mcontext->__ss.__edi;
    }
    return NULL;
}

unsigned long *
os_sigcontext_pc(ucontext_t *scp)
{
    return (unsigned long *) &scp->uc_mcontext->__ss.__eip;
}

unsigned char *
os_sigcontext_fpu_reg(ucontext_t *scp, int index)
{
    switch (index) {
    case 0:
	return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_stmm0;
    case 1:
	return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_stmm1;
    case 2:
	return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_stmm2;
    case 3:
	return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_stmm3;
    case 4:
	return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_stmm4;
    case 5:
	return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_stmm5;
    case 6:
	return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_stmm6;
    case 7:
	return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_stmm7;
    case 8:
       return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_xmm0;
    case 9:
       return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_xmm1;
    case 10:
       return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_xmm2;
    case 11:
       return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_xmm3;
    case 12:
       return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_xmm4;
    case 13:
       return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_xmm5;
    case 14:
       return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_xmm6;
    case 15:
      return (unsigned char *) &scp->uc_mcontext->__fs.__fpu_stmm7;
    default:
      return NULL;
    }
}

unsigned int
os_sigcontext_fpu_modes(ucontext_t *scp)
{
    unsigned int modes;

    assert(fpu_mode == SSE2);
    
    modes = scp->uc_mcontext->__fs.__fpu_mxcsr;
    DPRINTF(0, (stderr, "SSE2 modes = %08x\n", modes));

    /* Convert exception mask to exception enable */
    modes ^= (0x3f << 7);
    DPRINTF(0, (stderr, "Finale = %08x\n", modes));
    return modes;
}

void
restore_fpu(ucontext_t *scp)
{
    unsigned int mxcsr;

    mxcsr = scp->uc_mcontext->__fs.__fpu_mxcsr;
    DPRINTF(0, (stderr, "restore_fpu:  mxcsr (raw) = %04x\n", mxcsr));

    __asm__ __volatile__ ("ldmxcsr %0" :: "m" (*&mxcsr));
}
#endif

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_ANON;

    if (addr)
	flags |= MAP_FIXED;

    DPRINTF(0, (stderr, "os_validate %p %d => ", addr, len));

    addr = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);

    if (addr == (os_vm_address_t) - 1) {
	perror("mmap");
	return NULL;
    }

    DPRINTF(0, (stderr, "%p\n", addr));

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
#ifdef __ppc__    
    /* see ppc-arch.c */
    ppc_flush_icache(address, length);
#endif
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
	|| in_range_p(addr, (lispobj)control_stack, control_stack_size)
	|| in_range_p(addr, (lispobj)binding_stack, binding_stack_size))
	return TRUE;
    return FALSE;
}

static void
sigbus_handle_now(HANDLER_ARGS)
{
    interrupt_handle_now(signal, code, context);
}

static void
sigbus_handler(HANDLER_ARGS)
{
    os_context_t *os_context = (os_context_t *) context;
#if defined(GENCGC)
    caddr_t fault_addr = code->si_addr;
#endif
    
#ifdef RED_ZONE_HIT
    if (os_control_stack_overflow((void *) fault_addr, os_context))
       return;
#endif

#ifdef __ppc__
    DPRINTF(0, (stderr, "sigbus:\n"));
    DPRINTF(0, (stderr, " PC       = %p\n", SC_PC(os_context)));
    DPRINTF(0, (stderr, " ALLOC-TN = %p\n", SC_REG(os_context, reg_ALLOC)));
    DPRINTF(0, (stderr, " CODE-TN  = %p\n", SC_REG(os_context, reg_CODE)));
    DPRINTF(0, (stderr, " LRA-TN   = %p\n", SC_REG(os_context, reg_LRA)));
    DPRINTF(0, (stderr, " CFP-TN   = %p\n", SC_REG(os_context, reg_CFP)));
    DPRINTF(0, (stderr, " FDEFN-TN = %p\n", SC_REG(os_context, reg_FDEFN)));
    DPRINTF(0, (stderr, " foreign_function_call = %d\n", foreign_function_call_active));
#endif
    
#if defined(GENCGC)
#if defined(SIGSEGV_VERBOSE)
    fprintf(stderr, "Signal %d, fault_addr=%x, page_index=%d:\n",
	    signal, fault_addr, page_index);
#endif
    if (gc_write_barrier(code->si_addr))
	 return;
#else
    if (interrupt_maybe_gc(signal, code, os_context))
	return;
#endif
    /* a *real* protection fault */
    fprintf(stderr, "sigbus_handler: Real protection violation at %p, PC = %p\n",
            fault_addr, (void *) SC_PC(os_context));
    sigbus_handle_now(signal, code, os_context);
#ifdef __ppc__
    /* Work around G5 bug; fix courtesy gbyers via chandler */
    sigreturn(os_context);
#endif
}

void
os_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(SIGBUS, sigbus_handler);
}

void *
os_dlsym(const char *sym_name, lispobj lib_list)
{
    static void *program_handle;
    void *sym_addr = 0;
    int offset = sym_name[0] == '_' ? 1 : 0;

#if 0
    if (offset == 0) {
        fprintf(stderr, "sym-name = %s\n", sym_name);
    }
#endif    
    if (!program_handle)
	program_handle = dlopen((void *) 0, RTLD_LAZY | RTLD_GLOBAL);
    if (lib_list != NIL) {
	lispobj lib_list_head;

	for (lib_list_head = lib_list;
	     lib_list_head != NIL; lib_list_head = (CONS(lib_list_head))->cdr) {
	    struct cons *lib_cons = CONS(CONS(lib_list_head)->car);
	    struct sap *dlhandle = (struct sap *) PTR(lib_cons->car);

            /*
             * On Darwin, dlsym assumes the C name, so skip the underscore that
             * is prepended by EXTERN-ALIEN-NAME.
             */
            sym_addr = dlsym((void *) dlhandle->pointer, sym_name + offset);
	    if (sym_addr)
                return sym_addr;
	}
    }

    sym_addr = dlsym(program_handle, sym_name + offset);

    return sym_addr;
}

#ifdef i386
boolean
os_support_sse2()
{
    return TRUE;
}
#endif

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
     * macosx has a secure per-user temporary directory.
     * Don't cache the result as this is only called once.
     */
    char path[PATH_MAX];

    int pathSize = confstr(_CS_DARWIN_USER_TEMP_DIR, path, PATH_MAX);
    if (pathSize == 0 || pathSize > PATH_MAX) {
	strlcpy(path, "/tmp", sizeof(path));
    }

    return strdup(path);
}
