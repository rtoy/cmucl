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

#define _GNU_SOURCE /* for reg_* constants in uc_mcontext.gregs  */
#include <signal.h>
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


#if defined(__i386) || defined(__x86_64)
/* Prototype for personality(2). Done inline here since the header file
 * for this isn't available on old versions of glibc. */
int personality (unsigned long);

#if !defined(ADDR_NO_RANDOMIZE)
#define ADDR_NO_RANDOMIZE 0x40000
#endif
/* From personality(2) */
#define CURRENT_PERSONALITY 0xffffffffUL
#endif

void
check_personality(struct utsname *name, char *const *argv, char *const *envp)
{
    /* KLUDGE: Disable memory randomization on new Linux kernels
     * by setting a personality flag and re-executing. (We need
     * to re-execute, since the memory maps that can conflict with
     * the CMUCL spaces have already been done at this point).
     *
     * Since randomization is currently implemented only on x86 kernels,
     * don't do this trick on other platforms.
     */
#if defined(__i386) || defined(__x86_64)
    int major_version, minor_version, patch_version;
    char *p;
    
    p = name->release;
    major_version = atoi(p);

    /*
     * Try to extract the minor and patch version, but if we can't
     * just set it to zero.  In particular, some Debian systems have a
     * release like "3.7-trunk-686-pae" which is missing the patch
     * version.
     */

    p = strchr(p,'.');
    if (p) {
        minor_version = atoi(p + 1);
        p = strchr(p + 1,'.');
        patch_version = p ? atoi(p + 1) : 0;
    } else {
        minor_version = 0;
        patch_version = 0;
    }

    if ((major_version == 2
         /* Some old kernels will apparently lose unsupported personality flags
          * on exec() */
         && ((minor_version == 6 && patch_version >= 11)
             || (minor_version > 6)
             /* This is what RHEL 3 reports */
             || (minor_version == 4 && patch_version > 20)))
        || major_version >= 3)
        {
            int pers = personality(CURRENT_PERSONALITY);
            if (!(pers & ADDR_NO_RANDOMIZE)) {
                int retval = personality(pers | ADDR_NO_RANDOMIZE);
                /* Allegedly some Linux kernels (the reported case was
                 * "hardened Linux 2.6.7") won't set the new personality,
                 * but nor will they return -1 for an error. So as a
                 * workaround query the new personality...
                 */
                int newpers = personality(CURRENT_PERSONALITY);
                /* ... and don't re-execute if either the setting resulted
                 * in an error or if the value didn't change. Otherwise
                 * this might result in an infinite loop.
                 */
                if (retval != -1 && newpers != pers) {
                    /* Use /proc/self/exe instead of trying to figure out
                     * the executable path from PATH and argv[0], since
                     * that's unreliable. We follow the symlink instead of
                     * executing the file directly in order to prevent top
                     * from displaying the name of the process as "exe". */
                    char runtime[PATH_MAX+1];
                    int i = readlink("/proc/self/exe", runtime, PATH_MAX);
                    if (i != -1) {
                        runtime[i] = '\0';
                        execve(runtime, argv, envp);
                    }
                }
                /* Either changing the personality or execve() failed. Either
                 * way we might as well continue, and hope that the random
                 * memory maps are ok this time around.
                 */
                fprintf(stderr, "WARNING: Couldn't re-execute CMUCL with the proper personality flags"
                        "(maybe /proc isn't mounted?). Trying to continue anyway.\n");
            }
        }
#endif
}

/*
 * Check personality here, before we start processing command line
 * args.  (Previously it was done in os_init.)  check_personality
 * can re-exec us, so we end up parsing the command line args
 * twice.  Not usually a problem unless the processing causes
 * output, which can be confusing.
 */

void
os_init0(const char *argv[], const char *envp[])
{
    struct utsname name;
    uname(&name);
        
    check_personality(&name, (char *const *) argv, (char *const *) envp);
}

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

#ifdef __i386
unsigned long *
os_sigcontext_reg(ucontext_t *scp, int offset)
{
    switch (offset) {
    case 0:
	return (unsigned long *) &scp->uc_mcontext.gregs[REG_EAX];
    case 2:
	return (unsigned long *) &scp->uc_mcontext.gregs[REG_ECX];
    case 4:
	return (unsigned long *) &scp->uc_mcontext.gregs[REG_EDX];
    case 6:
	return (unsigned long *) &scp->uc_mcontext.gregs[REG_EBX];
    case 8:
	return (unsigned long *) &scp->uc_mcontext.gregs[REG_ESP];
    case 10:
	return (unsigned long *) &scp->uc_mcontext.gregs[REG_EBP];
    case 12:
	return (unsigned long *) &scp->uc_mcontext.gregs[REG_ESI];
    case 14:
	return (unsigned long *) &scp->uc_mcontext.gregs[REG_EDI];
    case 16:
        return (unsigned long*) &scp->uc_mcontext.gregs[REG_EFL];
    }
    return NULL;
}

unsigned long *
os_sigcontext_pc(ucontext_t *scp)
{
    return (unsigned long *) &scp->uc_mcontext.gregs[REG_EIP];
}

unsigned char *
os_sigcontext_fpu_reg(ucontext_t *scp, int offset)
{
    fpregset_t fpregs = scp->uc_mcontext.fpregs;
    unsigned char *reg = NULL;
    
    if (fpregs) {
        if (offset < 8) {
            reg = (unsigned char *) &fpregs->_st[offset];
        }
#ifdef FEATURE_SSE2
        else {
            struct _fpstate *fpstate;
            fpstate = (struct _fpstate*) scp->uc_mcontext.fpregs;
            if (fpstate->magic != 0xffff) {
                reg = (unsigned char *) &fpstate->_xmm[offset - 8];
            }
        }
#endif
    }
    return reg;
}

unsigned int
os_sigcontext_fpu_modes(ucontext_t *scp)
{
    unsigned int modes;
    unsigned short cw, sw;

    if (scp->uc_mcontext.fpregs == NULL) {
	cw = 0;
	sw = 0x3f;
    } else {
	cw = scp->uc_mcontext.fpregs->cw & 0xffff;
	sw = scp->uc_mcontext.fpregs->sw & 0xffff;
    }

    modes = ((cw & 0x3f) << 7) | (sw & 0x3f);

#ifdef FEATURE_SSE2
    /*
     * Add in the SSE2 part, if we're running the sse2 core.
     */
    if (fpu_mode == SSE2) {
        struct _fpstate *fpstate;
	unsigned long mxcsr;

        fpstate = (struct _fpstate*) scp->uc_mcontext.fpregs;
        if (fpstate->magic == 0xffff) {
            mxcsr = 0;
        } else {
            mxcsr = fpstate->mxcsr;
            DPRINTF(0, (stderr, "SSE2 modes = %08lx\n", mxcsr));
        }

	modes |= mxcsr;
    }
#endif

    modes ^= (0x3f << 7);
    return modes;
}
#endif

#ifdef __x86_64
int *
sc_reg(ucontext_t *c, int offset)
{
    switch (offset) {
      case 0:
	  return &c->uc_mcontext.gregs[REG_RAX];
      case 2:
	  return &c->uc_mcontext.gregs[REG_RCX];
      case 4:
	  return &c->uc_mcontext.gregs[REG_RDX];
      case 6:
	  return &c->uc_mcontext.gregs[REG_RBX];
      case 8:
	  return &c->uc_mcontext.gregs[REG_RSP];
      case 10:
	  return &c->uc_mcontext.gregs[REG_RBP];
      case 12:
	  return &c->uc_mcontext.gregs[REG_RSI];
      case 14:
	  return &c->uc_mcontext.gregs[REG_RDI];
      case 16:
	  return &c->uc_mcontext.gregs[REG_R8];
      case 18:
	  return &c->uc_mcontext.gregs[REG_R9];
      case 20:
	  return &c->uc_mcontext.gregs[REG_R10];
      case 22:
	  return &c->uc_mcontext.gregs[REG_R11];
      case 24:
	  return &c->uc_mcontext.gregs[REG_R12];
      case 26:
	  return &c->uc_mcontext.gregs[REG_R13];
      case 28:
	  return &c->uc_mcontext.gregs[REG_R14];
      case 30:
	  return &c->uc_mcontext.gregs[REG_R15];
    }
    return (int *) 0;
}
#endif

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
	|| in_range_p(addr, (lispobj) control_stack, control_stack_size)
	|| in_range_p(addr, (lispobj) binding_stack, binding_stack_size))
	return TRUE;
    return FALSE;
}


#if defined GENCGC

static void
sigsegv_handle_now(HANDLER_ARGS)
{
    interrupt_handle_now(signal, code, context);
}

static int tramp_signal;
static siginfo_t tramp_code;
static ucontext_t tramp_context;

static void
sigsegv_handler_tramp(void)
{
    sigsegv_handle_now(tramp_signal, &tramp_code, &tramp_context);
    assert(0);
}

void
sigsegv_handler(HANDLER_ARGS)
{
    os_context_t *os_context = (os_context_t *) context;
    int fault_addr = os_context->uc_mcontext.cr2;

#ifdef RED_ZONE_HIT
    if (os_control_stack_overflow((void *) fault_addr, os_context))
	return;
#endif
    if (gc_write_barrier(code->si_addr))
	return;
#if defined(__x86_64)
    DPRINTF(0, (stderr, "sigsegv: rip: %p\n", os_context->uc_mcontext.gregs[REG_RIP]));
#else
    DPRINTF(0, (stderr, "sigsegv: eip: %x\n", os_context->uc_mcontext.gregs[REG_EIP]));
#endif

#ifdef RED_ZONE_HIT
    {
	/* Switch back to the normal stack and invoke the Lisp signal
	   handler there.  Global variables are used to pass the context
	   to the other stack. */
	tramp_signal = signal;
	tramp_code = *code;
	tramp_context = *os_context;
	SC_PC(os_context) = (unsigned long) sigsegv_handler_tramp;
	return;
    }
#endif

    sigsegv_handle_now(signal, code, os_context);
}
#else
static void
sigsegv_handler(HANDLER_ARGS)
{
    os_vm_address_t addr;

    DPRINTF(0, (stderr, "sigsegv\n"));
#ifdef i386
    interrupt_handle_now(signal, contextstruct);
#else
#define CONTROL_STACK_TOP (((char*) control_stack) + control_stack_size)

    addr = arch_get_bad_addr(signal, code, context);

    if (addr != NULL && context->sc_regs[reg_ALLOC] & (1 << 63)) {
	context->sc_regs[reg_ALLOC] -= (1 << 63);
	interrupt_handle_pending(context);
    } else if (addr > CONTROL_STACK_TOP && addr < (os_vm_address_t)binding_stack) {
	fprintf(stderr, "Possible stack overflow at 0x%08lX!\n", addr);
	/* try to fix control frame pointer */
	while (!((lispobj)control_stack <= *current_control_frame_pointer &&
		 *current_control_frame_pointer <= CONTROL_STACK_TOP))
	    ((char *) current_control_frame_pointer) -= sizeof(lispobj);
	ldb_monitor();
    } else if (!interrupt_maybe_gc(signal, code, context))
	interrupt_handle_now(signal, code, context);
#endif
}
#endif

static void
sigbus_handler(HANDLER_ARGS)
{
    DPRINTF(1, (stderr, "sigbus:\n"));	/* there is no sigbus in linux??? */
    interrupt_handle_now(signal, code, context);
}

void
os_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(SIGSEGV, sigsegv_handler);
    interrupt_install_low_level_handler(SIGBUS, sigbus_handler);
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

void
restore_fpu(ucontext_t *context)
{
    if (context->uc_mcontext.fpregs) {
	short cw = context->uc_mcontext.fpregs->cw;
        DPRINTF(0, (stderr, "restore_fpu:  cw = %08x\n", cw));
	__asm__ __volatile__ ("fldcw %0" : : "m" (*&cw));
#ifdef FEATURE_SSE2
        if (fpu_mode == SSE2) {
            struct _fpstate *fpstate;
            unsigned int mxcsr;
            
            fpstate = (struct _fpstate*) context->uc_mcontext.fpregs;
            if (fpstate->magic != 0xffff) {
                mxcsr = fpstate->mxcsr;
                DPRINTF(0, (stderr, "restore_fpu:  mxcsr (raw) = %04x\n", mxcsr));
                __asm__ __volatile__ ("ldmxcsr %0" :: "m" (*&mxcsr));
            }
        }
#endif        
    }
}

#ifdef i386
boolean
os_support_sse2()
{
    return TRUE;
}
#endif
