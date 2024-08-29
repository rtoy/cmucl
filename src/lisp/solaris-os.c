/*
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is an experimental Solaris version based on sunos-os.c but
 * without the VM hack of unmapped pages for the GC trigger which
 * caused trouble when system calls were passed unmapped pages.
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/file.h>

#include <stropts.h>
#include <termios.h>
#include <unistd.h>
#include <errno.h>
#include <sys/param.h>

#include <dlfcn.h>

#include <sys/resource.h>

#if defined(GENCGC)
#include "lisp.h"
/* Need struct code defined to get rid of warning from gencgc.h */
#include "internals.h"
#include "gencgc.h"
#endif

#include "os.h"

#include "interrupt.h"

/* To get dynamic_0_space and friends */
#include "globals.h"
/* To get memory map */
#include "sparc-validate.h"

/* For type_ListPointer and NIL */
#include "internals.h"

#define EMPTYFILE "/tmp/empty"
#define ZEROFILE "/dev/zero"

/* ---------------------------------------------------------------- */


long os_vm_page_size = (-1);
static long os_real_page_size = (-1);

static int zero_fd = (-1);

static os_vm_size_t real_page_size_difference = 0;

static void
os_init_bailout(char *arg)
{
    char buf[500];

    sprintf(buf, "os_init: %s", arg);
    perror(buf);
    exit(1);
}

void
os_init0(const char *argv[], const char *envp[])
{}

void
os_init(const char *argv[], const char *envp[])
{
    zero_fd = open(ZEROFILE, O_RDONLY);
    if (zero_fd < 0)
	os_init_bailout(ZEROFILE);

    os_vm_page_size = os_real_page_size = sysconf(_SC_PAGESIZE);

    if (os_vm_page_size > OS_VM_DEFAULT_PAGESIZE) {
	fprintf(stderr, "os_init: Pagesize too large (%ld > %d)\n",
		os_vm_page_size, OS_VM_DEFAULT_PAGESIZE);
	exit(1);
    } else {
	/*
	 * we do this because there are apparently dependencies on
	 * the pagesize being OS_VM_DEFAULT_PAGESIZE somewhere...
	 * but since the OS doesn't know we're using this restriction,
	 * we have to grovel around a bit to enforce it, thus anything
	 * that uses real_page_size_difference.
	 */
	real_page_size_difference = OS_VM_DEFAULT_PAGESIZE - os_vm_page_size;
	os_vm_page_size = OS_VM_DEFAULT_PAGESIZE;
    }
}

/* ---------------------------------------------------------------- */

os_vm_address_t os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_NORESERVE;

    if (addr)
	flags |= MAP_FIXED;

    addr = (os_vm_address_t) mmap((void *) addr, len, OS_VM_PROT_ALL, flags, zero_fd, 0);

    if (addr == (os_vm_address_t) - 1) {
	perror("mmap");
        addr = NULL;
    }

    return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (munmap((void *) addr, len) == -1)
	perror("munmap");
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    if (
	(addr =
	 (os_vm_address_t) mmap((void *) addr, len, OS_VM_PROT_ALL,
				MAP_PRIVATE | MAP_FIXED, fd,
				(off_t) offset)) == (os_vm_address_t) - 1)
	perror("mmap");

    return addr;
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
#ifndef i386
    static int flushit = -1;

    /*
     * On some systems, iflush needs to be emulated in the kernel
     * On those systems, it isn't necessary
     * Call getenv() only once.
     */
    if (flushit == -1)
	flushit = getenv("CMUCL_NO_SPARC_IFLUSH") == 0;

    if (flushit) {
	static int traceit = -1;

	if (traceit == -1)
	    traceit = getenv("CMUCL_TRACE_SPARC_IFLUSH") != 0;

	if (traceit)
	    fprintf(stderr, ";;;iflush %p - %lx\n", (void *) address, length);
	flush_icache((unsigned int *) address, length);
    }
#endif
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect((void *) address, length, prot) == -1) {
        char msg[1000];

        snprintf(msg, sizeof(msg), "mprotect: os_protect(0x%p, %u, 0x%x): ",
                 address, length, prot);
        
	perror(msg);
    }
}

static boolean
in_range_p(os_vm_address_t a, lispobj sbeg, size_t slen)
{
    char *beg = (char *) sbeg;
    char *end = (char *) sbeg + slen;
    char *adr = (char *) a;

    return (adr >= beg && adr < end);
}

boolean valid_addr(os_vm_address_t addr)
{
    /* Just assume address is valid if it lies within one of the known
       spaces.  (Unlike sunos-os which keeps track of every valid page.) */
    return (in_range_p(addr, READ_ONLY_SPACE_START, read_only_space_size)
	    || in_range_p(addr, STATIC_SPACE_START, static_space_size)
	    || in_range_p(addr, DYNAMIC_0_SPACE_START, dynamic_space_size)
#ifndef GENCGC
	    || in_range_p(addr, DYNAMIC_1_SPACE_START, dynamic_space_size)
#endif
	    || in_range_p(addr, (lispobj)control_stack, control_stack_size)
	    || in_range_p(addr, (lispobj)binding_stack, binding_stack_size));
}

/* ---------------------------------------------------------------- */

/*
 * Running into the gc trigger page will end up here...
 */
#if defined(GENCGC)

void
segv_handle_now(HANDLER_ARGS)
{
    interrupt_handle_now(signal, code, context);
}

void real_segv_handler(HANDLER_ARGS)
{
    segv_handle_now(signal, code, context);
}

void
segv_handler(HANDLER_ARGS)
{
    os_context_t *os_context = (os_context_t *) context;
    
    caddr_t addr = code->si_addr;

    SAVE_CONTEXT();

#ifdef RED_ZONE_HIT
    if (os_control_stack_overflow(addr, context))
	return;
#endif

    if (gc_write_barrier(code->si_addr))
	 return;

    /*
     * Could be a C stack overflow.  Let's check
     */

    {
	struct rlimit rlimit;

	if (getrlimit(RLIMIT_STACK, &rlimit) == 0) {
	    /* The stack top here is based on the notes in sparc-validate.h */
	    char *stack_top = (char *) 0xffbf0000;
	    char *stack_bottom;

	    stack_bottom = stack_top - rlimit.rlim_cur;

	    /*
	     * Add a fudge factor.  Don't know why, but we get the signal
	     * sometime after the bottom of the stack, as computed above,
	     * has been reached.  (It seems to be 8K, so we use something
	     * larger.)
	     */

	    stack_bottom -= 16384;

	    if ((stack_bottom <= addr) && (addr <= stack_top)) {
		fprintf(stderr,
			"\nsegv_handler:  C stack overflow.  Try increasing stack limit (%ld).\n",
			rlimit.rlim_cur);

		segv_handle_now(signal, code, context);
	    }
	} else {
	    perror("getrlimit");
	}
    }

    /* a *real* protection fault */
    fprintf(stderr, "segv_handler: Real protection violation: %p, PC = %p\n",
            addr,
            os_context->uc_mcontext.gregs[1]);
    real_segv_handler(signal, code, context);
}
#else
void
segv_handler(HANDLER_ARGS)
{
    caddr_t addr = code->si_addr;

    SAVE_CONTEXT();

#ifdef RED_ZONE_HIT
    if (os_control_stack_overflow(addr, context))
	return;
#endif

    if (!interrupt_maybe_gc(signal, code, context)) {
	/* a *real* protection fault */
	fprintf(stderr, "segv_handler: Real protection violation: 0x%08x\n",
		addr);
	interrupt_handle_now(signal, code, context);
    }
}
#endif

void
os_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(SIGSEGV, segv_handler);
}


/* function definitions for register lvalues */

#ifndef i386
int *
solaris_register_address(struct ucontext *context, int reg)
{
    if (reg == 0) {
	static int zero;

	zero = 0;

	return &zero;
    } else if (reg < 16) {
	return &context->uc_mcontext.gregs[reg + 3];
    } else if (reg < 32) {
	int *sp = (int *) context->uc_mcontext.gregs[REG_SP];

	return &sp[reg - 16];
    } else
	return 0;
}
#endif

/* function defintions for backward compatibilty and static linking */

/* For now we put in some porting functions */

int
sigblock(int mask)
{
    sigset_t old, new;

    sigemptyset(&new);
    new.__sigbits[0] = mask;

    sigprocmask(SIG_BLOCK, &new, &old);

    return old.__sigbits[0];
}

int
sigsetmask(int mask)
{
    sigset_t old, new;

    sigemptyset(&new);
    new.__sigbits[0] = mask;

    sigprocmask(SIG_SETMASK, &new, &old);

    return old.__sigbits[0];

}

int
openpty(int *amaster, int *aslave, char *name, struct termios *termp,
        struct winsize *winp)
{
    char *slavename;
    int masterfd, slavefd;

    if ((masterfd = open("/dev/ptmx", O_RDWR)) == -1)
	return -1;
    if (grantpt(masterfd) == -1) {
	close(masterfd);
	return -1;
    }
    if (unlockpt(masterfd) == -1) {
	close(masterfd);
	return -1;
    }
    if ((slavename = ptsname(masterfd)) == NULL) {
	close(masterfd);
	return -1;
    }
    if ((slavefd = open(slavename, O_RDWR | O_NOCTTY)) == -1) {
	close(masterfd);
	return -1;
    }
    *amaster = masterfd;
    *aslave = slavefd;
    ioctl(*aslave, I_PUSH, "ptem");
    ioctl(*aslave, I_PUSH, "ldterm");
    ioctl(*aslave, I_PUSH, "ttcompat");
    if (name)
	strcpy(name, slavename);
    if (termp)
	tcsetattr(slavefd, TCSAFLUSH, termp);
    if (winp)
	ioctl(slavefd, TIOCSWINSZ, (char *) winp);
    return 0;
}

os_vm_address_t round_up_sparse_size(os_vm_address_t addr)
{
    return (addr + SPARSE_BLOCK_SIZE - 1) & ~SPARSE_SIZE_MASK;
}

/*
 * An array of the start of the spaces which should have holes placed
 * after them.  Must not include the dynamic spaces because the size
 * of the dynamic space can be controlled from the command line.  Also
 * must not include the binding and control stacks.  They're handled
 * below.
 */
static os_vm_address_t spaces[] = {
    READ_ONLY_SPACE_START, STATIC_SPACE_START,
};

/*
  
 * The corresponding array for the size of each space.  Be sure that
 * the spaces and holes don't overlap!  The sizes MUST be on
 * SPARSE_BLOCK_SIZE boundaries.
 
 */
static unsigned long *space_size[] = {
    &read_only_space_size, &static_space_size,
    &binding_stack_size, &control_stack_size
};

/*
 * The size of the hole to make.  It should be strictly smaller than
 * SPARSE_BLOCK_SIZE.
 */

#define HOLE_SIZE 0x2000

void
make_hole(os_vm_address_t space_start, size_t space_size)
{
    os_vm_address_t hole;

    /* Make holes of the appropriate size for desired spaces */

    hole = space_start + space_size;

    if (os_validate(hole, HOLE_SIZE) == NULL) {
        fprintf(stderr,
                "ensure_space: Failed to validate hole of %d bytes at 0x%08lX\n",
                HOLE_SIZE, (unsigned long) hole);
        exit(1);
    }
    /* Make it inaccessible */
    os_protect(hole, HOLE_SIZE, 0);
}

void
make_holes(void)
{
    int k;
    os_vm_address_t hole;

    /*
     * Make holes of the appropriate size for desired spaces.  The
     * stacks are handled in make_stack_holes, if they are
     * relocatable.
     */

    for (k = 0; k < sizeof(spaces) / sizeof(spaces[0]); ++k) {
        make_hole(spaces[k], *space_size[k]);
    }
    
    
    /* Round up the dynamic_space_size to the nearest SPARSE_BLOCK_SIZE */
    dynamic_space_size = round_up_sparse_size(dynamic_space_size);

    /* Now make a hole for the dynamic spaces */
    hole = dynamic_space_size + (os_vm_address_t) dynamic_0_space;

    if (os_validate(hole, HOLE_SIZE) == NULL) {
	fprintf(stderr,
		"ensure_space: Failed to validate hold of %d bytes at 0x%08lX\n",
		HOLE_SIZE, (unsigned long) hole);
	exit(1);
    }
    os_protect(hole, HOLE_SIZE, 0);

#ifndef GENCGC
    hole = dynamic_space_size + (os_vm_address_t) dynamic_1_space;
    if (os_validate(hole, HOLE_SIZE) == NULL) {
	fprintf(stderr,
		"ensure_space: Failed to validate hole of %ld bytes at 0x%08X\n",
		HOLE_SIZE, (unsigned long) hole);
	exit(1);
    }
    os_protect(hole, HOLE_SIZE, 0);
#endif
}

void
make_stack_holes(void)
{
    make_hole((os_vm_address_t)control_stack, control_stack_size);
    make_hole((os_vm_address_t)binding_stack, binding_stack_size);
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

    return sym_addr;
}

#ifdef i386
unsigned long *
os_sigcontext_reg(ucontext_t *scp, int index)
{
#if 0
    fprintf(stderr, "os_sigcontext_reg index = %d\n", index);
#endif    
    switch (index) {
    case 0:
	return (unsigned long *) &scp->uc_mcontext.gregs[EAX];
    case 2:
	return (unsigned long *) &scp->uc_mcontext.gregs[ECX];
    case 4:
	return (unsigned long *) &scp->uc_mcontext.gregs[EDX];
    case 6:
	return (unsigned long *) &scp->uc_mcontext.gregs[EBX];
    case 8:
	return (unsigned long *) &scp->uc_mcontext.gregs[ESP];
    case 10:
	return (unsigned long *) &scp->uc_mcontext.gregs[EBP];
    case 12:
	return (unsigned long *) &scp->uc_mcontext.gregs[ESI];
    case 14:
	return (unsigned long *) &scp->uc_mcontext.gregs[EDI];
    }
    return NULL;
}

unsigned long *
os_sigcontext_pc(ucontext_t *scp)
{
#if 0
    fprintf(stderr, "os_sigcontext_pc = %p\n", scp->uc_mcontext.gregs[EIP]);
#endif
    return (unsigned long *) &scp->uc_mcontext.gregs[EIP];
}


unsigned char *
os_sigcontext_fpu_reg(ucontext_t *scp, int offset)
{
    fpregset_t *fpregs = &scp->uc_mcontext.fpregs;
    unsigned char *reg = NULL;

    if (offset < 8) {
        unsigned char *fpustate;
        unsigned char *stregs;

        /*
         * Not sure this is right.  There is no structure defined for
         * the x87 fpu state in /usr/include/sys/regset.h
         */
        
        /* Point to the fpchip_state */
        fpustate = (unsigned char*) &fpregs->fp_reg_set.fpchip_state.state[0];
        /* Skip to where the x87 fp registers are */
        stregs = fpustate + 28;
    
        reg = stregs + 10*offset;
    }
#ifdef FEATURE_SSE2
    else {
        reg = (unsigned char*) &fpregs->fp_reg_set.fpchip_state.xmm[offset - 8];
    }
#endif

    return reg;
}

unsigned int
os_sigcontext_fpu_modes(ucontext_t *scp)
{
    unsigned int modes;
    unsigned short cw, sw;
    fpregset_t *fpr;
    unsigned int state;
        
    fpr = &scp->uc_mcontext.fpregs;

    cw = fpr->fp_reg_set.fpchip_state.state[0] & 0xffff;
    sw = fpr->fp_reg_set.fpchip_state.state[1] & 0xffff;

    modes = ((cw & 0x3f) << 7) | (sw & 0x3f);

    DPRINTF(0, (stderr, "cw = 0x%04x\n", cw));
    DPRINTF(0, (stderr, "sw = 0x%04x\n", sw));
    DPRINTF(0, (stderr, "modes = 0x%08x\n", modes));
    
#ifdef FEATURE_SSE2
    /*
     * Add in the SSE2 part, if we're running the sse2 core.
     */
    if (fpu_mode == SSE2) {
	unsigned long mxcsr;

        mxcsr = fpr->fp_reg_set.fpchip_state.mxcsr;
        DPRINTF(0, (stderr, "SSE2 modes = %08lx\n", mxcsr));

	modes |= mxcsr;
    }
#endif

    modes ^= (0x3f << 7);
    return modes;
}

unsigned int
os_set_sigcontext_fpu_modes(ucontext_t *scp, uint32_t modes)
{
    unsigned short cw, sw;
    fpregset_t *fpr;
    unsigned int state;
        
    fpr = &scp->uc_mcontext.fpregs;

    cw = modes & 0x3f;
    sw = (modes >> 7) &0x3f;

    DPRINTF(1, (stderr, "modes = 0x%08x\n", modes));
    DPRINTF(1, (stderr, "cw = 0x%04x\n", cw));
    DPRINTF(1, (stderr, "sw = 0x%04x\n", sw));

    fpr->fp_reg_set.fpchip_state.state[0] = cw;
    fpr->fp_reg_set.fpchip_state.state[1] = sw;
    
#ifdef FEATURE_SSE2
    /*
     * Add in the SSE2 part, if we're running the sse2 core.
     */
    if (fpu_mode == SSE2) {
	unsigned long mxcsr = modes & 0xffff;

        DPRINTF(1, (stderr, "SSE2 modes = %08lx\n", mxcsr));
        fpr->fp_reg_set.fpchip_state.mxcsr = mxcsr;

	modes |= mxcsr;
    }
#endif

    modes ^= (0x3f << 7);
    return modes;
}



boolean
os_support_sse2()
{
    return TRUE;
}
#endif
