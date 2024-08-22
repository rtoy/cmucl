/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lisp.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "internals.h"
#include "arch.h"
#include "lispregs.h"
#include "signal.h"
#include "alloc.h"
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"

#define BREAKPOINT_INST 0xcc	/* INT3 */

/*
 * The first two bytes of the UD1 instruction.  The mod r/m byte isn't
 * included here.
 */
static const unsigned char ud1[] = {0x0f, 0xb9};

/*
 * Extract the error trap code from the UD1 instruction.  BYTE must be
 * the 3rd byte of the UD1 instruction that represents the mod r/m
 * byte.
 */
#define UD1_CODE(modrm) ((modrm) & 0x3f)

/*
 * Set to positive value to enabled debug prints related to the sigill
 * and sigtrap handlers.  Also enables prints related to handling of
 * breakpoints.
 */
unsigned int debug_handlers = 0;

#if defined(SOLARIS)
/*
 * Use the /dev/cpu/self/cpuid interface on Solaris.  We could use the
 * same method below, but the Sun C compiler miscompiles the inline
 * assembly.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

void cpuid(int level, unsigned int* a, unsigned int* b,
           unsigned int* c, unsigned int* d)
{
    int device;
    uint32_t regs[4];
    static const char devname[] = "/dev/cpu/self/cpuid";

        *a = *b = *c = *d = 0;
    if ((device = open(devname, O_RDONLY)) == -1) {
        perror(devname);
        goto exit;
    }

    if (pread(device, regs, sizeof(regs), 1) != sizeof(regs)) {
        perror(devname);
        goto exit;
    }

    *a = regs[0];
    *b = regs[1];
    *c = regs[2];
    *d = regs[3];

  exit:
    (void) close(device);

    return;
}

#else
#define __cpuid(level, a, b, c, d)			\
  __asm__ ("xchgl\t%%ebx, %1\n\t"			\
	   "cpuid\n\t"					\
	   "xchgl\t%%ebx, %1\n\t"			\
	   : "=a" (a), "=r" (b), "=c" (c), "=d" (d)	\
	   : "0" (level))

void cpuid(int level, unsigned int* a, unsigned int* b,
           unsigned int* c, unsigned int* d)
{
    unsigned int eax, ebx, ecx, edx;
    
    __cpuid(level, eax, ebx, ecx, edx);

    *a = eax;
    *b = ebx;
    *c = ecx;
    *d = edx;
}
#endif

int
arch_support_sse2(void)
{
    unsigned int eax, ebx, ecx, edx;

    cpuid(1, &eax, &ebx, &ecx, &edx);

    /* Return non-zero if SSE2 is supported */
    return edx & 0x4000000;
}

char *
arch_init(fpu_mode_t mode)
{
    int have_sse2;

    have_sse2 = arch_support_sse2() && os_support_sse2();
    
    if (!have_sse2) {
        fprintf(stderr, "CMUCL requires a SSE2 support; exiting\n");
        abort();
    }
        
    switch (mode) {
      case AUTO:
      case X87:
          fprintf(stderr, "fpu mode AUTO or X87 is not longer supported.\n");
          /* Fall through and return the sse2 core */
      case SSE2:
          return "lisp-sse2.core";
          break;
      default:
          abort();
    }
}



/*
 * Skip the UD1 instruction, and any data bytes associated with the
 * trap.
 */
void
arch_skip_instruction(os_context_t * context)
{
    int vlen, code;

    DPRINTF(debug_handlers,
            (stderr, "[arch_skip_inst at %lx>]\n", SC_PC(context)));

    /* Get the address of the beginning of the UD1 instruction */
    char* pc = (char *) SC_PC(context);
    
    /*
     * Skip over the part of the UD1 inst (0x0f, 0xb9) so we can get to the mod r/m byte
     */
    pc += sizeof(ud1);

    code = UD1_CODE(*pc++);

    SC_PC(context) = (unsigned long) pc;

    switch (code) {
      case trap_Error:
      case trap_Cerror:
	  /* Lisp error arg vector length */
          vlen = *pc++;
          SC_PC(context) = (unsigned long) pc;
          
	  /* Skip lisp error arg data bytes */
          SC_PC(context) = (unsigned long) (pc + vlen);
	  break;

      case trap_Breakpoint:
          lose("Unexpected breakpoint trap in arch_skip_instruction\n");
          break;
      case trap_FunctionEndBreakpoint:
	  break;

      case trap_PendingInterrupt:
      case trap_Halt:
	  /* Only needed to skip the Code. */
	  break;

      default:
	  fprintf(stderr, "[arch_skip_inst invalid code 0x%x\n]\n", code);
	  break;
    }

    DPRINTF(debug_handlers,
            (stderr, "[arch_skip_inst resuming at %lx>]\n", SC_PC(context)));
}

unsigned char *
arch_internal_error_arguments(os_context_t * context)
{
    return (unsigned char *) (SC_PC(context) + 1);
}

boolean
arch_pseudo_atomic_atomic(os_context_t * context)
{
    return SymbolValue(PSEUDO_ATOMIC_ATOMIC);
}

void
arch_set_pseudo_atomic_interrupted(os_context_t * context)
{
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(1));
}



/*
 * Installs a breakpoint (INT3) at |pc|.  We return the byte that was
 * replaced by the int3 instruction.
 */
unsigned long
arch_install_breakpoint(void *pc)
{
    unsigned long result = *(unsigned char *) pc;
    *(unsigned char *) pc = BREAKPOINT_INST;

    DPRINTF(debug_handlers,
            (stderr, "arch_install_breakpoint at %p, old code = 0x%lx\n",
             pc, result));

    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    DPRINTF(debug_handlers,
            (stderr, "arch_remove_breakpoint: %p orig %lx\n",
             pc, orig_inst));

    /*
     * Just restore the byte from orig_inst.
     */
    *(unsigned char *) pc = orig_inst & 0xff;
}



/*
 * When single stepping single_stepping holds the original instruction
 * pc location.
 */

unsigned int *single_stepping = NULL;

#ifndef __linux__
unsigned int single_step_save1;
unsigned int single_step_save2;
unsigned int single_step_save3;
#endif

/*
 * This is called when we need to continue after a breakpoint.  This
 * works by putting the original byte back into the code, and then
 * enabling single-step mode to step one instruction.  When we return,
 * the instruction will get run and a sigtrap will get triggered when
 * the one instruction is done.
 *
 * TODO: Be more like other archs where the original inst is put back,
 * and the next inst is replaced with a afterBreakpoint trap.  When we
 * run, the afterBreakpoint trap is hit at the next instruction and
 * then we can put back the original breakpoint and replace the
 * afterBreakpoint trap with the original inst there too.
 *
 * For x86, this means computing how many bytes are used in the
 * current instruction, and then placing an int3 (or maybe ud1) after
 * it.
 */
void
arch_do_displaced_inst(os_context_t * context, unsigned long orig_inst)
{
    unsigned char *pc = (unsigned char *) SC_PC(context);

    DPRINTF(debug_handlers,
            (stderr, "arch_do_displaced_inst: pc %p orig_inst %lx\n",
             pc, orig_inst));
    
    /*
     * Put the original instruction back.
     */

    *pc = orig_inst & 0xff;

    /*
     * If we have the SC_EFLAGS macro, we can enable single-stepping
     * by setting the bit.  Otherwise, we need a more complicated way
     * of enabling single-stepping.
     */
#ifdef SC_EFLAGS
    SC_EFLAGS(context) |= 0x100;
#else

    /*
     * Install helper instructions for the single step:
     *    nop; nop; nop; pushf; or [esp],0x100; popf.
     *
     * The or instruction enables the trap flag which enables
     * single-stepping.  So when the popf instruction is run, we start
     * single-stepping and stop on the next instruction.
     */

    DPRINTF(0, (stderr, "Installing helper instructions\n"));
    
    single_step_save1 = *(pc - 3);
    single_step_save2 = *(pc - 2);
    single_step_save3 = *(pc - 1);
    *(pc - 3) = 0x9c909090;
    *(pc - 2) = 0x00240c81;
    *(pc - 1) = 0x9d000001;
#endif

    single_stepping = (unsigned int *) pc;

#ifndef SC_EFLAGS
    /*
     * pc - 9 points to the pushf instruction that we installed for
     * the helper.
     */
    
    DPRINTF(0, (stderr, " Setting pc to pushf instruction at %p\n", (void*) ((char*) pc - 9)));
    SC_PC(context) = (int)((char *) pc - 9);
#endif
}


/*
 * Handles the ud1 instruction from lisp that is used to signal
 * errors.  In particular, this does not handle the breakpoint traps.
 */
void
sigill_handler(HANDLER_ARGS)
{
    unsigned int trap;
    os_context_t* os_context = (os_context_t *) context;

    DPRINTF(debug_handlers,
            (stderr,"sigill: fp=%lx sp=%lx pc=%lx { %x, %x, %x, %x, %x }\n",
             SC_REG(context, reg_FP),
             SC_REG(context, reg_SP),
             SC_PC(context),
             *((unsigned char*)SC_PC(context) + 0), /* 0x0F */
             *((unsigned char*)SC_PC(context) + 1), /* 0x0B */
             *((unsigned char*)SC_PC(context) + 2),
             *((unsigned char*)SC_PC(context) + 3),
             *((unsigned char*)SC_PC(context) + 4)));

    
    /* This is just for info in case monitor wants to print an approx */
    current_control_stack_pointer = (unsigned long *) SC_SP(os_context);

    /*
     * In many places in the switch below, we eventually throw instead
     * of returning from the signal handler.  So, just in case, set
     * the current FPU modes from the saved context.
     */
    RESTORE_FPU(os_context);

    /*
     * On entry %eip points just to the beginning of the UD1
     * instruction.  For error-trap and cerror-trap a number of bytes
     * will follow, the first is the length of the byte arguments to
     * follow.
     */

    DPRINTF(debug_handlers,
            (stderr, "pc %x\n",  *(unsigned short *)SC_PC(context)));

    /*
     * If the trapping instruction is UD1, assume it's a Lisp trap
     * that we handle here.  Otherwise, just call interrupt_handle_now
     * for other cases.
     */
    if (memcmp((void *)SC_PC(context), ud1, sizeof(ud1)) == 0) {
      /*
       * This must match what the lisp code is doing.  The trap
       * number is placed in the low 6-bits of the 3rd byte of the
       * instruction.
       */
      trap = UD1_CODE(*(((char *)SC_PC(context)) + sizeof(ud1)));

      DPRINTF(debug_handlers, (stderr, "code = %x\n", trap));

      switch (trap) {
      case trap_PendingInterrupt:
        DPRINTF(debug_handlers, (stderr, "<trap Pending Interrupt.>\n"));
        arch_skip_instruction(os_context);
        interrupt_handle_pending(os_context);
        break;

      case trap_Halt:
        {
          FPU_STATE(fpu_state);
          save_fpu_state(fpu_state);

          fake_foreign_function_call(os_context);
          lose("%%primitive halt called; the party is over.\n");
          undo_fake_foreign_function_call(os_context);

          restore_fpu_state(fpu_state);
          arch_skip_instruction(os_context);
          break;
        }

      case trap_Error:
      case trap_Cerror:
        DPRINTF(debug_handlers, (stderr, "<trap Error %x>\n", CODE(code)));
        interrupt_internal_error(signal, code, os_context, CODE(code) == trap_Cerror);
        break;

      case trap_Breakpoint:
        lose("Unexpected breakpoint trap in sigill-hander.\n");
        break;

      case trap_FunctionEndBreakpoint:
        SC_PC(os_context) =
          (int) handle_function_end_breakpoint(signal, CODE(code), os_context);
        break;

#ifdef trap_DynamicSpaceOverflowWarning
      case trap_DynamicSpaceOverflowWarning:
        interrupt_handle_space_overflow(SymbolFunction
                                        (DYNAMIC_SPACE_OVERFLOW_WARNING_HIT),
                                        os_context);
        break;
#endif
#ifdef trap_DynamicSpaceOverflowError
      case trap_DynamicSpaceOverflowError:
        interrupt_handle_space_overflow(SymbolFunction
                                        (DYNAMIC_SPACE_OVERFLOW_ERROR_HIT),
                                        os_context);
        break;
#endif
      default:
        DPRINTF(debug_handlers,
                (stderr, "[C--trap default %d %d %p]\n", signal, CODE(code),
                 os_context));
        interrupt_handle_now(signal, code, os_context);
        break;
      }
    } else {
      interrupt_handle_now(signal, code, os_context);
    }
}

/*
 * Handles the breakpoint trap (int3) and also single-stepping
 */
void
sigtrap_handler(HANDLER_ARGS) 
{
    os_context_t* os_context = (os_context_t *) context;

    DPRINTF(debug_handlers,
            (stderr,"sigtrap: fp=%lx sp=%lx pc=%lx { %x, %x, %x, %x, %x }\n",
             SC_REG(context, reg_FP),
             SC_REG(context, reg_SP),
             SC_PC(context),
             *((unsigned char*)SC_PC(context) + 0), /* 0x0F */
             *((unsigned char*)SC_PC(context) + 1), /* 0x0B */
             *((unsigned char*)SC_PC(context) + 2),
             *((unsigned char*)SC_PC(context) + 3),
             *(unsigned char*)(SC_PC(context) + 4)));

    if (single_stepping) {
        /*
         * We were single-stepping so we now need to disable
         * single-stepping.  We want to put back the breakpoint (int3)
         * instruction so that the next time the breakpoint will be
         * hit again as expected.
         */
	DPRINTF(debug_handlers, (stderr, "* Single step trap %p\n", single_stepping));

#ifdef SC_EFLAGS
	/* Disable single-stepping */
	SC_EFLAGS(os_context) ^= 0x100;
#else
	/* Un-install single step helper instructions. */
	*(single_stepping - 3) = single_step_save1;
	*(single_stepping - 2) = single_step_save2;
	*(single_stepping - 1) = single_step_save3;
        DPRINTF(0, (stderr, "Uninstalling helper instructions\n"));
#endif

	/*
	 * Re-install the breakpoint if possible.
	 */
        DPRINTF(debug_handlers,
                (stderr, "* Maybe reinstall breakpoint for pc %p with single_stepping %p\n",
                 (void*) SC_PC(os_context), single_stepping));
        
        /*
         * Lose if single-stepping didn't move us past where the
         * breakpoint instruction was inserted.
         */
        if ((unsigned long) SC_PC(os_context) <= (unsigned long) single_stepping) {
            lose("Single-stepping did not advance past the breakpoint at %p\n",
                 single_stepping);
        }

        /*
         * Put back the breakpoint since we skipped over it.
         */
        char *ptr = (char *) single_stepping;
        ptr[0] = BREAKPOINT_INST;	/* x86 INT3 */

	single_stepping = NULL;
	return;
    }

    /*
     * We weren't single-stepping, so this we've just hit the breakpoint (int3).  Handle it.
     */
    DPRINTF(debug_handlers, (stderr, "*C break\n"));

    /*
     * The int3 instruction causes a trap that leaves us just after
     * the instruction.  Backup one so we're at the beginning.  This
     * is really important so that when we handle the breakpoint, the
     * offset of the instruction matches where Lisp thinks the
     * breakpoint was placed.
     */
    SC_PC(os_context) -= 1;

    handle_breakpoint(signal, CODE(code), os_context);

    DPRINTF(debug_handlers, (stderr, "*C break return\n"));
}


void
arch_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(SIGILL, sigill_handler);
    interrupt_install_low_level_handler(SIGTRAP, sigtrap_handler);
}


extern lispobj call_into_lisp(lispobj fun, lispobj * args, int nargs);

/* These next four functions are an interface to the 
 * Lisp call-in facility. Since this is C we can know
 * nothing about the calling environment. The control
 * stack might be the C stack if called from the monitor
 * or the Lisp stack if called as a result of an interrupt
 * or maybe even a separate stack. The args are most likely
 * on that stack but could be in registers depending on
 * what the compiler likes. So I try to package up the
 * args into a portable vector and let the assembly language
 * call-in function figure it out.
 */

lispobj
funcall0(lispobj function)
{
    lispobj *args = NULL;

    return call_into_lisp(function, args, 0);
}

lispobj
funcall1(lispobj function, lispobj arg0)
{
    lispobj args[1];

    args[0] = arg0;
    return call_into_lisp(function, args, 1);
}

lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj args[2];

    args[0] = arg0;
    args[1] = arg1;
    return call_into_lisp(function, args, 2);
}

lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj args[3];

    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;
    return call_into_lisp(function, args, 3);
}

#ifdef LINKAGE_TABLE

#ifndef LinkageEntrySize
#define LinkageEntrySize 8
#endif

void
arch_make_linkage_entry(long linkage_entry, void *target_addr, long type)
{
    char *reloc_addr = (char *) (FOREIGN_LINKAGE_SPACE_START

				 + linkage_entry * LinkageEntrySize);

    if (type == 1) {		/* code reference */
	/* Make JMP to function entry. */
	/* JMP offset is calculated from next instruction. */
	long offset = (char *) target_addr - (reloc_addr + 5);
	int i;

	*reloc_addr++ = 0xe9;	/* opcode for JMP rel32 */
	for (i = 0; i < 4; i++) {
	    *reloc_addr++ = offset & 0xff;
	    offset >>= 8;
	}
	/* write a nop for good measure. */
	*reloc_addr = 0x90;
    } else if (type == 2) {
	*(unsigned long *) reloc_addr = (unsigned long) target_addr;
    }
}

/* Make a call to the first function in the linkage table, which is
   resolve_linkage_tramp. */
void
arch_make_lazy_linkage(long linkage_entry)
{
    char *reloc_addr = (char *) (FOREIGN_LINKAGE_SPACE_START

				 + linkage_entry * LinkageEntrySize);
    long offset = (char *) (FOREIGN_LINKAGE_SPACE_START) - (reloc_addr + 5);
    int i;

    *reloc_addr++ = 0xe8;	/* opcode for CALL rel32 */
    for (i = 0; i < 4; i++) {
	*reloc_addr++ = offset & 0xff;
	offset >>= 8;
    }
    /* write a nop for good measure. */
    *reloc_addr = 0x90;
}

/* Get linkage entry.  The initial instruction in the linkage
   entry is a CALL; the return address we're passed points to the next
   instruction. */

long
arch_linkage_entry(unsigned long retaddr)
{
    return ((retaddr - 5) - FOREIGN_LINKAGE_SPACE_START) / LinkageEntrySize;
}
#endif /* LINKAGE_TABLE */
