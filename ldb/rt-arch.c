#include <stdio.h>
#include <mach.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/mman.h>

#include "ldb.h"
#include "lisp.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "arch.h"
#include "lispregs.h"
#include "signal.h"

#define FL_NONE 0
#define FL_FPA 2
#define FL_AFPA 4
#define FL_MC68881 1

#define READ_MC68881_FPCR 0xfc3ec001
#define WRITE_MC68881_FPCR 0xfc024001
#define MC68881_ZIC 0xfc1c0003
#define MC68881_PREC_MASK 0xff3f
#define MC68881_DOUBLE_PREC 0x0080

/* This is only used during arch_init to find out what kind of FP we have. */
static sigsys_handler(signal, code, scp)
int signal, code;
struct sigcontext *scp;
{
    return (-1);
}

char *arch_init()
{
    int hw, i, rc;
    int fs[5];
    struct sigvec sv, osv;

    /* Set up the state of the floating point hardware.  On the APC, there is
       the onboard MC68881 which must be dealt with properly.  The signal
       handling is necessary to be compatible with facilities version
       of Mach.  */

    sv.sv_handler = sigsys_handler;
    sv.sv_mask = sigmask(SIGSYS);
    sv.sv_onstack = FALSE;
    rc = sigvec(SIGSYS, &sv, &osv);
    i = 20;
    rc = syscall(167, fs, &i);
    if (rc == 0) {
	i = fs[0];
	hw = i & 7;
	if (i & 1) {
	    *(int *)READ_MC68881_FPCR = (int) &i;
	    *(int *)MC68881_ZIC = 0;
	    i = ((i & MC68881_PREC_MASK) | MC68881_DOUBLE_PREC);
	    *(int *)WRITE_MC68881_FPCR = (int) &i;
	    if (i & 2)
		hw = hw & (~2);
	}
    }
    else
	hw = FL_FPA;

    sigvec(SIGSYS, &osv, &sv);

    switch (hw) {
      case FL_FPA:
	fprintf(stderr, "CMUCL only supports the AFPA and the MC68881 floating point options.\n");
	exit(1);
      case FL_AFPA:
	return "eapc.core";
      case FL_MC68881:
	return NULL;
      default:
	fprintf(stderr, "CMUCL only supports the AFPA and the MC68881 floating point options.\nUse a different machine.\n");
	exit(1);
    }
}

os_vm_address_t arch_get_bad_addr(context)
struct sigcontext *context;
{
    return 0;
}

void arch_skip_instruction(context)
struct sigcontext *context;
{
    unsigned long pc = context->sc_iar;
    unsigned long prev_inst = ((unsigned long *)pc)[-1];

    if ((prev_inst >> 24) == 0x89) {
	short offset = prev_inst & 0xffff;
	context->sc_pc = pc - 4 + offset;
    }
    else
	context->sc_pc = pc + 4;
}

static sigtrap_handler(signal, code, context)
     int signal, code;
     struct sigcontext *context;
{
    switch (*((char *)context->sc_iar)) {
      case 0xCC:
	/* trap-immediate instruction. */
	switch (*((unsigned short *)context->sc_iar+1)) {
	  case trap_PendingInterrupt:
	    interrupt_handle_pending(context);
	    break;

	  case trap_Halt:
	    crap_out("%primitive halt called; the party is over.\n");

	  case trap_Error:
	  case trap_Cerror:
	    interrupt_internal_error(signal, code, context, code==trap_Cerror);
	    break;

	  case trap_Breakpoint:
	    sigsetmask(context->sc_mask);
	    fake_foreign_function_call(context);
	    handle_breakpoint(signal, code, context);
	    undo_fake_foreign_function_call(context);
	    break;

	  case trap_FunctionEndBreakpoint:
	    sigsetmask(context->sc_mask);
	    fake_foreign_function_call(context);
	    handle_function_end_breakpoint(signal, code, context);
	    undo_fake_foreign_function_call(context);
	    break;

	  default:
	    interrupt_handle_now(signal, code, context);
	    break;
	}
	break;

      case 0xBE:
	/* Trap Less Than instruction. */
	if (SymbolValue(ALLOCATION_POINTER)>SymbolValue(INTERNAL_GC_TRIGGER)) {
	    SetSymbolValue(INTERNAL_GC_TRIGGER, fixnum(-1));
	    interrupt_maybe_gc(context);
	    context->sc_iar += 2;
	}
	else
	    interrupt_handle_now(signal, code, context);
	break;

      default:
	interrupt_handle_now(signal, code, context);
	break;
    }
}


void arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGTRAP,sigtrap_handler);
}
