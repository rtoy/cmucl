/*
 * This code written as part of the CMUCL project and has been placed
 * in the public domain.
 */

#include <stdio.h>

#include "arch.h"
#include "lisp.h"
#include "internals.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "lispregs.h"
#include "signal.h"
#include "interrupt.h"
#include "gencgc.h"
#include "breakpoint.h"
#include "interr.h"

#define NOT_IMPLEMENTED() \
    fprintf(stderr, "%s: NOT IMPLEMENTED\n", __FUNCTION__)

char *
arch_init(fpu_mode_t mode)
{
    return 0;
}

os_vm_address_t
arch_get_bad_addr(HANDLER_ARGS)
{
    NOT_IMPLEMENTED();
    return 0;
}

void
arch_skip_instruction(os_context_t *context)
{
    NOT_IMPLEMENTED();
}

unsigned char *
arch_internal_error_arguments(os_context_t *scp)
{
    NOT_IMPLEMENTED();
    return NULL;
}

boolean
arch_pseudo_atomic_atomic(os_context_t *scp)
{
    NOT_IMPLEMENTED();
    return 0;
}

void
arch_set_pseudo_atomic_interrupted(os_context_t *scp)
{
    NOT_IMPLEMENTED();
}

unsigned long
arch_install_breakpoint(void *pc)
{
    NOT_IMPLEMENTED();
    return 0;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    NOT_IMPLEMENTED();
}

static unsigned int *skipped_break_addr, displaced_after_inst;

static sigset_t orig_sigmask;

void
arch_do_displaced_inst(os_context_t *scp, unsigned long orig_inst)
{
    NOT_IMPLEMENTED();
}

/*
 * How to identify an illegal instruction trap and a trap instruction
 * trap.
 */
static void
sigill_handler(HANDLER_ARGS)
{
    os_context_t *os_context = (os_context_t *) context;
    
    SAVE_CONTEXT();

    /*
     * Do we really want to have the same signals as in the context?
     * This would typically enable all signals, I think.  But there
     * are comments in interrupt_handle_now that says we want to
     * alloc_sap while interrupts are disabled.  The interrupt
     * handlers that eventually get called from here will re-enable
     * interrupts at the appropriate time, so we don't do anything
     * here.
     *
     * (I'm guessing here.  I don't really know if this is right or
     * not, but it doesn't seem to cause harm, and it does seem to be
     * a bad idea to have interrupts enabled here.)
     */
#if 0
    sigprocmask(SIG_SETMASK, &os_context->uc_sigmask, 0);
#endif

    if (CODE(code) == 0) {
	int illtrap_code;
	unsigned int inst;
	unsigned int *pc = (unsigned int *) (SC_PC(os_context));

	inst = *pc;

	illtrap_code = inst & 0x3fffff;

	switch (illtrap_code) {
	  case trap_PendingInterrupt:
	      arch_skip_instruction(os_context);
	      interrupt_handle_pending(os_context);
	      break;

	  case trap_Halt:
	      fake_foreign_function_call(os_context);
	      lose("%%primitive halt called; the party is over.\n");

	  case trap_Error:
	  case trap_Cerror:
	      interrupt_internal_error(signal, code, os_context,
				       illtrap_code == trap_Cerror);
	      break;

	  case trap_Breakpoint:
	      handle_breakpoint(signal, CODE(code), os_context);
	      break;

	  case trap_FunctionEndBreakpoint:
	      SC_PC(os_context) =
		  (long) handle_function_end_breakpoint(signal, CODE(code),
							os_context);
	      break;

	  case trap_AfterBreakpoint:
	      *skipped_break_addr = trap_Breakpoint;
	      skipped_break_addr = NULL;
	      *(unsigned long *) SC_PC(os_context) = displaced_after_inst;
	      os_context->uc_sigmask = orig_sigmask;
	      os_flush_icache((os_vm_address_t) SC_PC(os_context),

			      sizeof(unsigned long));
	      break;

#ifdef trap_DynamicSpaceOverflowWarning
	  case trap_DynamicSpaceOverflowWarning:
	      arch_skip_instruction(os_context);
	      interrupt_handle_space_overflow(SymbolFunction
					      (DYNAMIC_SPACE_OVERFLOW_WARNING_HIT),
					      os_context);
	      break;
#endif
#ifdef trap_DynamicSpaceOverflowError
	  case trap_DynamicSpaceOverflowError:
	      arch_skip_instruction(os_context);
	      interrupt_handle_space_overflow(SymbolFunction
					      (DYNAMIC_SPACE_OVERFLOW_ERROR_HIT),
					      os_context);
	      break;
#endif
	  default:
	      interrupt_handle_now(signal, code, os_context);
	      break;
	}
    } else {
	interrupt_handle_now(signal, code, os_context);
    }
}

void
arch_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(SIGILL, sigill_handler);
}


extern lispobj call_into_lisp(lispobj fun, lispobj * args, int nargs);

lispobj
funcall0(lispobj function)
{
    lispobj *args = current_control_stack_pointer;

    return call_into_lisp(function, args, 0);
}

lispobj
funcall1(lispobj function, lispobj arg0)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 1;
    args[0] = arg0;

    return call_into_lisp(function, args, 1);
}

lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 2;
    args[0] = arg0;
    args[1] = arg1;

    return call_into_lisp(function, args, 2);
}

lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 3;
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;

    return call_into_lisp(function, args, 3);
}

