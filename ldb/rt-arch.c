#include "ldb.h"
#include "lisp.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "arch.h"
#include "lispregs.h"
#include "signal.h"

void arch_init()
{
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
}


void arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGTRAP,sigtrap_handler);
}
