#include <mips/cpu.h>

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
    /* Finding the bad address on the mips is easy. */
    return (os_vm_address_t)context->sc_badvaddr;
}

void arch_skip_instruction(context)
struct sigcontext *context;
{
    /* Skip the offending instruction */
    if (context->sc_cause & CAUSE_BD)
        emulate_branch(context, *(unsigned long *)context->sc_pc);
    else
        context->sc_pc += 4;
}

static void sigtrap_handler(signal, code, context)
     int signal, code;
     struct sigcontext *context;
{
    switch (code) {
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

#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

static void sigfpe_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    unsigned long bad_inst;
    unsigned int op, rs, rt, rd, funct, dest;
    int immed;
    long result;

    if (context->sc_cause & CAUSE_BD)
        bad_inst = *(unsigned long *)(context->sc_pc + 4);
    else
        bad_inst = *(unsigned long *)(context->sc_pc);

    op = (bad_inst >> 26) & 0x3f;
    rs = (bad_inst >> 21) & 0x1f;
    rt = (bad_inst >> 16) & 0x1f;
    rd = (bad_inst >> 11) & 0x1f;
    funct = bad_inst & 0x3f;
    immed = (((int)(bad_inst & 0xffff)) << 16) >> 16;

    switch (op) {
        case 0x0: /* SPECIAL */
            switch (funct) {
                case 0x20: /* ADD */
                    result = FIXNUM_VALUE(context->sc_regs[rs]) + FIXNUM_VALUE(context->sc_regs[rt]);
                    dest = rd;
                    break;

                case 0x22: /* SUB */
                    result = FIXNUM_VALUE(context->sc_regs[rs]) - FIXNUM_VALUE(context->sc_regs[rt]);
                    dest = rd;
                    break;

                default:
                    dest = 32;
                    break;
            }
            break;

        case 0x8: /* ADDI */
            result = FIXNUM_VALUE(context->sc_regs[rs]) + (immed>>2);
            dest = rt;
            break;

        default:
            dest = 32;
            break;
    }

    if (dest < 32) {
        set_global_pointer(saved_global_pointer);
        current_dynamic_space_free_pointer =
            (lispobj *) context->sc_regs[ALLOC];

        context->sc_regs[dest] = alloc_number(result);

	context->sc_regs[ALLOC] =
	    (unsigned long) current_dynamic_space_free_pointer;

        arch_skip_instruction(context);

    }
    else
        interrupt_handle_now(signal, code, context);
}

void arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGTRAP,sigtrap_handler);
    interrupt_install_low_level_handler(SIGEMT,sigfpe_handler);
}
