#include <machine/trap.h>

#include "ldb.h"
#include "lisp.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "arch.h"
#include "lispregs.h"
#include "signal.h"

char *arch_init()
{
    return NULL;
}

os_vm_address_t arch_get_bad_addr(context)
struct sigcontext *context;
{
    unsigned long badinst;
    int rs1;

    /* On the sparc, we have to decode the instruction. */

    /* Make sure it's not the pc thats bogus, and that it was lisp code */
    /* that caused the fault. */
    if ((context->sc_pc & 3) != 0 ||
	((context->sc_pc < READ_ONLY_SPACE_START ||
	  context->sc_pc >= READ_ONLY_SPACE_START+READ_ONLY_SPACE_SIZE) &&
	 ((lispobj *)context->sc_pc < current_dynamic_space &&
	  (lispobj *)context->sc_pc >=
	      current_dynamic_space + DYNAMIC_SPACE_SIZE)))
	return NULL;

    badinst = *(unsigned long *)context->sc_pc;

    if ((badinst >> 30) != 3)
	/* All load/store instructions have op = 11 (binary) */
	return NULL;

    rs1 = (badinst>>14)&0x1f;

    if (badinst & (1<<13)) {
	/* r[rs1] + simm(13) */
	int simm13 = badinst & 0x1fff;

	if (simm13 & (1<<12))
	    simm13 |= -1<<13;

	return (os_vm_address_t)(context->sc_regs[rs1] + simm13);
    }
    else {
	/* r[rs1] + r[rs2] */
	int rs2 = badinst & 0x1f;

	return (os_vm_address_t)(context->sc_regs[rs1] + context->sc_regs[rs2]);
    }

}

void arch_skip_instruction(context)
struct sigcontext *context;
{
    /* Skip the offending instruction */
    context->sc_pc = context->sc_npc;
    context->sc_npc += 4;
}

static void sigill_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    int badinst;

    if (code == T_UNIMP_INSTR) {
	int trap;

	trap = *(unsigned long *)(context->sc_pc) & 0x3fffff;

	switch (trap) {
	  case trap_PendingInterrupt:
	    interrupt_handle_pending(context);
	    break;

	  case trap_Halt:
	    crap_out("%primitive halt called; the party is over.\n");

	  case trap_Error:
	  case trap_Cerror:
	    interrupt_internal_error(signal, code, context, trap == trap_Cerror);
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
	    context->sc_npc = context->sc_pc + 4;
	    break;

	  default:
	    interrupt_handle_now(signal, code, context);
	    break;
	}
    }
    else if (code >= T_SOFTWARE_TRAP + 16 & code < T_SOFTWARE_TRAP + 32)
	interrupt_internal_error(signal, code, context, FALSE);
    else
	interrupt_handle_now(signal, code, context);
}

static void sigemt_handler(signal, code, context)
     int signal, code;
     struct sigcontext *context;
{
    unsigned long badinst;
    boolean subtract, immed;
    int rd, rs1, op1, rs2, op2, result;

    badinst = *(unsigned long *)context->sc_pc;
    if ((badinst >> 30) != 2 || ((badinst >> 20) & 0x1f) != 0x11) {
	/* It wasn't a tagged add.  Pass the signal into lisp. */
	interrupt_handle_now(signal, code, context);
	return;
    }
	
    /* Extract the parts of the inst. */
    subtract = badinst & (1<<19);
    rs1 = (badinst>>14) & 0x1f;
    op1 = context->sc_regs[rs1];

    /* If the first arg is $ALLOC then it is really a signal-pending note */
    /* for the pseudo-atomic noise. */
    if (rs1 == ALLOC) {
	/* Perform the op anyway. */
	op2 = badinst & 0x1fff;
	if (op2 & (1<<12))
	    op2 |= -1<<13;
	if (subtract)
	    result = op1 - op2;
	else
	    result = op1 + op2;
	context->sc_regs[ALLOC] = result;
	interrupt_handle_pending(context);
	return;
    }

    if ((op1 & 3) != 0) {
	/* The first arg wan't a fixnum. */
	interrupt_internal_error(signal, code, context, FALSE);
	return;
    }

    if (immed = badinst & (1<<13)) {
	op2 = badinst & 0x1fff;
	if (op2 & (1<<12))
	    op2 |= -1<<13;
    }
    else {
	rs2 = badinst & 0x1f;
	op2 = context->sc_regs[rs2];
    }

    if ((op2 & 3) != 0) {
	/* The second arg wan't a fixnum. */
	interrupt_internal_error(signal, code, context, FALSE);
	return;
    }

    rd = (badinst>>25) & 0x1f;
    if (rd != 0) {
	/* Don't bother computing the result unless we are going to use it. */
	if (subtract)
	    result = (op1>>2) - (op2>>2);
	else
	    result = (op1>>2) + (op2>>2);

        current_dynamic_space_free_pointer =
            (lispobj *) context->sc_regs[ALLOC];

	context->sc_regs[rd] = alloc_number(result);

	context->sc_regs[ALLOC] =
	    (unsigned long) current_dynamic_space_free_pointer;
    }

    arch_skip_instruction(context);
}

void arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGILL,sigill_handler);
    interrupt_install_low_level_handler(SIGEMT,sigemt_handler);
}
