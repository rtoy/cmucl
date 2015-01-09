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
    do { \
        fprintf(stderr, "%s: NOT IMPLEMENTED\n", __FUNCTION__); \
        abort(); \
    } while (0)

char *
arch_init(fpu_mode_t mode)
{
    return NULL;
}

os_vm_address_t
arch_get_bad_addr(HANDLER_ARGS)
{
    NOT_IMPLEMENTED();
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
}

boolean
arch_pseudo_atomic_atomic(os_context_t *scp)
{
    NOT_IMPLEMENTED();
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
    NOT_IMPLEMENTED();
}

void
arch_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(SIGILL, sigill_handler);
}


extern lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs);

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

    current_control_stack_pointer -= 1;
    args[0] = arg0;

    return call_into_lisp(function, args, 1);
}

lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer -= 2;
    args[0] = arg0;
    args[1] = arg1;

    return call_into_lisp(function, args, 2);
}

lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer -= 3;
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;

    return call_into_lisp(function, args, 3);
}

