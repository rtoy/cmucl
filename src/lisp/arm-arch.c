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

/* 
 * Print out a not-implemented message where "x" is the desired
 * message.  "x" must of of the form "(stderr, <printf-string>, args,
 * ...)".
 */
#define NOT_IMPLEMENTED(x) \
    do {                                                        \
        fprintf(stderr, "%s is not implemented", __FUNCTION__); \
        fprintf x;                                             \
        fprintf(stderr, "\n");                                  \
        abort();                                                \
    } while (0)

char *
arch_init(fpu_mode_t mode)
{
    return NULL;
}

os_vm_address_t
arch_get_bad_addr(HANDLER_ARGS)
{
    NOT_IMPLEMENTED((stderr, "."));
}

void
arch_skip_instruction(os_context_t *context)
{
    NOT_IMPLEMENTED((stderr, "."));
}

unsigned char *
arch_internal_error_arguments(os_context_t *scp)
{
    NOT_IMPLEMENTED((stderr, "."));
}

boolean
arch_pseudo_atomic_atomic(os_context_t *scp)
{
    NOT_IMPLEMENTED((stderr, "."));
}

void
arch_set_pseudo_atomic_interrupted(os_context_t *scp)
{
    NOT_IMPLEMENTED((stderr, "."));
}

unsigned long
arch_install_breakpoint(void *pc)
{
    NOT_IMPLEMENTED((stderr, ": addr: %p", pc));
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    NOT_IMPLEMENTED((stderr, ":addr %p", pc));
}

static unsigned int *skipped_break_addr, displaced_after_inst;

static sigset_t orig_sigmask;

void
arch_do_displaced_inst(os_context_t *scp, unsigned long orig_inst)
{
    NOT_IMPLEMENTED((stderr, ": orig: %08lx", orig_inst));
}

static void
sigill_handler(HANDLER_ARGS)
{
    os_context_t *os_context = context;

    if (CODE(code) == ILL_ILLOPC) {
        int udf_code;
        unsigned int inst;
        unsigned int *pc = (unsigned int *) (SC_PC(os_context));

        inst = pc[0];
        udf_code = (inst & 0xf) | ((inst >> 8) & 0xfff);

        switch (udf_code) {
          case trap_NotImplemented: {
              /*
               * Print out the name.  The next instruction MUST be
               * a branch immediate.
               */
              unsigned char *string;
              int length;

              if (((pc[1] >> 24) & 0xf) != 0xa) {
                  fprintf(stderr, "ERROR: NOT-IMPLEMENTED trap not followed relative branch: 0x%08x\n",
                          pc[1]);
                  abort();
              }

              /*
               * Compute the maximum length of the string from the
               * offset in the branch instruction.  Then try to
               * find the last nul character for end of the
               * string.
               */
              string = (unsigned char *) &pc[2];
              length = ((pc[1] & 0xffffff) << 2) + 4;

              while (string[length - 1] == '\0') {
                  --length;
              }

              /*
               * Don't want to use NOT_IMPLEMENTED here because we
               * don't actually want to abort.  We want to continue,
               * but print out a useful message.
               */
              printf("NOT-IMPLEMENTED: %p: \"%.*s\"\n", pc, length, (char*)(pc + 2));

              /*
               * Skip over the UDF instruction so if we can
               * continue.  This will execute the branch, skipping
               * over the string too.
               */
              SC_PC(context) = (unsigned long) (pc + 1);
              break;
          }
          default:
              NOT_IMPLEMENTED((stderr, ": udf code: %d", udf_code));
        }
    } else {
        NOT_IMPLEMENTED((stderr, ": unknown CODE: %d", CODE(code)));
    }
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
    *--current_control_stack_pointer = arg0;
    
    return call_into_lisp(function, current_control_stack_pointer, 1);
}

lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    *--current_control_stack_pointer = arg1;
    *--current_control_stack_pointer = arg0;

    return call_into_lisp(function, current_control_stack_pointer, 2);
}

lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    *--current_control_stack_pointer = arg2;
    *--current_control_stack_pointer = arg1;
    *--current_control_stack_pointer = arg0;

    return call_into_lisp(function, current_control_stack_pointer, 3);
}

