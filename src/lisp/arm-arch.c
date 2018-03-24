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

char *
arch_init(fpu_mode_t mode)
{
    return NULL;
}

os_vm_address_t
arch_get_bad_addr(HANDLER_ARGS)
{
    lose("NOT IMPLEMENTED: %s\n", __FUNCTION__);
    return NULL;
}

void
arch_skip_instruction(os_context_t *context)
{
    lose("NOT IMPLEMENTED: %s\n", __FUNCTION__);
}

unsigned char *
arch_internal_error_arguments(os_context_t *scp)
{
    lose("NOT IMPLEMENTED: %s\n", __FUNCTION__);
    return NULL;
}

boolean
arch_pseudo_atomic_atomic(os_context_t *scp)
{
    lose("NOT IMPLEMENTED: %s\n", __FUNCTION__);
    return 0;
}

void
arch_set_pseudo_atomic_interrupted(os_context_t *scp)
{
    lose("NOT IMPLEMENTED: %s\n", __FUNCTION__);
}

unsigned long
arch_install_breakpoint(void *pc)
{
    lose("NOT IMPLEMEMTED: %s: addr: %p\n", __FUNCTION__, pc);
    return 0;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    lose("NOT IMPLEMENTED: %s: addr %p\n", __FUNCTION__, pc);
}

static unsigned int *skipped_break_addr, displaced_after_inst;

static sigset_t orig_sigmask;

void
arch_do_displaced_inst(os_context_t *scp, unsigned long orig_inst)
{
    lose("NOT IMPLEMENTED: %s: orig: %08x\n", __FUNCTION__, orig_inst);
}

static void
sigill_handler(HANDLER_ARGS)
{
    fprintf(stderr, "sigill: signal = %d, code = %p, context = %p\n",
            signal, code, context);
    
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
                  lose("ERROR: NOT-IMPLEMENTED trap not followed relative branch: 0x%08x\n",
                       pc[1]);
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
            lose("Unknown udf code: %d at %p (inst = %8x)\n", udf_code,
                 pc, inst);
        }
    } else {
        lose("Unknown CODE: %d\n", CODE(code));
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
    lispobj *args = current_control_stack_pointer;

    /* The control stack grows down on ARM. */
    current_control_stack_pointer -= 1;

    args[0] = arg0;

    return call_into_lisp(function, args, 1);
}

lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj *args = current_control_stack_pointer;

    /* The control stack grows down on ARM. */
    current_control_stack_pointer -= 2;

    args[0] = arg0;
    args[-1] = arg1;

    return call_into_lisp(function, args, 2);
}

lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer -= 3;

    args[0] = arg0;
    args[-1] = arg1;
    args[-2] = arg2;

    return call_into_lisp(function, args, 3);
}

