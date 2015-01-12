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
 * How to identify an illegal instruction trap
 */
static void
sigill_handler(HANDLER_ARGS)
{
    os_context_t *os_context = (os_context_t *) context;

    if (CODE(code) == ILL_ILLOPC) {
        int udf_code;
        unsigned int inst;
        unsigned int *pc = (unsigned int *) (SC_PC(os_context));

        inst = *pc;
        udf_code = (inst & 0xf) | ((inst >> 8) & 0xfff);

        switch (udf_code) {
          case trap_NotImplemented:
              {
                  /*
                   * Print out the name.  The next instruction MUST be
                   * a branch immediate.
                   */
                  int offset;
                  unsigned char *string;
                  unsigned char *string_end;
                  unsigned int binst = pc[1];
                  if (((binst >> 24) & 0xf) != 0xa) {
                      fprintf(stderr, "ERROR: NOT-IMPLEMENTED trap not followed relative branch: 0x%08x\n",
                              binst);
                      abort();
                  }
                  string = (unsigned char *) &pc[2];
                  offset = binst & 0xffffff;
                  /*
                   * Find the possible end of the string based on the
                   * offset of the branch instruction.
                   */
                  string_end = (string + ((offset << 2) + 4));

                  /*
                   * Print out the characters, being careful not to go
                   * too far.
                   */

                  printf("NOT-IMPLEMENTED: \"");
                  while (*string != '\0' && (string < string_end)) {
                      putchar(*string);
                      ++string;
                  }
                  printf("\"\n");
                  /*
                   * FIXME: What should we do after printing this message?
                   */
                  /*
                   * Skip over the UDF instruction so if we continue,
                   * we'll execute the branch, skipping over the
                   * string.
                   */
                  SC_PC(context) = pc + 1;
              }
              break;
          default:
              NOT_IMPLEMENTED();
        }
    } else {
        NOT_IMPLEMENTED();
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

