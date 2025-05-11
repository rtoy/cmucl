/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/
/* Variables everybody needs to look at or frob on. */

#include <stdio.h>

#include "lisp.h"
#include "internals.h"
#include "globals.h"

int foreign_function_call_active;

lispobj *current_control_stack_pointer;
lispobj *current_control_frame_pointer;

#ifndef BINDING_STACK_POINTER
lispobj *current_binding_stack_pointer;
#endif

lispobj *read_only_space;
lispobj *static_space;
lispobj *dynamic_0_space;
lispobj *dynamic_1_space;
unsigned dynamic_space_size;
lispobj *control_stack;
lispobj *control_stack_end;
lispobj *binding_stack;

lispobj *current_dynamic_space;

#ifndef ALLOCATION_POINTER
lispobj *current_dynamic_space_free_pointer;
#endif
#ifndef INTERNAL_GC_TRIGGER
lispobj *current_auto_gc_trigger;
#endif

unsigned long read_only_space_size;
unsigned long binding_stack_size;
unsigned long static_space_size;
unsigned long control_stack_size;


void
globals_init(void)
{
    /* Space, stack, and free pointer vars are initialized by
       validate() and coreparse(). */

#ifndef INTERNAL_GC_TRIGGER
    /* No GC trigger yet */
    current_auto_gc_trigger = NULL;
#endif

    /* Set foreign function call active. */
    foreign_function_call_active = 1;

    /* Initialize the current lisp state. */
#if !(defined(i386) || defined(__x86_64) || defined(__arm__))
    current_control_stack_pointer = control_stack;
#elif defined(__arm__)
    /*
     * On ARM, we want the Lisp control stack to grow down.
     * control_stack_end points at the top of the control stack on
     * ARM, which is just past the end of the control stack space, and
     * this isn't writable.  Back up a word so that the control stack
     * pointer points inside the control stack space.
     */
    current_control_stack_pointer = control_stack_end - 1;
#else
    current_control_stack_pointer = control_stack_end;
#endif

    current_control_frame_pointer = (lispobj *) 0;
#ifndef BINDING_STACK_POINTER
    current_binding_stack_pointer = binding_stack;
#endif
}
