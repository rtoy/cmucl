/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/globals.c,v 1.8 1991/10/22 18:38:02 wlott Exp $ */

/* Variables everybody needs to look at or frob on. */

#include <stdio.h>

#include "lisp.h"
#include "ldb.h"
#include "globals.h"

char *number_stack_start;

int foreign_function_call_active;

#ifdef mips
unsigned long saved_global_pointer;
unsigned long current_flags_register;
#endif

lispobj *current_control_stack_pointer;
lispobj *current_control_frame_pointer;
#ifndef ibmrt
lispobj *current_binding_stack_pointer;
#endif

lispobj *read_only_space;
lispobj *static_space;
lispobj *dynamic_0_space;
lispobj *dynamic_1_space;
lispobj *control_stack;
lispobj *binding_stack;

lispobj *current_dynamic_space;
#ifndef ibmrt
lispobj *current_dynamic_space_free_pointer;
lispobj *current_auto_gc_trigger;
#endif

globals_init()
{
	/* Space, stack, and free pointer vars are initialized by
	   validate() and coreparse(). */

#ifdef mips
	/* Get the current value of GP. */
	saved_global_pointer = current_global_pointer();

        /* Set the Atomic flag */
	current_flags_register = 1<<flag_Atomic;
#endif

        /* No GC trigger yet */
        current_auto_gc_trigger = NULL;

	/* Set foreign function call active. */
	foreign_function_call_active = 1;

	/* Initialize the current lisp state. */
	current_control_stack_pointer = control_stack;
	current_control_frame_pointer = (lispobj *)0;
#ifndef ibmrt
	current_binding_stack_pointer = binding_stack;
#endif
}
