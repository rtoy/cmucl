/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/globals.c,v 1.2 1990/05/24 17:46:05 wlott Exp $ */

/* Variables everybody needs to look at or frob on. */

#include "lisp.h"
#include "globals.h"

int foreign_function_call_active;

unsigned long saved_global_pointer;

lispobj *current_control_stack_pointer;
lispobj *current_control_frame_pointer;
lispobj *current_binding_stack_pointer;
unsigned long current_flags_register;

lispobj *read_only_space;
lispobj *static_space;
lispobj *dynamic_0_space;
lispobj *dynamic_1_space;
lispobj *control_stack;
lispobj *binding_stack;

lispobj *current_dynamic_space;
lispobj *current_dynamic_space_free_pointer;

globals_init()
{
	/* Space, stack, and free pointer vars are initialized by
	   validate() and coreparse(). */

	/* Get the current value of GP. */
	saved_global_pointer = current_global_pointer();

	/* Set foreign function call active. */
	foreign_function_call_active = 1;

	/* Initialize the current lisp state. */
	current_control_stack_pointer = control_stack;
	current_control_frame_pointer = (lispobj *)0;
	current_binding_stack_pointer = binding_stack;
	current_flags_register = 1<<flag_Atomic;
}
