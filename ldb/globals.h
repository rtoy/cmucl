/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/globals.h,v 1.2 1990/05/24 17:46:07 wlott Exp $ */

#if !defined(_INCLUDE_GLOBALS_H_)
#define _INCLUDED_GLOBALS_H_

#include "lisp.h"

#if !defined(LANGUAGE_ASSEMBLY)

extern int foreign_function_call_active;

extern unsigned long saved_global_pointer;

extern lispobj *current_control_stack_pointer;
extern lispobj *current_control_frame_pointer;
extern lispobj *current_binding_stack_pointer;
extern unsigned long current_flags_register;

extern lispobj *read_only_space;
extern lispobj *static_space;
extern lispobj *dynamic_0_space;
extern lispobj *dynamic_1_space;
extern lispobj *control_stack;
extern lispobj *binding_stack;

extern lispobj *current_dynamic_space;
extern lispobj *current_dynamic_space_free_pointer;

#else

/* These are needed by ./assem.s */

.extern foreign_function_call_active 4

.extern current_dynamic_space_free_pointer 4
.extern current_control_stack_pointer 4
.extern current_control_frame_pointer 4
.extern current_binding_stack_pointer 4
.extern current_flags_register 4

#endif

#endif
