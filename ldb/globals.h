/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/globals.h,v 1.6 1990/10/23 00:04:36 wlott Exp $ */

#if !defined(_INCLUDE_GLOBALS_H_)
#define _INCLUDED_GLOBALS_H_

#include "lisp.h"

#ifndef LANGUAGE_ASSEMBLY

extern char *number_stack_start;

extern int foreign_function_call_active;

#ifdef mips
extern unsigned long saved_global_pointer;
extern unsigned long current_flags_register;
#endif

extern lispobj *current_control_stack_pointer;
extern lispobj *current_control_frame_pointer;
extern lispobj *current_binding_stack_pointer;

extern lispobj *read_only_space;
extern lispobj *static_space;
extern lispobj *dynamic_0_space;
extern lispobj *dynamic_1_space;
extern lispobj *control_stack;
extern lispobj *binding_stack;

extern lispobj *current_dynamic_space;
extern lispobj *current_dynamic_space_free_pointer;
extern lispobj *current_auto_gc_trigger;

#else  LANGUAGE_ASSEMBLY

/* These are needed by ./assem.s */

#ifdef mips
#define EXTERN(name,bytes) .extern name bytes
#endif
#ifdef sparc
#define EXTERN(name,bytes) .global _/**/name
#endif

EXTERN(foreign_function_call_active, 4)

EXTERN(current_dynamic_space_free_pointer, 4)
EXTERN(current_control_stack_pointer, 4)
EXTERN(current_control_frame_pointer, 4)
EXTERN(current_binding_stack_pointer, 4)

#ifdef mips
EXTERN(current_flags_register, 4)
#endif

#endif LANGUAGE_ASSEMBLY

#endif _INCLUDED_GLOBALS_H_
