/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef GLOBALS_H
#define GLOBALS_H

#ifndef LANGUAGE_ASSEMBLY

#include "lisp.h"

extern int foreign_function_call_active;

extern fpu_mode_t fpu_mode;

extern lispobj *current_control_stack_pointer;
extern lispobj *current_control_frame_pointer;

#if !defined(ibmrt) && !defined(i386)
extern lispobj *current_binding_stack_pointer;
#endif

extern lispobj *read_only_space;
extern lispobj *static_space;
extern lispobj *dynamic_0_space;
extern lispobj *dynamic_1_space;
extern unsigned dynamic_space_size;
extern unsigned static_size;
extern lispobj *control_stack;
extern lispobj *binding_stack;

extern unsigned long read_only_space_size;
extern unsigned long binding_stack_size;
extern unsigned long static_space_size;
extern unsigned long control_stack_size;

extern lispobj *control_stack_end;
extern lispobj *current_dynamic_space;

#if !defined(ALLOCATION_POINTER)
extern lispobj *current_dynamic_space_free_pointer;
#endif
#if !defined(ibmrt) && !defined(i386)
extern lispobj *current_auto_gc_trigger;
#endif

extern void globals_init(void);

#else /* LANGUAGE_ASSEMBLY */

/* These are needed by ./assem.s */

#ifdef ppc
#ifdef DARWIN
#define EXTERN(name,bytes) .globl _/**/name
#else
#define EXTERN(name,bytes) .globl _/**/name
#endif
#endif
#ifdef mips
#define EXTERN(name,bytes) .extern name bytes
#endif
#ifdef sparc
#if defined(SVR4) || defined(FEATURE_ELF)
#define EXTERN(name,bytes) .global name
#else
#define EXTERN(name,bytes) .global _ ## name
#endif
#endif
#ifdef ibmrt
#define EXTERN(name,bytes) .globl _/**/name
#endif

#ifdef i386
#ifdef __linux__
#define EXTERN(name,bytes) .globl _/**/name
#else
#define EXTERN(name,bytes) .global _ ## name
#endif
#endif

#if defined(__arm__) && defined(__linux__)
#define EXTERN(name, bytes)
#endif

EXTERN(foreign_function_call_active, 4)

    EXTERN(current_control_stack_pointer, 4)
    EXTERN(current_control_frame_pointer, 4)

#if !defined(ibmrt) && !defined(i386)
    EXTERN(current_binding_stack_pointer, 4)
#if !defined(__arm__)
    EXTERN(current_dynamic_space_free_pointer, 4)
#endif
#endif
#ifdef mips
    EXTERN(current_flags_register, 4)
#endif
#endif /* LANGUAGE_ASSEMBLY */
#endif /* GLOBALS_H */
