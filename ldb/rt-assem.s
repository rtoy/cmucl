	.globl	.oVncs
	.set	.oVncs,0

#define LANGUAGE_ASSEMBLY
#include "lispregs.h"
#include "lisp.h"
#include "globals.h"

	.text
	.data	1
	.globl	_call_into_lisp
_call_into_lisp:
	.long	_.call_into_lisp
	.text
	.align	1
	.globl	_.call_into_lisp
_.call_into_lisp:
/* Save the callee-saves registers. */
	stm   	r6,-64(r1)
/* Put the pointer to our data area where we can use it. */
	mr    	r14,r0
/* Move the callers stack pointer into our frame pointer. */
	mr    	r13,r1
/* And allocate our frame on the stack. */
	cal   	r1,-64(r1)

/* Set up the lisp state. */
	mr	CNAME, r2
	mr	LEXENV, r3
	mr	CFP, r4
	mr	NARGS, r5
	sli	NARGS, 2
	get	NULLREG, $NIL

/* No longer in foreign function call. */
/* Note: the atomic flag should still be set. */
	lis	A0, 0
	store	A0, _foreign_function_call_active, NL0

/* Load the rest of lisp state. */
	load	CSP, _current_control_stack_pointer
	load	OCFP, _current_control_frame_pointer

/* No longer atomic. */
	store	A0, PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET, NL0

/* Were we interrupted? */
	load	NL0, PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET
	ci	NL0, 0
	je	1f

	ti	7, r0, trap_PendingInterrupt
1:

/* Load the args. */
	l	A0, 0(CFP)
	l	A1, 4(CFP)
	l	A2, 8(CFP)

/* Calculate LRA. */
	get	LRA, $lra+type_OtherPointer

/* Indirect closure */
	l	CODE, CLOSURE_FUNCTION_OFFSET(LEXENV)

	cal	LIP, FUNCTION_HEADER_CODE_OFFSET(CODE)
	br	LIP

	.align	3
lra:
	.long	type_ReturnPcHeader

/* Blow off any extra values. */
	cal	CSP, 0(OCFP)	/* We must use a 32-bit instruction here */

/* Save the return value. */
	mr	r2, A0

/* Turn on pseudo-atomic */
	store	CFP, PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET, r3

/* Store lisp state */
	store	CSP, _current_control_stack_pointer, r3
	store	CFP, _current_control_frame_pointer, r3

/* No longer in lisp. */
	store	CFP, _foreign_function_call_active, r3

/* Were we interrupted? */
	load	r3, PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET
	ci	r3, 0
	je	1f

/* Yep. */
	ti	7, r0, trap_PendingInterrupt

1:
/* Restore callee-saves registers. */
	lm	r6,0(r1)
/* Return to C */
	brx	r15
/* And reset the stack. */
	ai	r1,64

/* Noise to keep debuggers happy. */
	.long	0xDF07DF68
	.short	0x0110


/* Call_into_C

On entry:
 NL0 - addr of fn to call
 NSP[0]...NSP[n-1] - args
 LRA, CODE - must be preserved (and GCed if necessary)

*/

	.text
	.globl	call_into_c
call_into_c:
	/* Build a stack frame. */
	mr	OCFP, CFP
	mr	CFP, CSP
	cal	CSP, 32(CSP)
	st	OCFP, 0(CFP)
	st	LRA, 4(CFP)
	st	CODE, 8(CFP)
	
	/* Get the text addr we are supposed to jump to */
	l	r15, 0(NL0)
	mr	r0, NL0

	/* Set pseudo-atomic. */
	store	CFP, PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET, r3

	/* Save lisp state. */
	store	CSP, _current_control_stack_pointer, r2
	store	CFP, _current_control_frame_pointer, r2

	/* Now in foreign function call land */
	store	CFP, _foreign_function_call_active, r2

	/* Were we interrupted? */
	load	r3, PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET
	ci	r3, 0
	je	1f

/* Yep. */
	ti	7, r0, trap_PendingInterrupt

1:
	/* Get the first 4 args, and adjust the stack pointer.  We need to */
	/* adjust the stack pointer because r1 is supposed to point at the */
	/* 5th argument, not the 1st.  This is easier than trying to fix */
	/* pack to be able to deal with TNs with a negative offset. */
	l	r2, 0(r1)
	l	r3, 4(r1)
	l	r4, 8(r1)
	l	r5, 12(r1)
	cal	r1, 16(r1)

	/* And hit it. */
	balr	r15, r15

	/* Save the second return value (assuming there is one) */
	mr	NARGS, OCFP

	/* Clear desriptor regs.  We have to do this before we clear the
	foreign-function-call-active flag even if we are just going to
	load the saved value back into the reg to make sure we don't keep
	ahold of any pointers that have been moved by GC. */
	lis	CODE, 0
	lis	CNAME, 0
	lis	LEXENV, 0
	lis	LRA, 0
	lis	A0, 0
	lis	A1, 0
	lis	A2, 0

/* No longer in foreign function call. */
/* Note: the atomic flag should still be set. */
	lis	A0, 0
	store	A0, _foreign_function_call_active, OCFP

/* Load the rest of lisp state. */
	load	CSP, _current_control_stack_pointer
	load	CFP, _current_control_frame_pointer

/* No longer atomic. */
	store	A0, PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET, OCFP

/* Were we interrupted? */
	load	OCFP, PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET
	ci	OCFP, 0
	je	1f

	ti	7, r0, trap_PendingInterrupt
1:

	/* Restore OCFP, LRA & CODE (they may have been GC'ed) */
	l	OCFP, 0(CFP)
	l	LRA, 4(CFP)
	l	CODE, 8(CFP)

	/* Reset the stack. */
	mr	CSP, CFP
	mr	CFP, OCFP
	cal	r1, -16(r1)

	/* Restore the second return value */
	mr	OCFP, NARGS

	/* And return */
	cal	LIP, (4-type_OtherPointer)(LRA)
	br	LIP



	.text
	.globl	undefined_tramp
undefined_tramp:
	ti	7, r0, trap_Error
	.byte	4
	.byte	23
	.byte	254
	.byte	43
	.byte	1
	.align	2

	.globl	closure_tramp
closure_tramp:
        l	LEXENV, SYMBOL_FUNCTION_OFFSET(CNAME)
        l	CNAME, CLOSURE_FUNCTION_OFFSET(LEXENV)
        cal	LIP, FUNCTION_HEADER_CODE_OFFSET(CNAME)
        br	LIP

/*
 * Function-end breakpoint magic.
 */
	.text
	.align	2
	.globl	_function_end_breakpoint_guts
_function_end_breakpoint_guts:
	.long	type_ReturnPcHeader
	b	1f
	mr	OCFP, CSP
	inc	CSP, 4
	lis	NARGS, 4
	mr	A1, NULLREG
	mr	A2, NULLREG
1:

	.globl	_function_end_breakpoint_trap
_function_end_breakpoint_trap:
	ti	7, r0, trap_FunctionEndBreakpoint
	b	1b

	.globl	_function_end_breakpoint_end
_function_end_breakpoint_end:
