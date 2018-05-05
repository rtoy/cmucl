/*
 * This code written as part of the CMUCL project and has been placed
 * in the public domain.
 */
#define LANGUAGE_ASSEMBLY

#include "internals.h"
#include "lispregs.h"
#include "globals.h"

#define GFUNCDEF(x) \
	.globl x ; \
	.align 2 ; \
	.type	x, %function ; \
x:

#define STATIC_SYMBOL_OFFSET(sym)	(sym - NIL)

/*
 * Load the value of a static symbol into the given register
 */
#define LOAD_SYMBOL_VALUE(reg, sym) \
	ldr	reg, [reg_NULL, #(STATIC_SYMBOL_OFFSET(sym) + SYMBOL_VALUE_OFFSET)]

/*
 * Store a register as the value of a static symbol.
 */
#define STORE_SYMBOL_VALUE(reg, sym) \
	str	reg, [reg_NULL, #(STATIC_SYMBOL_OFFSET(sym) + SYMBOL_VALUE_OFFSET)]
	
#define ENDFUNC(x) \
	.size x, .-x ; \
	.pool
	
/*
 * Generate not-implemented trap with name "name"
 */	
#define NOT_IMPLEMENTED(name) \
	udf	# trap_NotImplemented ; \
	b	1f ; \
	.ascii	name ; \
	.align	4,0 ; \
1:

	.arm
	.text

/*
 * call_into_lisp(function, args, count)
 * Per AAPCS, r0-r2 contain the three args
 */		
GFUNCDEF(call_into_lisp)

	/* Save the callee-saved registers */
	push	{r4-r12, lr}
	vpush	{d8-d15}

	/* Establish NIL */
	ldr	reg_NULL, =NIL

	/* Save C stack pointer and the C frame pointer */
	STORE_SYMBOL_VALUE(sp, NUMBER_STACK_POINTER)
	STORE_SYMBOL_VALUE(reg_OCFP, NUMBER_FRAME_POINTER)

	/* Initialize tagged registers to 0 */
	eor	reg_CODE, reg_CODE
	eor	reg_A0, reg_A0
	eor	reg_A1, reg_A1
	eor	reg_A2, reg_A2
	eor	reg_LRA, reg_LRA
	eor	reg_LEXENV, reg_LEXENV
	eor	reg_CFP, reg_CFP
	eor	reg_LIP, reg_LIP

	/* Set pseudo-atomic flag */
	mov	reg_OCFP, #1
	STORE_SYMBOL_VALUE(reg_OCFP, PSEUDO_ATOMIC_ATOMIC)

	ldr	reg_OCFP, =current_control_frame_pointer
	ldr	reg_OCFP, [reg_OCFP]

	/* No longer atomic (A0 = 0, above) */
	STORE_SYMBOL_VALUE(reg_A0, PSEUDO_ATOMIC_ATOMIC)
	/* Check if we were interrupted. (Not implemented yet.) */

	/* setup up the current frame */
	mov	reg_CFP, reg_OCFP

	/* Pass in the arguments */
	mov	reg_LEXENV, reg_NL0
	cmp	reg_NARGS, #1
	ldrge	reg_A0, [reg_CFP, #0]
	cmp	reg_NARGS, #2
	ldrge	reg_A1, [reg_CFP, #-4]
	cmp	reg_NARGS, #3
	ldrge	reg_A2, [reg_CFP, #-8]

	/* Function is an indirect closure */
	ldr	reg_CODE, [reg_LEXENV, #CLOSURE_FUNCTION_OFFSET]

	/* Calculate LRA */
	adr	reg_LRA, lra
	add	reg_LRA, reg_LRA, #type_OtherPointer

	/* Convert arg count to fixnum */
	lsl	reg_NARGS, reg_NARGS, #2
	
	/*
	 * Get the Lisp stack pointer into the sp since on ARM sp is both
	 * the C and Lisp stack pointer.
	 */
	ldr	reg_NL0, =current_control_stack_pointer
	ldr	reg_CSP, [reg_NL0]

	/*
	 * Should we use reg_LIP instead of reg_NL0?
	 */
	add	reg_NL0, reg_CODE, #FUNCTION_CODE_OFFSET
	bx	reg_NL0
lra:
	.long	type_ReturnPcHeader

	NOT_IMPLEMENTED("call_into_lisp_return_to_c")

	vpop	{d8-d15}
	pop	{r4-r12,pc}
	ENDFUNC(call_into_lisp)

/*
 * Calls the specified C routine.  On entry, the C stack must be laid
 * out in this way:
 *
 *   <top of stack>	
 *	arg[N]
 *	arg[N-1]
 *	...
 *	arg[4]
 *	arg[3]
 *	arg[2]
 *	arg[1]
 *	arg[0]
 *	<addr of C routine>
 *   <bottom of stack>
 *
 * arg[0] to arg[3], along with the address of the C routine, will be
 * popped off the stack before calling the C routine.  Thus,
 * arg[0]-arg[3] are required to be allocated on the stack. The args
 * do not need to be allocated if the function does not need them.
 * arg[0]-arg[3] will be stored in r0-r3 as required by the AAPCS.
 * This also means that arg[4] should be on a double-word boundary as
 * required by the AAPCS.
 *
 * TODO: describe how floats are stored.
 */	
GFUNCDEF(call_into_c)
	/* Build a lisp stack frame */
	mov	reg_OCFP, reg_CFP
	mov	reg_CFP, reg_CSP
	sub	reg_CSP, reg_CSP, #32
	str	reg_OCFP, [reg_CFP]
	str	reg_CODE, [reg_CFP, #-8]

	/*
	 * The unboxed regs should be free for use to use as we wish.
	 * All of the args to the C function that would go in
	 * registers should be on the C stack.
	 */
	
	/* Turn on pseudo-atomic */
	mov	reg_NARGS, #1
	STORE_SYMBOL_VALUE(reg_NARGS, PSEUDO_ATOMIC_ATOMIC)

	/* Convert the return addres to an offset and save it on the stack */
	sub	reg_NL0, reg_LIP, reg_CODE
	add	reg_NL0, reg_NL0, #type_OtherPointer
	str	reg_NL0, [reg_CFP, #-4]

        /* Store lisp state */
	ldr	reg_NARGS, =current_control_stack_pointer
	str	reg_CSP, [reg_NARGS]
	ldr	reg_NARGS, =current_control_frame_pointer
	str	reg_CFP, [reg_NARGS]

	/* No longer in Lisp */
	ldr	reg_NARGS, =foreign_function_call_active
	str	reg_CSP, [reg_NARGS]

	/* Were we interrupted? Not implemented */

	/*
	 * Get the C stack pointer so we can grab the C args. (The
	 * Lisp stack pointer was saved above.)
	 */
	LOAD_SYMBOL_VALUE(sp, NUMBER_STACK_POINTER)

	/* Pop the function address first */
	pop	{reg_LIP}
	/* Now pop off r0-r3 */
	pop	{reg_NL0, reg_OCFP, reg_NARGS, reg_NFP}
	/* Into C we go */
	blx	reg_LIP

	/*
	 * Any integer results are stored in r0 and r1, so don't touch
	 * them!
	 */
	
	/* Re-establish NIL */
	ldr	reg_NULL, =NIL

	/* Save the C stack pointer, and get the Lisp stack pointer back */
	STORE_SYMBOL_VALUE(reg_CSP, NUMBER_STACK_POINTER)
	ldr	reg_LIP, =current_control_stack_pointer
	ldr	reg_CSP, [reg_LIP]
	ldr	reg_LIP, =current_control_frame_pointer
	ldr	reg_CFP, [reg_LIP]

	/* Atomic */
	mov	reg_NARGS, #1
	STORE_SYMBOL_VALUE(reg_NARGS, PSEUDO_ATOMIC_ATOMIC)

	/* No longer in foreign function call */
	
	/* Load the rest of the lisp state */
	/* Get the return address back */
	ldr	reg_LIP, [reg_CFP, #-4]
	ldr	reg_CODE, [reg_CFP, #-8]
	add	reg_LIP, reg_LIP, reg_CODE
	sub	reg_LIP, reg_LIP, #type_OtherPointer

	/* No longer atomic */
	mov	reg_NARGS, #0
	STORE_SYMBOL_VALUE(reg_NARGS, PSEUDO_ATOMIC_ATOMIC)
	
	/* Reset the lisp stack */
	mov	reg_CFP, reg_CSP
	mov	reg_OCFP, reg_CFP

	/* Return to Lisp */
	bx	reg_LIP
	ENDFUNC(call_into_c)

	.globl	undefined_tramp
	.byte	type_FunctionHeader
undefined_tramp:	
	.byte	0, 0, 0
	.long	undefined_tramp	/* self slot */
	.long	NIL		/* next slot */
	.long	NIL		/* name slot */
	.long	NIL		/* arglist slot */
	.long	NIL		/* type slot */

	NOT_IMPLEMENTED("undefined_trap")

	.globl	closure_tramp
	.byte	type_FunctionHeader
closure_tramp:	
	.byte	0, 0, 0	/* Header */
	.long	closure_tramp
	.long	NIL 
	.long	NIL
	.long	NIL
	.long	NIL

	NOT_IMPLEMENTED("closure_tramp")

/*
 * Function-end breakpoint magic.  See MAKE-BOGUS-LRA in
 * code/debug-int.lisp.
 */			
	.globl function_end_breakpoint_guts
function_end_breakpoint_guts:	
	.long	type_ReturnPcHeader
	NOT_IMPLEMENTED("function_end_breakpoint_guts")
	
	.globl function_end_breakpoint_trap
function_end_breakpoint_trap:
	NOT_IMPLEMENTED("function_end_breakpoint_trap")
	
	.globl function_end_breakpoint_end
function_end_breakpoint_end:
