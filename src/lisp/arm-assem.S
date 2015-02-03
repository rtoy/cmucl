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
	eor	reg_OCFP, reg_OCFP
	eor	reg_CFP, reg_CFP
	eor	reg_LIP, reg_LIP

	/* Set pseudo-atomic flag */
	mov	reg_NARGS, #1
	STORE_SYMBOL_VALUE(reg_NARGS, PSEUDO_ATOMIC_ATOMIC)

	ldr	reg_OCFP, =current_control_frame_pointer
	ldr	reg_OCFP, [reg_OCFP]

	/* No longer atomic */
	mov	reg_NARGS, #0
	STORE_SYMBOL_VALUE(reg_NARGS, PSEUDO_ATOMIC_ATOMIC)
	/* Check if we were interrupted. (Not implemented yet.)

	/* Pass in the arguments */	
	mov	reg_LEXENV, reg_NL0			
	mov	reg_CFP, reg_NL1
	cmp	reg_NL2, #1
	ldrge	reg_A0, [reg_CFP, #0]
	cmp	reg_NL2, #2
	ldrge	reg_A1, [reg_CFP, #4]
	cmp	reg_NL2, #3
	ldrge	reg_A2, [reg_CFP, #8]

	/* Function is an indirect closure */
	ldr	reg_CODE, [reg_LEXENV, #CLOSURE_FUNCTION_OFFSET]

	/* Calculate LRA */
	adr	reg_LRA, lra
	add	reg_LRA, reg_LRA, #type_OtherPointer

	/* Convert arg count to fixnum */
	lsl	reg_NARGS, reg_NL2, #2
	
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

GFUNCDEF(call_into_c)
	NOT_IMPLEMENTED("call_into_c")
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
