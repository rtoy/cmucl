/*
 * This code written as part of the CMUCL project and has been placed
 * in the public domain.
 */
#define LANGUAGE_ASSEMBLY

#include "internals.h"
#include "lispregs.h"
#include "globals.h"

#define	FUNCDEF(x) \
	.text ; \
	.align 2 ; \
	.type	x, %function ; \
x:
						
#define	GFUNCDEF(x) \
	.global x ; \
	FUNCDEF(x)

/*
 * Load register from C global using temp as a work register
 */
#define load(reg, global, temp) \
	movw	temp, #:lower16:global ; \
	movt	temp, #:upper16:global ; \
	ldr	reg, [temp]

/*
 * Like load, but the stores reg into global.
 */
#define store(reg, global, temp) \
	movw	temp, #:lower16:global ; \
	movt	temp, #:upper16:global ; \
	str	reg, [temp]

#define SET_SIZE(x) \
	.size	x, .-x

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
 * Per ARM ABI, r0-r2 contains the three args
 */		
	GFUNCDEF(call_into_lisp)

	/* Save the callee-saved registers */
	push	{r4-r12,lr}
	vpush	{d8-d15}

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
	/* Establish NIL */
	movw	reg_NULL, #(NIL & 0xffff)
	movt	reg_NULL, #(NIL >> 16)
	
	/* Save C stack pointer */
	str	sp, [reg_NULL, #(NUMBER_STACK_POINTER - NIL)]

	/* Set pseudo-atomic flag */
	mov	reg_NARGS, #1
	str	reg_NARGS, [reg_NULL, #(PSEUDO_ATOMIC_ATOMIC - NIL)]

	load(reg_OCFP, current_control_frame_pointer, reg_NARGS)

	/* No longer atomic */
	mov	reg_NARGS, #0
	str	reg_NARGS, [reg_NULL, #(PSEUDO_ATOMIC_ATOMIC - NIL)]
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
	ldr reg_CODE, [reg_LEXENV, #CLOSURE_FUNCTION_OFFSET]

	/* Calculate LRA */
	adr	reg_LRA, lra
	add	reg_LRA, reg_LRA, #type_OtherPointer

	/* Convert arg count to fixnum */
	lsl	reg_NARGS, reg_NL2, #2
	
	/*
	 * Get the Lisp stack pointer into the sp since on ARM sp is both
	 * the C and Lisp stack pointer.
	 */
	load(reg_CSP, current_control_stack_pointer, reg_NARGS)

	/*
	 * This is the ppc version, not the sparc version. Should we
	 * use reg_LIP instead of reg_NL0?
	 */
	add	reg_NL0, reg_CODE, # FUNCTION_CODE_OFFSET*4 - type_FunctionPointer
	bx	reg_NL0
lra:
	.long	type_ReturnPcHeader

	NOT_IMPLEMENTED("call_into_lisp_return_to_c")

	vpop	{d8-d15}
	pop	{r4-r12,pc}
	SET_SIZE(call_into_lisp)

	GFUNCDEF(call_into_c)
	NOT_IMPLEMENTED("call_into_c")
	SET_SIZE(call_into_c)

	.globl	undefined_tramp
undefined_tramp:	
	.byte 0, 0, type_FunctionHeader
	.long undefined_tramp	/* self slot */
	.long NIL		/* next slot */
	.long NIL		/* name slot */
	.long NIL		/* arglist slot */
	.long NIL		/* type slot */

	.align
	NOT_IMPLEMENTED("undefined_trap")

	.globl	closure_tramp
closure_tramp:	
	.byte 0,0,type_FunctionHeader	/* Header */
	.long closure_tramp
	.long NIL 
	.long NIL
	.long NIL
	.long NIL

	.align
	NOT_IMPLEMENTED("closure_tramp")

/*
 * Function-end breakpoint magic.  See MAKE-BOGUS-LRA in
 * code/debug-int.lisp.
 */			
	.text
	.align
	.globl function_end_breakpoint_guts
function_end_breakpoint_guts:	
	.long	type_ReturnPcHeader
	NOT_IMPLEMENTED("function_end_breakpoint_guts")
	
	.globl function_end_breakpoint_trap
function_end_breakpoint_trap:
	NOT_IMPLEMENTED("function_end_breakpoint_trap")
	
	.globl function_end_breakpoint_end
function_end_breakpoint_end:
