/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/mips-assem.s,v 1.14 1992/03/08 18:44:06 wlott Exp $ */
#include <machine/regdef.h>

#include "lisp.h"
#include "lispregs.h"
#include "globals.h"

/*
 * Function to save the global pointer.
 */
	.text
	.globl	current_global_pointer
	.ent	current_global_pointer
current_global_pointer:
	move	v0, gp
	j	ra
	.end	current_global_pointer

/*
 * And a function to restore the global pointer.
 */
	.text
	.globl	set_global_pointer
	.ent	set_global_pointer
set_global_pointer:
	move	gp, a0
	j	ra
	.end	set_global_pointer

#if !defined(s8)
#define s8 $30
#endif

/*
 * Function to transfer control into lisp.
 */
	.text
	.globl	call_into_lisp
	.ent	call_into_lisp
call_into_lisp:
#define framesize 12*4
	subu	sp, framesize
	.frame	sp, framesize, ra
	/* Save all the C regs. */
	.mask	0xd0ff0000, 0
	sw	ra, framesize(sp)
	sw	s8, framesize-4(sp)
	sw	gp, framesize-8(sp)
	sw	s7, framesize-12(sp)
	sw	s6, framesize-16(sp)
	sw	s5, framesize-20(sp)
	sw	s4, framesize-24(sp)
	sw	s3, framesize-28(sp)
	sw	s2, framesize-32(sp)
	sw	s1, framesize-36(sp)
	sw	s0, framesize-40(sp)

	/* Clear descriptor regs */
	move	t0, zero
	move	t1, zero
	move	t2, zero
	move	t3, zero
	move	t4, zero
	move	t5, zero
	move	t6, zero
	move	t7, zero
	move	s0, zero
	move	s1, zero
	move	s2, zero
	move	s3, zero
	move	gp, zero
	move	ra, zero

	.set	noreorder

	/* The saved FLAGS has the pseudo-atomic bit set. */
	li	NULLREG, NIL
	lw	FLAGS, current_flags_register

	/* No longer in foreign call. */
	sw	zero, foreign_function_call_active

        .set    reorder

	/* Load the rest of the LISP state. */
	lw	ALLOC, current_dynamic_space_free_pointer
	lw	BSP, current_binding_stack_pointer
	lw	CSP, current_control_stack_pointer
	lw	OCFP, current_control_frame_pointer

        .set    noreorder

	/* Check for interrupt */
	and	FLAGS, (0xffff^(1<<flag_Atomic))
	and	v0, FLAGS, (1<<flag_Interrupted)
	beq	v0, zero, 1f
	nop

	/* We were interrupted. Hit the trap. */
	break	trap_PendingInterrupt
1:

	.set	reorder

	/* Pass in args */
	move	CNAME, $4
	move	LEXENV, $5
	move	CFP, $6
	sll	NARGS, $7, 2
	lw	A0, 0(CFP)
	lw	A1, 4(CFP)
	lw	A2, 8(CFP)
	lw	A3, 12(CFP)
	lw	A4, 16(CFP)
	lw	A5, 20(CFP)

	/* Calculate LRA */
	la	LRA, lra + type_OtherPointer

	/* Indirect closure */
	lw	CODE, 4-1(LEXENV)

	/* Jump into lisp land. */
	addu	LIP, CODE, 6*4 - type_FunctionPointer
	j	LIP

	.set	noreorder

	.align	3
lra:
	.word	type_ReturnPcHeader

	/* Multiple value return spot, clear stack */
	move	CSP, OCFP
	nop

	/* Pass one return value back to C land. */
	move	v0, A0

	/* Set pseudo-atomic flag. */
	or	FLAGS, (1<<flag_Atomic)

	/* Save LISP registers. */
	sw	ALLOC, current_dynamic_space_free_pointer
	sw	BSP, current_binding_stack_pointer
	sw	CSP, current_control_stack_pointer
	sw	CFP, current_control_frame_pointer
	sw	FLAGS, current_flags_register

	/* Back in foreign function call */
	li	t0, 1
	sw	t0, foreign_function_call_active

	/* Check for interrupt */
	and	FLAGS, (0xffff^(1<<flag_Atomic))
	and	v1, FLAGS, (1<<flag_Interrupted)
	beq	v1, zero, 1f
	nop

	/* We were interrupted. Hit the trap. */
	break	trap_PendingInterrupt
1:

	.set	reorder

	/* Restore C regs */
	lw	ra, framesize(sp)
	lw	s8, framesize-4(sp)
	lw	gp, framesize-8(sp)
	lw	s7, framesize-12(sp)
	lw	s6, framesize-16(sp)
	lw	s5, framesize-20(sp)
	lw	s4, framesize-24(sp)
	lw	s3, framesize-28(sp)
	lw	s2, framesize-32(sp)
	lw	s1, framesize-36(sp)
	lw	s0, framesize-40(sp)

	/* Restore C stack. */
	addu	sp, framesize

	/* Back we go. */
	j	ra

	.end	call_into_lisp

/*
 * Transfering control from Lisp into C
 */
	.text
	.globl	call_into_c
	.ent	call_into_c
call_into_c:
	/* Set up a stack frame. */
	move	OCFP, CFP
	move	CFP, CSP
	addu	CSP, CFP, 32
	sw	OCFP, 0(CFP)
	sw	LRA, 4(CFP)
	sw	CODE, 8(CFP)

	/* Note: the C stack is already set up. */

	/* Set the pseudo-atomic flag. */
	.set	noreorder
	or	FLAGS, (1<<flag_Atomic)

	/* Save lisp state. */
	sw	ALLOC, current_dynamic_space_free_pointer
	sw	BSP, current_binding_stack_pointer
	sw	CSP, current_control_stack_pointer
	sw	CFP, current_control_frame_pointer
	sw	FLAGS, current_flags_register

	/* Mark us as in C land. */
	li	t0, 1
	sw	t0, foreign_function_call_active

	/* Restore GP */
	lw	gp, saved_global_pointer

	/* Were we interrupted? */
	and	FLAGS, (0xffff^(1<<flag_Atomic))
	and	t0, FLAGS, (1<<flag_Interrupted)
	beq	t0, zero, 1f
	nop

	/* We were interrupted. Hit the trap. */
	break	trap_PendingInterrupt
1:

	.set	reorder

	/* Into C land we go. */
	jal	v0

	/* Clear unsaved descriptor regs */
	move	t0, zero
	move	t1, zero
	move	t2, zero
	move	t3, zero
	move	t4, zero
	move	t5, zero
	move	t6, zero
	move	t7, zero
	move	gp, zero
	move	ra, zero

	.set	noreorder

	/* Restore FLAGS (which set the pseudo-atomic flag) */
	lw	FLAGS, current_flags_register

	/* Mark us at in Lisp land. */
	sw	zero, foreign_function_call_active

	/* Restore other lisp state. */
	lw	ALLOC, current_dynamic_space_free_pointer
	lw	BSP, current_binding_stack_pointer

	/* Check for interrupt */
	and	FLAGS, (0xffff^(1<<flag_Atomic))
	and	a0, FLAGS, (1<<flag_Interrupted)
	beq	a0, zero, 1f
	nop

	/* We were interrupted. Hit the trap. */
	break	trap_PendingInterrupt
1:

	.set	reorder

	/* Restore LRA & CODE (they may have been GC'ed) */
	lw	CODE, 8(CFP)
	lw	LRA, 4(CFP)

	/* Reset the lisp stack. */
	/* Note: OCFP and CFP are in saved regs. */
	move	CSP, CFP
	move	CFP, OCFP

	/* Return to LISP. */
	addu	LIP, LRA, 4-type_OtherPointer
	j	LIP

	.end	call_into_c

	.text
	.globl	start_of_tramps
start_of_tramps:

/*
 * The undefined-function trampoline.
 */
        .text
        .globl  undefined_tramp
        .ent    undefined_tramp
undefined_tramp:
        break   10
        .byte    4
        .byte    23
        .byte    254
        .byte    208
        .byte    1
        .align 2
        .end    undefined_tramp

/*
 * The closure trampoline.
 */
        .text
        .globl  closure_tramp
        .ent    closure_tramp
closure_tramp:
        lw      LEXENV, FDEFN_FUNCTION_OFFSET(CNAME)
        lw      L0, CLOSURE_FUNCTION_OFFSET(LEXENV)
        add     LIP, L0, FUNCTION_HEADER_CODE_OFFSET
        j       LIP
        .end    closure_tramp

	.text
	.globl	end_of_tramps
end_of_tramps:


/*
 * Function-end breakpoint magic.
 */

	.text
	.align	2
	.set	noreorder
	.globl	function_end_breakpoint_guts
function_end_breakpoint_guts:
	.word	type_ReturnPcHeader
	beq	zero, zero, 1f
	nop
	move	OCFP, CSP
	addu	CSP, 4
	li	NARGS, 4
	move	A1, NULLREG
	move	A2, NULLREG
	move	A3, NULLREG
	move	A4, NULLREG
	move	A5, NULLREG
1:

	.globl	function_end_breakpoint_trap
function_end_breakpoint_trap:
	break	trap_FunctionEndBreakpoint
	beq	zero, zero, 1b
	nop

	.globl	function_end_breakpoint_end
function_end_breakpoint_end:
	.set	reorder
