/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/mips-assem.s,v 1.2 1990/02/28 18:20:35 wlott Exp $ */
#include <machine/regdef.h>

#include "lisp.h"
#include "lispregs.h"

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


/*
 * Function to transfer control into lisp.
 */
	.text
	.globl	call_into_lisp
	.ent	call_into_lisp
call_into_lisp:
#define framesize 11*4
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
	lw	FLAGS, (SAVED_FLAGS_REGISTER - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)

	/* No longer in foreign call. */
	sw	NULLREG, (FOREIGN_FUNCTION_CALL_ACTIVE - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)

	/* Load the rest of the LISP state. */
	lw	ALLOC, (SAVED_ALLOCATION_POINTER - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)
	lw	BSP, (SAVED_BINDING_STACK_POINTER - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)
	lw	CSP, (SAVED_CONTROL_STACK_POINTER - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)

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
	move	ARGS, $6
	sll	NARGS, $7, 2
	lw	A0, 0(ARGS)
	lw	A1, 4(ARGS)
	lw	A2, 8(ARGS)
	lw	A3, 12(ARGS)
	lw	A4, 16(ARGS)
	lw	A5, 20(ARGS)

	/* Calculate LRA */
	la	LRA, lra + type_OtherPointer

	/* Establish context pointers */
	move	OLDCONT, $0 /* C doesn't have a context ptr */
	move	CONT, CSP

	/* Indirect closure */
	lw	CODE, 4-1(LEXENV)

	/* Jump into lisp land. */
	addu	$2, CODE, 6*4 - type_FunctionPointer
	j	$2

	.set	noreorder

	.align	3
lra:
	.word	type_ReturnPcHeader

	/* Multiple value return spot, clear stack */
	move	CSP, ARGS
	nop

	/* Pass one return value back to C land. */
	move	v0, A0

	/* Set pseudo-atomic flag. */
	or	FLAGS, (1<<flag_Atomic)

	/* Save LISP registers. */
	sw	ALLOC, SAVED_ALLOCATION_POINTER + SYMBOL_VALUE_OFFSET
	sw	BSP, SAVED_BINDING_STACK_POINTER + SYMBOL_VALUE_OFFSET
	sw	CSP, SAVED_CONTROL_STACK_POINTER + SYMBOL_VALUE_OFFSET
	sw	FLAGS, SAVED_FLAGS_REGISTER + SYMBOL_VALUE_OFFSET

	/* Back in foreign function call */
	addu	t0, NULLREG, T - NIL
	sw	t0, (FOREIGN_FUNCTION_CALL_ACTIVE - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)

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
	.set	noreorder
	/* Note: the C stack is already set up. */
	
	/* Set the pseudo-atomic flag. */
	or	FLAGS, (1<<flag_Atomic)

	/* Save lisp state in symbols. */
	sw	ALLOC, (SAVED_ALLOCATION_POINTER - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)
	sw	BSP, (SAVED_BINDING_STACK_POINTER - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)
	sw	CSP, (SAVED_CONTROL_STACK_POINTER - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)
	sw	FLAGS, (SAVED_FLAGS_REGISTER - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)

	/* Mark us as in C land. */
	addu	t0, NULLREG, T - NIL
	sw	t0, (FOREIGN_FUNCTION_CALL_ACTIVE - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)

	/* Restore GP */
	lw	gp, SAVED_GLOBAL_POINTER + SYMBOL_VALUE_OFFSET

	/* Were we interrupted? */
	and	FLAGS, (0xffff^(1<<flag_Atomic))
	and	v1, FLAGS, (1<<flag_Interrupted)
	beq	v1, zero, 1f
	nop

	/* We were interrupted. Hit the trap. */
	break	trap_PendingInterrupt
1:

	.set	reorder

	/* Get first 4 args. */
	lw	a0, 0(sp)
	lw	a1, 4(sp)
	lw	a2, 8(sp)
	lw	a3, 12(sp)

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
	lw	FLAGS, SAVED_FLAGS_REGISTER + SYMBOL_VALUE_OFFSET

	/* Mark us at in Lisp land. */
	sw	NULLREG, (FOREIGN_FUNCTION_CALL_ACTIVE - NIL + SYMBOL_VALUE_OFFSET)(NULLREG)

	/* Restore other lisp state. */
	lw	ALLOC, SAVED_ALLOCATION_POINTER + SYMBOL_VALUE_OFFSET
	lw	BSP, SAVED_BINDING_STACK_POINTER + SYMBOL_VALUE_OFFSET
	lw	CSP, SAVED_CONTROL_STACK_POINTER + SYMBOL_VALUE_OFFSET

	/* Check for interrupt */
	and	FLAGS, (0xffff^(1<<flag_Atomic))
	and	a0, FLAGS, (1<<flag_Interrupted)
	beq	a0, zero, 1f
	nop

	/* We were interrupted. Hit the trap. */
	break	trap_PendingInterrupt
1:

	.set	reorder

	/* Return to LISP. */
	addu	a0, LRA, 4-type_OtherPointer
	j	a0

	.end	call_into_c
