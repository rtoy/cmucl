### x86-assem.S -*- Mode: Asm; -*-
/**
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/x86-assem.S,v 1.3 1997/02/22 19:19:47 dtc Exp $
 *
 * Author:	Paul F. Werkowski <pw@snoopy.mv.com>
 *
 * This code was written to support the port of CMU Common Lisp
 * to the Intel X86 ISA and the FreeBSD operating system. The
 * author has placed this code in the public domain September 1996.
 *
 */


#include "x86-validate.h"
	
#define LANGUAGE_ASSEMBLY
#include "internals.h"

/* Minimize conditionalization for different OS naming schemes */
#if defined __linux__
#define GNAME(var) var
#else
#define GNAME(var) _##var
#endif

/* Get the right type of alignment.  Linux wants alignment in bytes. */
#ifdef	__linux__
#define align_4byte   4
#define       align_8byte     8
#define       align_16byte    16
#else
#define	align_4byte	2
#define	align_8byte	3
#define	align_16byte	4	
#endif			
	.text
	.global	GNAME(sigtrap_handler)
	.global	GNAME(foreign_function_call_active)
	.global	GNAME(purify)
	.global GNAME(collect_garbage)
	.global	GNAME(current_control_stack_pointer)
	.global GNAME(call_into_c)
	.type	GNAME(call_into_c),@function
	.data

Cstack:	.long	0
Purify:	.long	GNAME(purify)

	.text


/*
 * The C function will preserve ebx, esi, edi, and ebp across its
 * function call - ebx is used to save the return lisp address.
 *
 * Return values are in eax and maybe edx for quads, or st(0) for
 * floats.
 *
 * It should work for lisp calls c calls lisp calls c ..
 */
	.text
	.align	align_16byte,0x90
	.global GNAME(call_into_c)
	.type	GNAME(call_into_c),@function
GNAME(call_into_c):
	movl	$1,GNAME(foreign_function_call_active)

/* Save the return lisp address in ebx */	
	popl	%ebx

/* Setup the NPX for C */
	pushl	%ecx		# Make a little temp room.
	fnstcw  (%esp)		# save exception masks
	fninit
	fldcw	(%esp)		# restore exception masks
	popl	%ecx		# Restore the stack.

	call	*%eax		# normal callout using Lisp stack

	movl	%eax,%ecx	# remember integer return value

/* Check for a return FP value */
	fxam
	fnstsw	%eax
	andl	$0x4500,%eax
	cmpl	$0x4100,%eax
	jne	Lfp_rtn_value

/* The return value is in eax, or eax,edx? */
/* Setup the NPX stack for lisp */
	fldz			# insure no regs are empty
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz

/* Restore the return value */
	movl	%ecx,%eax	# maybe return value

	movl	$0,GNAME(foreign_function_call_active)
/* Return */	
	jmp	*%ebx

Lfp_rtn_value:
/* The return result is in st(0) */
/* Setup the NPX stack for lisp, placing the result in st(0) */
	fldz			# insure no regs are empty
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	fxch	%st(7)		# move the result back to st(0)

/* Don't need to restore eax as the result is in st(0) */

	movl	$0,GNAME(foreign_function_call_active)
/* Return */	
	jmp	*%ebx

	.size	GNAME(call_into_c), . - GNAME(call_into_c)

	.align	align_16byte,0x90
	.type prep_mind_warp,@function
/* Purify wants to mess with the control stack and I can't really
 * do that if I am also running on that stack. So I will transfer
 * to the stack that was left at initial call_into_lisp time.
 */
prep_mind_warp:
	movl	%esp,GNAME(current_control_stack_pointer)
	movl	%esp,%ecx	# temp
	xchgl	%esp,Cstack	# now on C stack
	pushl	8(%ecx)		# arg 2
	pushl	4(%ecx)		# arg 1
	call	*%eax		# purify
	addl	$8,%esp		# pop 2 args
	xchgl	%esp,Cstack	# back to lisp stack

/* Purify does not return a fp value, so don't need to check,
 * just setup the lisp stack for Lisp */
	fldz			# insure no regs are not empty
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz

	movl	$0,GNAME(foreign_function_call_active)
/* Return */	
	jmp	%ebx
	.size	prep_mind_warp, . - prep_mind_warp



	.type	GNAME(do_purify),@function
	.global	GNAME(do_purify)
/* This is called from save to purge dynamic memory.
 * 
 * dtc: We are now on the lisp stack, but got here via a call_into_c so we
 * are in foreign_function land. Since a lisp image is being saved and
 * lisp will not be returned to it's not important to preserve
 * state which is not significant to saving an image.
 *
 * Need to see where any dynamic objects end up.
 * This seems like an incredible hack. Maybe I can fix purify.
 */
GNAME(do_purify):
	movl	%esp,GNAME(current_control_stack_pointer)

	movl	%esp,%ecx	# temp
	xchgl	%esp,Cstack	# now on C stack

	pushl	%ebp		# save old frame pointer
	movl	%esp,%ebp	# establish new frame

	pushl	8(%ecx)		# arg 2
	pushl	4(%ecx)		# arg 1
	call	GNAME(purify)	# purify
	addl	$8,%esp		# pop 2 args
	
	popl	%ebp

	xchgl	%esp,Cstack	# back to lisp stack
	
	ret
	.size	GNAME(do_purify), . - GNAME(do_purify)


	.global GNAME(call_into_lisp)
	.type  GNAME(call_into_lisp),@function
fpmask:	.word	0x33f		# 64-bit precision | all execptions masked
		
/* The C conventions require that ebx, esi, edi, and ebp be preserved
	across function calls. */
	
/* The *alien-stack* pointer is setup on the first call_into_lisp when
   the stack changes. */
	
	.align	align_16byte,0x90
GNAME(call_into_lisp):
	pushl	%ebp		# save old frame pointer
	movl	%esp,%ebp	# establish new frame

/* Save the NPX state */
	fwait			# Catch any pending NPX exceptions.
	subl	$108,%esp	# Make room for the NPX state.
	fnsave	(%esp)		# Resets NPX
	fldcw	(%esp)		# Recover modes
	fldz			# insure no regs are not empty
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	
/* Save C regs: ebx esi edi */
	pushl	%ebx
	pushl	%esi
	pushl	%edi
	
/* clear descriptor regs */
	xorl	%eax,%eax	# lexenv
	xorl	%ebx,%ebx	# available
	xorl	%ecx,%ecx	# arg count
	xorl	%edx,%edx	# first arg
	xorl	%edi,%edi	# second arg
	xorl	%esi,%esi	# third arg

/* no longer in function call */
	movl	%eax, GNAME(foreign_function_call_active)

	movl	%esp,%ebx	# remember current stack
	cmpl	$CONTROL_STACK_START,%esp
	jbe	ChangeToLispStack
	cmpl	$CONTROL_STACK_END,%esp
	jbe	OnLispStack
ChangeToLispStack:
	/* Setup the *alien-stack* pointer */
	movl	%esp,ALIEN_STACK + SYMBOL_VALUE_OFFSET
	movl	$CONTROL_STACK_END,%esp		# New stack
OnLispStack:
	pushl	%ebx		# save entry stack on (maybe) new stack

	/* establish lisp args */
	movl	 8(%ebp),%eax	# lexenv?
	movl	12(%ebp),%ebx	# address of arg vec
	movl	16(%ebp),%ecx	# num args
	shll	$2,%ecx		# make into fixnum
	cmpl	$0,%ecx
	je	Ldone
	movl	(%ebx),%edx	# arg0
	cmpl	$4,%ecx
	je	Ldone
	movl	4(%ebx),%edi	# arg1
	cmpl	$8,%ecx
	je	Ldone
	movl	8(%ebx),%esi	# arg2
Ldone:	
	/* registers eax, ecx, edx,edi,esi now live */

	/* alloc new frame */
	mov	%esp,%ebx	# current sp marks start of new frame
	push	%ebp		# fp in save location S0
	mov	%ebx,%ebp	# switch to new frame

	/* lra in the save location */
	pushl	$Llra

	/* push the lra for backward compatibility */
	pushl	$Llra

	/* indirect the closure */
	jmp	*CLOSURE_FUNCTION_OFFSET(%eax)
	
	.align	align_8byte
        .long   type_ReturnPcHeader
	nop
	nop
	nop
Llra:
	/* Multi-value return - blow off any extra values */
	mov	%ebx, %esp
	/* Single value return */	

/* Restore the stack, in case there was a stack change. */
	popl	%esp		# c-sp

/* restore C regs: ebx esi edi */
	popl	%edi
	popl	%esi
	popl	%ebx

/* Restore the NPX state */
	frstor  (%esp)
	addl	$108, %esp
	
	popl	%ebp		# c-sp
	movl	%edx,%eax	# c-val
	ret
	.size	GNAME(call_into_lisp), . - GNAME(call_into_lisp)


/* Fdefn objects for undefined functions have a 'raw' slot
   pointing here. The RISC systems seem to have this set up
   as an actual function object instead.
 */
	.text
	.align	align_4byte,0x90
	.global GNAME(undefined_tramp)
	.type	GNAME(undefined_tramp),@function
GNAME(undefined_tramp):
	int3
	.byte	trap_Cerror
        .byte   2
        .byte   23
        .byte   14
	ret
	.size	GNAME(undefined_tramp), .-GNAME(undefined_tramp)

/* This apparently gets called to hack on something in closures
   that can't get done by the compiler.
*/
	.align align_4byte,0x90
	.global GNAME(closure_tramp)
	.type	GNAME(closure_tramp),@function
GNAME(closure_tramp):
	movl	FDEFN_FUNCTION_OFFSET(%eax),%eax
	jmp	CLOSURE_FUNCTION_OFFSET(%eax)

	.size	GNAME(closure_tramp), .-GNAME(closure_tramp)

	.align align_4byte
	.global GNAME(function_end_breakpoint_trap)
	.global GNAME(function_end_breakpoint_end)
	.global	GNAME(function_end_breakpoint_guts)

GNAME(function_end_breakpoint_trap):
	ret
	.align align_4byte
GNAME(function_end_breakpoint_guts):
	ret
	.align align_4byte
GNAME(function_end_breakpoint_end):
	ret

	.global	GNAME(test_point)
	.align	align_4byte
GNAME(test_point):
	ret


	.global	GNAME(do_pending_interrupt)
	.type	GNAME(do_pending_interrupt),@function
	.align align_4byte,0x90
GNAME(do_pending_interrupt):
	int3
	.byte 	trap_PendingInterrupt
	ret
	.size	GNAME(do_pending_interrupt),.-GNAME(do_pending_interrupt)

/* A copy function optimized for the Pentium and works ok on
 * 486 as well. This assumes (does not check) that the input
 * byte count is a multiple of 8-bytes (one lisp object).
 * This code takes advantage of pairing in the Pentium as well
 * as the 128-bit cache line.
 */
	.global	GNAME(fastcopy16)
	.type	GNAME(fastcopy16),@function
	.align align_4byte,0x90
GNAME(fastcopy16):
	pushl	%ebp
	movl	%esp,%ebp
	movl	8(%ebp), %edx	# dst
	movl	12(%ebp),%eax	# src
	movl	16(%ebp),%ecx	# bytes
	pushl	%ebx
	pushl	%esi
	pushl	%edi
	movl	%edx,%edi
	movl	%eax,%esi
	sarl	$3,%ecx		# number 8-byte units
	testl	$1,%ecx		# odd?
	jz	Lquad
	movl	(%esi),%eax
	movl	4(%esi),%ebx
	movl	%eax,(%edi)
	movl	%ebx,4(%edi)
	leal	8(%esi),%esi
	leal	8(%edi),%edi
Lquad:	sarl	$1,%ecx		# count 16-byte units
	jz	Lend
	movl	%ecx,%ebp	# use ebp for loop counter
	.align	align_16byte,0x90
Ltop:
	movl	  (%edi),%eax	#prefetch! MAJOR Pentium win.
	movl	  (%esi),%eax
	movl	 4(%esi),%ebx
	movl	 8(%esi),%ecx
	movl	12(%esi),%edx
	movl	%eax,  (%edi)
	movl	%ebx, 4(%edi)
	movl	%ecx, 8(%edi)
	movl	%edx,12(%edi)
	leal	16(%esi),%esi
	leal	16(%edi),%edi
	decl	%ebp
	jnz	Ltop		# non-prefixed jump saves cycles
Lend:
	popl	%edi
	popl	%esi
	popl	%ebx
	popl	%ebp
	ret
	.size	GNAME(fastcopy16),.-GNAME(fastcopy16)
	.end
