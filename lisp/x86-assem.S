### x86-assem.S -*- Mode: Asm; -*-
/**
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/x86-assem.S,v 1.8 1997/11/28 14:58:23 dtc Exp $
 *
 * Authors:	Paul F. Werkowski <pw@snoopy.mv.com>
 *		Douglas T. Crosher
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
	.global	GNAME(foreign_function_call_active)
	

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
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)

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


	.text	
	.global GNAME(call_into_lisp)
	.type  GNAME(call_into_lisp),@function
		
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

/*
 * The undefined-function trampoline.
 */
	.text
	.align	align_4byte,0x90
	.global GNAME(undefined_tramp)
	.type	GNAME(undefined_tramp),@function
GNAME(undefined_tramp):
	int3
	.byte	trap_Error
        .byte   2
        .byte   23
        .byte   sc_DescriptorReg # EAX in the Descriptor-reg SC.
	ret
	.size	GNAME(undefined_tramp), .-GNAME(undefined_tramp)

/*
 * The closure trampoline.
 */
	.text
	.align	align_4byte,0x90
	.global GNAME(closure_tramp)
	.type	GNAME(closure_tramp),@function
GNAME(closure_tramp):
	movl	FDEFN_FUNCTION_OFFSET(%eax),%eax
	jmp	CLOSURE_FUNCTION_OFFSET(%eax)
	.size	GNAME(closure_tramp), .-GNAME(closure_tramp)

/*
 * Function-end breakpoint magic.
 */
	.text
	.global	GNAME(function_end_breakpoint_guts)
	.align	align_4byte
GNAME(function_end_breakpoint_guts):
	/* Multiple Value return */
	jmp	multiple_value_return
	/* Single value return */

	/* The eventual return will now be a multi-value return so
	   setup for it. Multi-value returns are not in general valid
	   for one value so convert to 2 values. Perhaps the
	   convertion should be changed? */
	movl	$8,%ecx		# Setup ecx=2 for the debugger.
	movl	$NIL,%edi	# Second value
	movl	%esp,%ebx	# ebx has the ofp
	subl	$8,%esp		# needed?
		
multiple_value_return:
	
	.global GNAME(function_end_breakpoint_trap)
GNAME(function_end_breakpoint_trap):
	int3
	.byte 	trap_FunctionEndBreakpoint

	/* Never returns here. */
	.global GNAME(function_end_breakpoint_end)
GNAME(function_end_breakpoint_end):
	hlt


	.global	GNAME(do_pending_interrupt)
	.type	GNAME(do_pending_interrupt),@function
	.align	align_4byte,0x90
GNAME(do_pending_interrupt):
	int3
	.byte 	trap_PendingInterrupt
	ret
	.size	GNAME(do_pending_interrupt),.-GNAME(do_pending_interrupt)

#ifdef WANT_CGC
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
#endif

#ifdef GENCGC
/* Fast bzero using the FPU. The first argument is the start address
   which needs to be aligned on an 8 byte boundary, the second argument
   is the number of bytes which needs to be a multiple of 8 bytes and
   not zero. */
	.text
	.globl	GNAME(i586_bzero)
	.type	GNAME(i586_bzero),@function
	.align	align_4byte,0x90
GNAME(i586_bzero):
	movl	4(%esp),%edx	# Load the start address
	movl	8(%esp),%eax	# Load the number of bytes
	fldz
l1:	fstl	0(%edx)
	addl	$8,%edx
	subl	$8,%eax
	jnz	l1
	fstp	%st(0)
	ret
	.size	GNAME(i586_bzero),.-GNAME(i586_bzero)
#endif	


/*
   Allocate bytes and return the start of the allocated space
   in the specified destination register.

   In the general case the size will be in the destination register.

   All registers must be preserved except the destination.
   The C conventions will preserve ebx, esi, edi, and ebp.
   So only eax, ecx, and edx need special care here. */
		
	.globl	GNAME(alloc_to_eax)
	.type	GNAME(alloc_to_eax),@function
	.align	align_4byte,0x90
GNAME(alloc_to_eax):
	pushl	%ecx	# Save ecx and edx as C could destroy them.
	pushl	%edx
	pushl	%eax	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	popl	%edx	# Restore ecx and edx.
	popl	%ecx
	ret
	.size	GNAME(alloc_to_eax),.-GNAME(alloc_to_eax)

	.globl	GNAME(alloc_8_to_eax)
	.type	GNAME(alloc_8_to_eax),@function
	.align	align_4byte,0x90
GNAME(alloc_8_to_eax):
	pushl	%ecx	# Save ecx and edx as C could destroy them.
	pushl	%edx
	pushl	$8	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	popl	%edx	# Restore ecx and edx.
	popl	%ecx
	ret
	.size	GNAME(alloc_8_to_eax),.-GNAME(alloc_8_to_eax)

	.globl	GNAME(alloc_8_to_eax)
	.type	GNAME(alloc_8_to_eax),@function
	.align	align_4byte,0x90

	.globl	GNAME(alloc_16_to_eax)
	.type	GNAME(alloc_16_to_eax),@function
	.align	align_4byte,0x90
GNAME(alloc_16_to_eax):
	pushl	%ecx	# Save ecx and edx as C could destroy them.
	pushl	%edx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	popl	%edx	# Restore ecx and edx.
	popl	%ecx
	ret
	.size	GNAME(alloc_16_to_eax),.-GNAME(alloc_16_to_eax)


	.globl	GNAME(alloc_to_ecx)
	.type	GNAME(alloc_to_ecx),@function
	.align	align_4byte,0x90
GNAME(alloc_to_ecx):
	pushl	%eax	# Save eax and edx as C could destroy them.
	pushl	%edx
	pushl	%ecx	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ecx	# setup the destination.
	popl	%edx	# Restore eax and edx.
	popl	%eax
	ret
	.size	GNAME(alloc_to_ecx),.-GNAME(alloc_to_ecx)

	.globl	GNAME(alloc_8_to_ecx)
	.type	GNAME(alloc_8_to_ecx),@function
	.align	align_4byte,0x90
GNAME(alloc_8_to_ecx):
	pushl	%eax	# Save eax and edx as C could destroy them.
	pushl	%edx
	pushl	$8	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ecx	# setup the destination.
	popl	%edx	# Restore eax and edx.
	popl	%eax
	ret
	.size	GNAME(alloc_8_to_ecx),.-GNAME(alloc_8_to_ecx)

	.globl	GNAME(alloc_16_to_ecx)
	.type	GNAME(alloc_16_to_ecx),@function
	.align	align_4byte,0x90
GNAME(alloc_16_to_ecx):
	pushl	%eax	# Save eax and edx as C could destroy them.
	pushl	%edx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ecx	# setup the destination.
	popl	%edx	# Restore eax and edx.
	popl	%eax
	ret
	.size	GNAME(alloc_16_to_ecx),.-GNAME(alloc_16_to_ecx)


	.globl	GNAME(alloc_to_edx)
	.type	GNAME(alloc_to_edx),@function
	.align	align_4byte,0x90
GNAME(alloc_to_edx):
	pushl	%eax	# Save eax and ecx as C could destroy them.
	pushl	%ecx
	pushl	%edx	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edx	# setup the destination.
	popl	%ecx	# Restore eax and ecx.
	popl	%eax
	ret
	.size	GNAME(alloc_to_edx),.-GNAME(alloc_to_edx)

	.globl	GNAME(alloc_8_to_edx)
	.type	GNAME(alloc_8_to_edx),@function
	.align	align_4byte,0x90
GNAME(alloc_8_to_edx):
	pushl	%eax	# Save eax and ecx as C could destroy them.
	pushl	%ecx
	pushl	$8	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edx	# setup the destination.
	popl	%ecx	# Restore eax and ecx.
	popl	%eax
	ret
	.size	GNAME(alloc_8_to_edx),.-GNAME(alloc_8_to_edx)

	.globl	GNAME(alloc_16_to_edx)
	.type	GNAME(alloc_16_to_edx),@function
	.align	align_4byte,0x90
GNAME(alloc_16_to_edx):
	pushl	%eax	# Save eax and ecx as C could destroy them.
	pushl	%ecx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edx	# setup the destination.
	popl	%ecx	# Restore eax and ecx.
	popl	%eax
	ret
	.size	GNAME(alloc_16_to_edx),.-GNAME(alloc_16_to_edx)



	.globl	GNAME(alloc_to_ebx)
	.type	GNAME(alloc_to_ebx),@function
	.align	align_4byte,0x90
GNAME(alloc_to_ebx):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	%ebx	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ebx	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	.size	GNAME(alloc_to_ebx),.-GNAME(alloc_to_ebx)

	.globl	GNAME(alloc_8_to_ebx)
	.type	GNAME(alloc_8_to_ebx),@function
	.align	align_4byte,0x90
GNAME(alloc_8_to_ebx):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$8	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ebx	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	.size	GNAME(alloc_8_to_ebx),.-GNAME(alloc_8_to_ebx)

	.globl	GNAME(alloc_16_to_ebx)
	.type	GNAME(alloc_16_to_ebx),@function
	.align	align_4byte,0x90
GNAME(alloc_16_to_ebx):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ebx	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	.size	GNAME(alloc_16_to_ebx),.-GNAME(alloc_16_to_ebx)



	.globl	GNAME(alloc_to_esi)
	.type	GNAME(alloc_to_esi),@function
	.align	align_4byte,0x90
GNAME(alloc_to_esi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	%esi	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%esi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	.size	GNAME(alloc_to_esi),.-GNAME(alloc_to_esi)

	.globl	GNAME(alloc_8_to_esi)
	.type	GNAME(alloc_8_to_esi),@function
	.align	align_4byte,0x90
GNAME(alloc_8_to_esi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$8	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%esi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	.size	GNAME(alloc_8_to_esi),.-GNAME(alloc_8_to_esi)

	.globl	GNAME(alloc_16_to_esi)
	.type	GNAME(alloc_16_to_esi),@function
	.align	align_4byte,0x90
GNAME(alloc_16_to_esi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%esi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	.size	GNAME(alloc_16_to_esi),.-GNAME(alloc_16_to_esi)


	.globl	GNAME(alloc_to_edi)
	.type	GNAME(alloc_to_edi),@function
	.align	align_4byte,0x90
GNAME(alloc_to_edi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	%edi	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	.size	GNAME(alloc_to_edi),.-GNAME(alloc_to_edi)

	.globl	GNAME(alloc_8_to_edi)
	.type	GNAME(alloc_8_to_edi),@function
	.align	align_4byte,0x90
GNAME(alloc_8_to_edi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$8	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	.size	GNAME(alloc_8_to_edi),.-GNAME(alloc_8_to_edi)

	.globl	GNAME(alloc_16_to_edi)
	.type	GNAME(alloc_16_to_edi),@function
	.align	align_4byte,0x90
GNAME(alloc_16_to_edi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	.size	GNAME(alloc_16_to_edi),.-GNAME(alloc_16_to_edi)
		
	.end



#ifdef GENCGC

/* Called from lisp when an inline allocation overflows.
   Every register except the result needs to be preserved.
   We depend on C to preserve ebx, esi, edi, and ebp.
   But where necessary must save eax, ecx, edx. */

/* This routine handles an overflow with eax=crfp+size. So the
   size=eax-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_eax)
	.type	GNAME(alloc_overflow_eax),@function
GNAME(alloc_overflow_eax):
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%eax
	pushl	%eax		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_eax),.-GNAME(alloc_overflow_eax)

/* This routine handles an overflow with ecx=crfp+size. So the
   size=ecx-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_ecx)
	.type	GNAME(alloc_overflow_ecx),@function
GNAME(alloc_overflow_ecx):
	pushl	%eax		# Save eax
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%ecx
	pushl	%ecx		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ecx	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_ecx),.-GNAME(alloc_overflow_ecx)

/* This routine handles an overflow with edx=crfp+size. So the
   size=edx-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_edx)
	.type	GNAME(alloc_overflow_edx),@function
GNAME(alloc_overflow_edx):
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%edx
	pushl	%edx		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edx	# setup the destination.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_edx),.-GNAME(alloc_overflow_edx)

/* This routine handles an overflow with ebx=crfp+size. So the
   size=ebx-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_ebx)
	.type	GNAME(alloc_overflow_ebx),@function
GNAME(alloc_overflow_ebx):
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%ebx
	pushl	%ebx		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ebx	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_ebx),.-GNAME(alloc_overflow_ebx)

/* This routine handles an overflow with esi=crfp+size. So the
   size=esi-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_esi)
	.type	GNAME(alloc_overflow_esi),@function
GNAME(alloc_overflow_esi):
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%esi
	pushl	%esi		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%esi	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_esi),.-GNAME(alloc_overflow_esi)

/* This routine handles an overflow with edi=crfp+size. So the
   size=edi-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_edi)
	.type	GNAME(alloc_overflow_edi),@function
GNAME(alloc_overflow_edi):
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%edi
	pushl	%edi		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edi	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_edi),.-GNAME(alloc_overflow_edi)

#endif
