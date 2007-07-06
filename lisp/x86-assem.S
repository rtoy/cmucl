### x86-assem.S -*- Mode: Asm; -*-
/**
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/x86-assem.S,v 1.28 2007/07/06 08:04:39 cshapiro Exp $
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
#include "lispregs.h"

/* Minimize conditionalization for different OS naming schemes */
#ifndef DARWIN	
#define GNAME(var) var
#define FUNCDEF(x) \
	.text			; \
	.balign 4,0x90		; \
	.globl GNAME(x)		; \
	.type x,@function	; \
GNAME(x):			;
#define ENDFUNC(x) \
	.size GNAME(x),.-GNAME(x)
#else
#define GNAME(var) _##var
#define FUNCDEF(x) \
	.text			; \
	.align 2,0x90		; \
	.globl GNAME(x)		; \
GNAME(x):			;
#define ENDFUNC(x)
#endif

/* Get the right type of alignment.  Linux wants alignment in bytes. */
#if defined (__linux__) || defined (__FreeBSD__)
#define align_16byte    16
#else
#define	align_16byte	4	
#endif			

	.text
	.globl	GNAME(foreign_function_call_active)
	

/*
 * The C function will preserve ebx, esi, edi, and ebp across its
 * function call - ebx is used to save the return lisp address.
 *
 * Return values are in eax and maybe edx for quads, or st(0) for
 * floats.
 *
 * It should work for lisp calls C calls lisp calls C ..
 */
FUNCDEF(call_into_c)
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
ENDFUNC(call_into_c)



/* The C conventions require that ebx, esi, edi, and ebp be preserved
	across function calls. */
/* The *alien-stack* pointer is setup on the first call_into_lisp when
   the stack changes. */
FUNCDEF(call_into_lisp)
	pushl	%ebp		# save old frame pointer
	movl	%esp,%ebp	# establish new frame

/* Save the NPX state */
	fwait			# Catch any pending NPX exceptions.
	subl	$108,%esp	# Make room for the NPX state.
	fnsave	(%esp)		# Resets NPX

	movl	(%esp),%eax	# Load NPX control word
	andl	$0xfffff3ff,%eax	# Set rounding mode to nearest
#ifdef type_LongFloat
	orl	$0x00000300,%eax	# Set precision to 64 bits
#else
	orl	$0x00000200,%eax	# Set precision to 53 bits
#endif	
	pushl	%eax
	fldcw	(%esp)		# Recover modes
	popl	%eax

	fldz			# insure no FP regs are empty
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
	/* Registers eax, ecx, edx,edi,esi now live */

	/* Allocate new frame */
	mov	%esp,%ebx	# current sp marks start of new frame
	push	%ebp		# fp in save location S0
	sub	$8,%esp		# Ensure 3 slots are allocated, one above.
	mov	%ebx,%ebp	# switch to new frame

	/* Indirect the closure */
	call	*CLOSURE_FUNCTION_OFFSET(%eax)
	
	/* Multi-value return - blow off any extra values */
	mov	%ebx, %esp
	/* Single value return */	

/* Restore the stack, in case there was a stack change. */
	popl	%esp		# c-sp

/* Restore C regs: ebx esi edi */
	popl	%edi
	popl	%esi
	popl	%ebx

/* Restore the NPX state */
	frstor  (%esp)
	addl	$108, %esp
	
	popl	%ebp		# c-sp
	movl	%edx,%eax	# c-val
	ret
ENDFUNC(call_into_lisp)

/* Support for saving and restoring the NPX state from C. */
FUNCDEF(fpu_save)
	movl	4(%esp),%eax
	fnsave	(%eax)		# Save the NPX state - Resets NPX
	ret
ENDFUNC(fpu_save)

FUNCDEF(fpu_restore)
	movl	4(%esp),%eax
	frstor	(%eax)		# Restore the NPX state.
	ret
ENDFUNC(fpu_restore)


/*
 * The undefined-function trampoline.
 */
FUNCDEF(undefined_tramp)
	int3
	.byte	trap_Error
        /* Number of argument bytes */
        .byte   2
	.byte	UNDEFINED_SYMBOL_ERROR
        /* SC_OFFSET(sc_DescriptorReg,reg_EAX) */
        .byte   SC_OFFSET(sc_DescriptorReg,0)
	ret
ENDFUNC(undefined_tramp)

/*
 * The closure trampoline.
 */
FUNCDEF(closure_tramp)
	movl	FDEFN_FUNCTION_OFFSET(%eax),%eax
	jmp	*CLOSURE_FUNCTION_OFFSET(%eax)
ENDFUNC(closure_tramp)

/*
 * Function-end breakpoint magic.
 */
FUNCDEF(function_end_breakpoint_guts)
	/* Multiple Value return */
	jmp	multiple_value_return
	/* Single value return: The eventual return will now use the
	   multiple values return convention but with a return values
	   count of one. */
	movl	%esp,%ebx	# Setup ebx - the ofp.
	subl	$4,%esp		# Allocate one stack slot for the return value
	movl	$4,%ecx		# Setup ecx for one return value.
	movl	$NIL,%edi	# Default second value
	movl	$NIL,%esi	# Default third value
		
multiple_value_return:
	
	.globl GNAME(function_end_breakpoint_trap)
GNAME(function_end_breakpoint_trap):
	int3
	.byte 	trap_FunctionEndBreakpoint
	hlt			# Should never return here.

	.globl GNAME(function_end_breakpoint_end)
GNAME(function_end_breakpoint_end):


FUNCDEF(do_pending_interrupt)
	int3
	.byte 	trap_PendingInterrupt
	ret
ENDFUNC(do_pending_interrupt)
	
#ifdef trap_DynamicSpaceOverflowError
FUNCDEF(do_dynamic_space_overflow_error)
	int3
	.byte 	trap_DynamicSpaceOverflowError
	ret
ENDFUNC(do_dynamic_space_overflow_error)
#endif				
	
#ifdef trap_DynamicSpaceOverflowWarning
FUNCDEF(do_dynamic_space_overflow_warning)
	int3
	.byte 	trap_DynamicSpaceOverflowWarning
	ret
ENDFUNC(do_dynamic_space_overflow_warning)
#endif				
	

#ifdef WANT_CGC
/* A copy function optimized for the Pentium and works ok on
 * 486 as well. This assumes (does not check) that the input
 * byte count is a multiple of 8-bytes (one lisp object).
 * This code takes advantage of pairing in the Pentium as well
 * as the 128-bit cache line.
 */
FUNCDEF(fastcopy16)
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
ENDFUNC(fastcopy16)
#endif


/*
   Allocate bytes and return the start of the allocated space
   in the specified destination register.

   In the general case the size will be in the destination register.

   All registers must be preserved except the destination.
   The C conventions will preserve ebx, esi, edi, and ebp.
   So only eax, ecx, and edx need special care here. */
		
FUNCDEF(alloc_to_eax)
	pushl	%ecx	# Save ecx and edx as C could destroy them.
	pushl	%edx
	pushl	%eax	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	popl	%edx	# Restore ecx and edx.
	popl	%ecx
	ret
ENDFUNC(alloc_to_eax)

FUNCDEF(alloc_8_to_eax)
	pushl	%ecx	# Save ecx and edx as C could destroy them.
	pushl	%edx
	pushl	$8	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	popl	%edx	# Restore ecx and edx.
	popl	%ecx
	ret
ENDFUNC(alloc_8_to_eax)

FUNCDEF(alloc_16_to_eax)
	pushl	%ecx	# Save ecx and edx as C could destroy them.
	pushl	%edx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	popl	%edx	# Restore ecx and edx.
	popl	%ecx
	ret
ENDFUNC(alloc_16_to_eax)

FUNCDEF(alloc_to_ecx)
	pushl	%eax	# Save eax and edx as C could destroy them.
	pushl	%edx
	pushl	%ecx	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ecx	# setup the destination.
	popl	%edx	# Restore eax and edx.
	popl	%eax
	ret
ENDFUNC(alloc_to_ecx)

FUNCDEF(alloc_8_to_ecx)
	pushl	%eax	# Save eax and edx as C could destroy them.
	pushl	%edx
	pushl	$8	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ecx	# setup the destination.
	popl	%edx	# Restore eax and edx.
	popl	%eax
	ret
ENDFUNC(alloc_8_to_ecx)

FUNCDEF(alloc_16_to_ecx)
	pushl	%eax	# Save eax and edx as C could destroy them.
	pushl	%edx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ecx	# setup the destination.
	popl	%edx	# Restore eax and edx.
	popl	%eax
	ret
ENDFUNC(alloc_16_to_ecx)

FUNCDEF(alloc_to_edx)
	pushl	%eax	# Save eax and ecx as C could destroy them.
	pushl	%ecx
	pushl	%edx	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edx	# setup the destination.
	popl	%ecx	# Restore eax and ecx.
	popl	%eax
	ret
ENDFUNC(alloc_to_edx)

FUNCDEF(alloc_8_to_edx)
	pushl	%eax	# Save eax and ecx as C could destroy them.
	pushl	%ecx
	pushl	$8	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edx	# setup the destination.
	popl	%ecx	# Restore eax and ecx.
	popl	%eax
	ret
ENDFUNC(alloc_8_to_edx)

FUNCDEF(alloc_16_to_edx)
	pushl	%eax	# Save eax and ecx as C could destroy them.
	pushl	%ecx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edx	# setup the destination.
	popl	%ecx	# Restore eax and ecx.
	popl	%eax
	ret
ENDFUNC(alloc_16_to_edx)

FUNCDEF(alloc_to_ebx)
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
ENDFUNC(alloc_to_ebx)

FUNCDEF(alloc_8_to_ebx)
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
ENDFUNC(alloc_8_to_ebx)

FUNCDEF(alloc_16_to_ebx)
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
ENDFUNC(alloc_16_to_ebx)

FUNCDEF(alloc_to_esi)
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
ENDFUNC(alloc_to_esi)

FUNCDEF(alloc_8_to_esi)
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
ENDFUNC(alloc_8_to_esi)

FUNCDEF(alloc_16_to_esi)
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
ENDFUNC(alloc_16_to_esi)

FUNCDEF(alloc_to_edi)
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
ENDFUNC(alloc_to_edi)

FUNCDEF(alloc_8_to_edi)
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
ENDFUNC(alloc_8_to_edi)

FUNCDEF(alloc_16_to_edi)
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
ENDFUNC(alloc_16_to_edi)


#ifdef GENCGC

/* Called from lisp when an inline allocation overflows.
   Every register except the result needs to be preserved.
   We depend on C to preserve ebx, esi, edi, and ebp.
   But where necessary must save eax, ecx, edx. */

/* This routine handles an overflow with eax=crfp+size. So the
   size=eax-crfp. */
FUNCDEF(alloc_overflow_eax)
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%eax
	pushl	%eax		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
ENDFUNC(alloc_overflow_eax)

/* This routine handles an overflow with ecx=crfp+size. So the
   size=ecx-crfp. */
FUNCDEF(alloc_overflow_ecx)
	pushl	%eax		# Save eax
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%ecx
	pushl	%ecx		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ecx	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
ENDFUNC(alloc_overflow_ecx)

/* This routine handles an overflow with edx=crfp+size. So the
   size=edx-crfp. */
FUNCDEF(alloc_overflow_edx)
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	/* Calculate the size for the allocation. */
	subl	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%edx
	pushl	%edx		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edx	# setup the destination.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
ENDFUNC(alloc_overflow_edx)

/* This routine handles an overflow with ebx=crfp+size. So the
   size=ebx-crfp. */
FUNCDEF(alloc_overflow_ebx)
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%ebx
	pushl	%ebx		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ebx	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
ENDFUNC(alloc_overflow_ebx)

/* This routine handles an overflow with esi=crfp+size. So the
   size=esi-crfp. */
FUNCDEF(alloc_overflow_esi)
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%esi
	pushl	%esi		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%esi	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
ENDFUNC(alloc_overflow_esi)

/* This routine handles an overflow with edi=crfp+size. So the
   size=edi-crfp. */
FUNCDEF(alloc_overflow_edi)
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%edi
	pushl	%edi		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edi	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
ENDFUNC(alloc_overflow_edi)

#endif

#ifdef LINKAGE_TABLE

/* Call into C code to resolve a linkage entry.  The initial code in the 
 * linkage entry has done a call to here; pass that return entry along as a 
 * parameter.
 *
 * We could be called from raw Lisp code or from a foreign call site, so we 
 * have to save all the registers...
 */	
FUNCDEF(resolve_linkage_tramp)
	pushl	%ebp		# save old frame pointer
	movl	%esp,%ebp	# establish new frame
	subl	$28, %esp
	andl	$-16, %esp
	movl	%eax, 24(%esp)
	movl	%ecx, 20(%esp)
	movl	%edx, 16(%esp)
	movl	%ebx, 12(%esp)
	movl	%esi, 8(%esp)	
	movl	%edi, 4(%esp)
	/* calling location (plus offset) was on top of stack */
	movl	4(%ebp), %eax
	movl	%eax, (%esp)	# push for C function
	call	GNAME(lazy_resolve_linkage)
	/* real address of target is in %eax.  Replace return address on stack
	 * with it.  That way we can get out of here without trashing any 
	 *registers!
	 */
	movl	%eax,4(%ebp)
	movl	4(%esp), %edi
	movl	8(%esp), %esi
	movl	12(%esp), %ebx
	movl	16(%esp), %edx
	movl	20(%esp), %ecx
	movl	24(%esp), %eax
	leave
	ret			# jump to the real target
ENDFUNC(resolve_linkage_tramp)

/*
 * The C-callable undefined-foreign-symbol trapping function.
 */
FUNCDEF(undefined_foreign_symbol_trap)
	/* C Calling Convention, move one arg to EAX */
        pushl %ebp
        movl %esp,%ebp
        movl 8(%ebp),%eax

	/* Now trap to Lisp */
	int3
	.byte	trap_Error
        /* Number of argument bytes */
        .byte   2
	.byte	UNDEFINED_FOREIGN_SYMBOL_ERROR
        /* SC_OFFSET(sc_DescriptorReg,reg_EAX) */
        .byte   SC_OFFSET(sc_DescriptorReg,0)

	/* C Calling Convention */
	/* Doesn't matter here, but could if we'd use trap_Cerror */
	leave
	ret
ENDFUNC(undefined_foreign_symbol_trap)

#endif /* LINKAGE_TABLE */
