/* ### x86-assem.S -*- Mode: Asm; -*- */
/**
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/x86-assem.S,v 1.35 2010/12/22 02:12:52 rtoy Exp $
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

/*
 * Emit the appropriate instruction used for implementing traps.
 * Currently, this is the UD1 instruction.  However, it make it
 * easy to add the trap code, use a sequence of bytes.  The code
 * is smashed into the mod r/m byte with the mod bits set to
 * #b11.  This MUST be coordinated with the Lisp code and the C
 * code.
 *
 * Also, clang doesn't recognize the ud1 instruction.
 */
#define UD1(code) \
	.byte 0x0f		; \
	.byte 0xb9		; \
	.byte 0xc0 + code

/* Minimize conditionalization for different OS naming schemes */
#ifdef DARWIN	
#define GNAME(var) _##var
#define FUNCDEF(x) \
	.text			; \
	.align 2,0x90		; \
	.globl GNAME(x)		; \
GNAME(x):			;
#define ENDFUNC(x)
#elif defined(SOLARIS)
#define GNAME(var) var
#define FUNCDEF(x) \
	.text			; \
	.align 16,0x90		; \
	.globl GNAME(x)		; \
	.type x,@function	; \
GNAME(x):			;
#define ENDFUNC(x) \
	.size GNAME(x),.-GNAME(x)
#else
#define GNAME(var) var
#define FUNCDEF(x) \
	.text			; \
	.balign 4,0x90		; \
	.globl GNAME(x)		; \
	.type x,@function	; \
GNAME(x):			;
#define ENDFUNC(x) \
	.size GNAME(x),.-GNAME(x)
#endif

#ifdef SOLARIS
#define	INT3	int $3

#else
#define INT3	int3
#endif
								
/* Get the right type of alignment.  Linux wants alignment in bytes. */
#if defined (__linux__) || defined (__FreeBSD__)
#define align_16byte    16
#else
#define	align_16byte	4	
#endif			

/*
 * Allocate |bytes| on the stack, and make sure the stack pointer is
 * aligned on a 16-byte boundary.  (Needed on Darwin, and harmless on 
 * others that don't need such alignment.)
 */		
#define STACK_PROLOGUE(bytes) \
	pushl	%ebp		; \
	mov	%esp, %ebp	; \
	subl	$bytes, %esp	; \
	andl	$-16, %esp	; 

/* Undo STACK_PROLOGUE */
#define STACK_EPILOGUE \
	movl	%ebp, %esp	; \
	popl	%ebp		; 
	
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

	call	*%eax		# normal callout using Lisp stack

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
	movl	%esp, %eax	# Remember the current stack pointer
	/*
	 * The SSE state is 512 bytes, but we need 16 more because we
	 * need 16-byte alignment.
	 */
	subl	$512+16,%esp
	andl	$-16, %esp	# fxsave needs 16-byte alignment
	fxsave	(%esp)		
	pushl	%eax		# Save the old stack pointer
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
	cmpl	GNAME(control_stack), %esp

	jbe	ChangeToLispStack
	cmpl	GNAME(control_stack_end), %esp
	jbe	OnLispStack
ChangeToLispStack:
	/* Setup the *alien-stack* pointer */
	movl	%esp,ALIEN_STACK + SYMBOL_VALUE_OFFSET
	movl	GNAME(control_stack_end), %esp		# New stack
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
	popl	%eax		# Get the old stack pointer
	fxrstor	(%esp)		# Restore the SSE state
	movl	%eax, %esp	# Now really restore the old stack pointer
			
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

FUNCDEF(sse_save)
	movl	4(%esp),%eax
	addl	$16, %eax	# Make sure eax is on a 16-byte boundary
	and	$-16, %eax
	fxsave	(%eax)
	ret
ENDFUNC(sse_save)
		
FUNCDEF(sse_restore)
	movl	4(%esp),%eax
	addl	$16, %eax	# Make sure eax is on a 16-byte boundary
	and	$-16, %eax
	fxrstor	(%eax)
	ret
ENDFUNC(sse_restore)


#if 0
/*
 * These are now implemented as Lisp assembly routines.  We leave
 * these here for the time being until we're sure the assembly
 * routines are working as expected.
 */

 /*
 * The undefined-function trampoline.
 */
FUNCDEF(undefined_tramp)
	UD1(trap_Error)
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

#endif

/*
 * Function-end breakpoint magic.
 */
FUNCDEF(function_end_breakpoint_guts)
	/* Multiple Value return
	   This MUST be a two-byte instruction.  If it isn't tracing is 
	   majorly broken. */
	jmp	1f
	/* Single value return: The eventual return will now use the
	   multiple values return convention but with a return values
	   count of one. */
	movl	%esp,%ebx	# Setup ebx - the ofp.
	subl	$4,%esp		# Allocate one stack slot for the return value
	movl	$4,%ecx		# Setup ecx for one return value.
	movl	$NIL,%edi	# Default second value
	movl	$NIL,%esi	# Default third value
		
1:	
multiple_value_return:
	
	.globl GNAME(function_end_breakpoint_trap)
GNAME(function_end_breakpoint_trap):
	UD1(trap_FunctionEndBreakpoint)
	hlt			# Should never return here.
ENDFUNC(function_end_breakpoint_trap)

	.globl GNAME(function_end_breakpoint_end)
GNAME(function_end_breakpoint_end):

FUNCDEF(do_pending_interrupt)
	UD1(trap_PendingInterrupt)
	ret
ENDFUNC(do_pending_interrupt)
	
#ifdef trap_DynamicSpaceOverflowError
FUNCDEF(do_dynamic_space_overflow_error)
	UD1(trap_DynamicSpaceOverflowError)
	ret
ENDFUNC(do_dynamic_space_overflow_error)
#endif				
	
#ifdef trap_DynamicSpaceOverflowWarning
FUNCDEF(do_dynamic_space_overflow_warning)
	UD1(trap_DynamicSpaceOverflowWarning)
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
 * alloc_overflow_sse2 must not be called from
 * C because it doesn't follow C conventions.
 *
 * On entry:
 * %eax = bytes to allocate
 * On exit:
 * %eax = address
 */
FUNCDEF(alloc_overflow_sse2)
	# Need 8*16 bytes for the xmm registers, and space to save ecx
	# and edx, space for mxcsr, a temp, and one arg to pass to alloc.
	# That's 8*16 + 5*4 = 148 bytes.  Might as well have a few
	# more so the xmm0 area is 16-byte aligned. That makes it 160
	# bytes.
	#
	# Stack looks like:
	#
	#      +160
	#      +144 -> xmm7
	#      +128 -> xmm6
	#      +112 -> xmm5
	#      +96  -> xmm4
	#      +80  -> xmm3
	#      +64  -> xmm2
	#      +48  -> xmm1
	#      +32  -> xmm0
	#      +20  -> unused
	#      +16  -> temp
	#      +12  -> mxcsr
	#      + 8  -> save ecx
	#      + 4  -> save edx
	#  esp + 0  -> arg for alloc
	STACK_PROLOGUE(160)
	movapd  %xmm0, (32 + 0*16)(%esp)
	movapd  %xmm1, (32 + 1*16)(%esp)
	movapd  %xmm2, (32 + 2*16)(%esp)
	movapd  %xmm3, (32 + 3*16)(%esp)
	movapd  %xmm4, (32 + 4*16)(%esp)
	movapd  %xmm5, (32 + 5*16)(%esp)
	movapd  %xmm6, (32 + 6*16)(%esp)
	movapd  %xmm7, (32 + 7*16)(%esp)

	movl	%ecx, 8(%esp)	# Save ecx and edx registers
	movl	%edx, 4(%esp)
	stmxcsr 12(%esp)	# Save MXCSR
	/* Clear the exceptions that might have occurred */
	movl	12(%esp), %edx
	and	$-64, %edx	# Clear the exceptions
	movl	%edx, 16(%esp)
	ldmxcsr 16(%esp)	# Get new mxcsr value
	movl	%eax, (%esp)	# Put size on stack for first arg to alloc()

	call	GNAME(alloc)

	movl	4(%esp), %edx	# Restore edx and ecx registers.  eax has the return value.
	movl	8(%esp), %ecx
	ldmxcsr	12(%esp)

	movapd  (32 + 0*16)(%esp), %xmm0
	movapd  (32 + 1*16)(%esp), %xmm1
	movapd  (32 + 2*16)(%esp), %xmm2
	movapd  (32 + 3*16)(%esp), %xmm3
	movapd  (32 + 4*16)(%esp), %xmm4
	movapd  (32 + 5*16)(%esp), %xmm5
	movapd  (32 + 6*16)(%esp), %xmm6
	movapd  (32 + 7*16)(%esp), %xmm7

	STACK_EPILOGUE
	ret
ENDFUNC(alloc_overflow_sse2)	

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
	UD1(trap_Error)
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
