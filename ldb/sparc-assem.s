
#include <machine/asm_linkage.h>
#include <machine/psl.h>
#include <machine/trap.h>

#define LANGUAGE_ASSEMBLY
#include "lispregs.h"
#include "lisp.h"
#include "globals.h"

#define load(sym, reg) \
        sethi   %hi(NAME(sym)), reg;    ld      [reg+%lo(NAME(sym))], reg
#define store(reg, sym) \
        sethi %hi(NAME(sym)), L0; st reg, [L0+%lo(NAME(sym))]


#define FRAMESIZE (SA(MINFRAME)+SA(4))

        .seg    "text"
        .global NAME(call_into_lisp)
NAME(call_into_lisp):
        save    %sp, -FRAMESIZE, %sp

	/* Flush all of C's register windows to the stack. */
	ta	ST_FLUSH_WINDOWS

        /* Save the return address. */
        st      %i7, [%fp-4]

        /* Clear the descriptor regs. */
        mov     ZERO, A0
        mov     ZERO, A1
        mov     ZERO, A2
        mov     ZERO, A3
        mov     ZERO, A4
        mov     ZERO, A5
        mov     ZERO, OCFP
        mov     ZERO, LRA
        mov     ZERO, L2
        mov     ZERO, CODE

        /* Establish NIL */
        set     NIL, NULLREG

        /* No longer in foreign function call. */
        /* Note: the atomic flag should still be set. */
        sethi   %hi(NAME(foreign_function_call_active)), NL0
        st      ZERO, [NL0+%lo(NAME(foreign_function_call_active))]

        /* Load the rest of lisp state. */
        load(current_dynamic_space_free_pointer, ALLOC)
        load(current_binding_stack_pointer, BSP)
        load(current_control_stack_pointer, CSP)
        load(current_control_frame_pointer, OCFP)

        /* No longer atomic. */
        sethi   %hi(PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET), NL0
        st      ZERO, [NL0+%lo(PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET)]

        /* Were we interrupted? */
        sethi   %hi(PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET), NL0
        ld      [NL0+%lo(PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET)], NL0
        tst     NL0
        beq     1f
        nop

        /* Yep, we were. */
        unimp   trap_PendingInterrupt
1:

        /* Pass in the args. */
        /* Note: CNAME and LEXENV already hold the correct values. */
        mov     %i2, CFP
        sll     %i3, 2, NARGS
        ld      [CFP+0], A0
        ld      [CFP+4], A1
        ld      [CFP+8], A2
        ld      [CFP+12], A3
        ld      [CFP+16], A4
        ld      [CFP+20], A5

        /* Calculate LRA */
        set     lra + type_OtherPointer, LRA

        /* Indirect closure */
        ld      [LEXENV+CLOSURE_FUNCTION_OFFSET], CODE

        jmp     CODE+FUNCTION_HEADER_CODE_OFFSET
        nop

        .align  8
lra:
        .word   type_ReturnPcHeader

        /* Blow off any extra values. */
        mov     OCFP, CSP
        nop

        /* Return the one value. */
        mov     A0, %i0

        /* Turn on pseudo_atomic */
        set     1, NL1
        sethi   %hi(PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET), L0
        st      NL1, [L0+%lo(PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET)]

        /* Store LISP state */
        store(ALLOC,current_dynamic_space_free_pointer)
        store(BSP,current_binding_stack_pointer)
        store(CSP,current_control_stack_pointer)
        store(CFP,current_control_frame_pointer)

        /* No longer in Lisp. */
        store(NL1,foreign_function_call_active)

        /* Were we interrupted? */
        sethi   %hi(PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET), L0
        ld      [L0+%lo(PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET)], L0
        tst     L0
        beq     1f
        nop

        /* Yep, we were. */
        unimp   trap_PendingInterrupt
1:

        /* Back to C we go. */
	ld	[%sp+FRAMESIZE-4], %i7
        ret
        restore	%sp, FRAMESIZE, %sp



        .global _call_into_c
_call_into_c:
        /* Build a lisp stack frame */
        mov     CFP, OCFP
        mov     CSP, CFP
        add     CSP, 32, CSP
        st      OCFP, [CFP]
        st      CODE, [CFP+8]

        /* Turn on pseudo-atomic. */
        sethi   %hi(PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET), L0
        st      CSP, [L0+%lo(PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET)]

	/* Convert the return address to an offset and save it on the stack. */
	sub	LIP, CODE, L0
	add	L0, type_OtherPointer, L0
	st	L0, [CFP+4]

        /* Store LISP state */
        store(ALLOC,current_dynamic_space_free_pointer)
        store(BSP,current_binding_stack_pointer)
        store(CSP,current_control_stack_pointer)
        store(CFP,current_control_frame_pointer)

        /* No longer in Lisp. */
        store(CSP,foreign_function_call_active)

        /* Were we interrupted? */
        sethi   %hi(PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET), L0
        ld      [L0+%lo(PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET)], L0
        tst     L0
        beq     1f
        nop

        /* Yep, we were. */
        unimp   trap_PendingInterrupt
1:

        /* Into C we go. */
        call    CFUNC
        nop

        /* Re-establish NIL */
        set     NIL, NULLREG

        /* No longer in foreign function call. */
        /* Note: the atomic flag should still be set. */
        sethi   %hi(NAME(foreign_function_call_active)), NL1
        st      ZERO, [NL1+%lo(NAME(foreign_function_call_active))]

        /* Load the rest of lisp state. */
        load(current_dynamic_space_free_pointer, ALLOC)
        load(current_binding_stack_pointer, BSP)
        load(current_control_stack_pointer, CSP)
        load(current_control_frame_pointer, CFP)

	/* Get the return address back. */
	ld	[CFP+4], LIP
	ld	[CFP+8], CODE
	add	LIP, CODE, LIP
	sub	LIP, type_OtherPointer, LIP

        /* No longer atomic. */
        sethi   %hi(PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET), NL1
        st      ZERO, [NL1+%lo(PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET)]

        /* Were we interrupted? */
        sethi   %hi(PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET), NL1
        ld      [NL1+%lo(PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET)], NL1
        tst     NL1
        beq     1f
        nop

        /* Yep, we were. */
        unimp   trap_PendingInterrupt
1:

        /* Reset the lisp stack. */
        /* Note: OCFP is in one of the locals, it gets preserved across C. */
        mov     CFP, CSP
        mov     OCFP, CFP

        /* And back into lisp. */
        ret
        nop




        .global _undefined_tramp
        .align  8
        .byte   0
_undefined_tramp:
        .byte   0, 0, type_FunctionHeader
        .word   _undefined_tramp
        .word   NIL
        .word   NIL
        .word   NIL
        .word   NIL

	b	1f
        unimp   trap_Cerror
	.byte	4, 23, 254, 12, 3
	.align	4
1:
	ld	[CNAME+SYMBOL_RAW_FUNCTION_ADDR_OFFSET], CODE
	jmp	CODE+FUNCTION_HEADER_CODE_OFFSET
	nop

	.global	_closure_tramp
	.align	8
	.byte	0
_closure_tramp:
	.byte	0, 0, type_FunctionHeader
	.word	_closure_tramp
	.word	NIL
        .word   NIL
	.word	NIL
	.word	NIL

	ld	[CNAME+SYMBOL_FUNCTION_OFFSET], LEXENV
	ld	[LEXENV+CLOSURE_FUNCTION_OFFSET], CODE
	jmp	CODE+FUNCTION_HEADER_CODE_OFFSET
	nop



/*
 * Function-end breakpoint magic.
 */

	.text
	.align	8
	.global	function_end_breakpoint_guts
function_end_breakpoint_guts:
	.word	type_ReturnPcHeader
	b	1f
	nop
	mov	4, NARGS
	mov	NULLREG, A1
	mov	NULLREG, A2
	mov	NULLREG, A3
	mov	NULLREG, A4
	mov	NULLREG, A5
1:

	.global	function_end_breakpoint_trap
function_end_breakpoint_trap:
	unimp	trap_FunctionEndBreakpoint
	b	1b
	nop

	.global	function_end_breakpoint_end
function_end_breakpoint_end:




/****************************************************************\

We need our own version of sigtramp.

\****************************************************************/

	.global	__sigtramp, __sigfunc
__sigtramp:
	!
	! On entry sp points to:
	! 	0 - 63: window save area
	!	64: signal number
	!	68: signal code
	!	72: pointer to sigcontext
	!	76: addr parameter
	!
	! A sigcontext looks like:
	!	00: on signal stack flag
	!	04: old signal mask
	!	08: old sp
	!	12: old pc
	!	16: old npc
	!	20: old psr
	!	24: old g1
	!	28: old o0
	!
        ! Out sigcontext has the following added:
        !       32: regs[32]
        !       160: fpregs[32]
        !       288: y
        !       292: fsr
        !
        ! After we allocate space for the new sigcontext, the stack looks like:
        !       < window save area, etc >
        !       SCOFF: start of generic sigcontext.
        !       IREGSOFF: start of lisp sigcontext.
        !       SIGNUM: signal number
        !       CODE: signal code
        !       OSCP: pointer to orig sigcontext
        !       ADDR: addr parameter (which isn't supplied by the kernel)

#define SCOFF SA(MINFRAME)
#define IREGSOFF SCOFF+32
#define FPREGSOFF IREGSOFF+(32*4)
#define YOFF FPREGSOFF+(32*4)
#define FSROFF YOFF+4
#define SCSIZE SA(32+(32*4)+(32*4)+4+4)
#define SIGNUMOFF SCOFF+SCSIZE
#define CODEOFF SCOFF+SCSIZE+4
#define ADDROFF SCOFF+SCSIZE+12

#ifdef MACH
#define OSCOFF ADDROFF
#else
#define OSCOFF SCOFF+SCSIZE+16
#endif

        ! Allocate space for our sigcontext.
        sub     %sp, SCSIZE+SA(MINFRAME)-64, %sp

        ! Save integer registers.
        ! Note: the globals and outs are good, but the locals and ins have
        ! been trashed.  But luckly, they have been saved on the stack.
        ! So we need to extract the saved stack pointer from the sigcontext
        ! to determine where they are.
        std     %g0, [%sp+IREGSOFF]
        std     %g2, [%sp+IREGSOFF+8]
        std     %g4, [%sp+IREGSOFF+16]
        std     %g6, [%sp+IREGSOFF+24]
        std     %o0, [%sp+IREGSOFF+32]
        ld      [%sp + OSCOFF+8], %o0
        std     %o2, [%sp+IREGSOFF+40]
        std     %o4, [%sp+IREGSOFF+48]
        st      %o0, [%sp+IREGSOFF+56]
        st      %o7, [%sp+IREGSOFF+60]

        ldd     [%o0], %l0
        ldd     [%o0+8], %l2
        ldd     [%o0+16], %l4
        ldd     [%o0+24], %l6
        ldd     [%o0+32], %i0
        ldd     [%o0+40], %i2
        ldd     [%o0+48], %i4
        ldd     [%o0+56], %i6
        std     %l0, [%sp+IREGSOFF+64]
        std     %l2, [%sp+IREGSOFF+72]
        std     %l4, [%sp+IREGSOFF+80]
        std     %l6, [%sp+IREGSOFF+88]
        std     %i0, [%sp+IREGSOFF+96]
        std     %i2, [%sp+IREGSOFF+104]
        std     %i4, [%sp+IREGSOFF+112]
        std     %i6, [%sp+IREGSOFF+120]

        ! Copy the original sigcontext down to our sigcontext.
        ld      [%sp + OSCOFF], %l0
        ld      [%sp + OSCOFF+4], %l1
        ld      [%sp + OSCOFF+8], %l2
        ld      [%sp + OSCOFF+12], %l3
        ld      [%sp + OSCOFF+16], %l4
        ld      [%sp + OSCOFF+20], %l5
        ld      [%sp + OSCOFF+24], %l6
        ld      [%sp + OSCOFF+28], %l7
        std     %l0, [%sp+SCOFF]
        std     %l2, [%sp+SCOFF+8]
        std     %l4, [%sp+SCOFF+16]
        std     %l6, [%sp+SCOFF+24]

        ! Check to see if we need to save the fp regs.
	set	PSR_EF, %l0
	mov	%y, %l2			! save y
	btst	%l0, %l5		! is FPU enabled?
	bz	1f			! if not skip FPU save
	st	%l2, [%sp + YOFF]

	std	%f0, [%sp + FPREGSOFF+(0*4)]	! save all fpu registers.
	std	%f2, [%sp + FPREGSOFF+(2*4)]
	std	%f4, [%sp + FPREGSOFF+(4*4)]
	std	%f6, [%sp + FPREGSOFF+(6*4)]
	std	%f8, [%sp + FPREGSOFF+(8*4)]
	std	%f10, [%sp + FPREGSOFF+(10*4)]
	std	%f12, [%sp + FPREGSOFF+(12*4)]
	std	%f14, [%sp + FPREGSOFF+(14*4)]
	std	%f16, [%sp + FPREGSOFF+(16*4)]
	std	%f18, [%sp + FPREGSOFF+(18*4)]
	std	%f20, [%sp + FPREGSOFF+(20*4)]
	std	%f22, [%sp + FPREGSOFF+(22*4)]
	std	%f24, [%sp + FPREGSOFF+(24*4)]
	std	%f26, [%sp + FPREGSOFF+(26*4)]
	std	%f28, [%sp + FPREGSOFF+(28*4)]
	std	%f30, [%sp + FPREGSOFF+(30*4)]
	st	%fsr, [%sp + FSROFF] ! save old fsr
1:

	ld	[%sp + SIGNUMOFF], %o0	! get signal number
	set	__sigfunc, %g1		! get array of function ptrs
	sll	%o0, 2, %g2		! scale signal number for index
	ld	[%g1 + %g2], %g1	! get func
	ld	[%sp + CODEOFF], %o1	! get code
        add     %sp, SCOFF, %o2         ! compute scp
	call	%g1			! (*_sigfunc[sig])(sig,code,scp,addr)
	ld	[%sp + ADDROFF], %o3	! get addr

        ! Recompute scp, and call into _sigreturn
        add     %sp, SCOFF, %o0


        .global _sigreturn
_sigreturn:
        ! Move values we can't restore directory into real sigcontext.
        ld      [%o0+32+(4*1)], %l0     ! g1
        ld      [%o0+32+(4*8)], %l1     ! o0
        ld      [%o0+32+(4*14)], %l2    ! sp
        st      %l0, [%o0+24]
        st      %l1, [%o0+28]
        st      %l2, [%o0+8]

	ld	[%o0 + 20], %l2		! get psr
	set	PSR_EF, %l0
	ld	[%o0 + 288], %l1        ! restore y
	btst	%l0, %l2		! is FPU enabled?
	bz	2f			! if not skip FPU restore
	mov	%l1, %y

	ldd	[%o0 + 160+(0*4)], %f0	! restore all fpu registers.
	ldd	[%o0 + 160+(2*4)], %f2
	ldd	[%o0 + 160+(4*4)], %f4
	ldd	[%o0 + 160+(6*4)], %f6
	ldd	[%o0 + 160+(8*4)], %f8
	ldd	[%o0 + 160+(10*4)], %f10
	ldd	[%o0 + 160+(12*4)], %f12
	ldd	[%o0 + 160+(14*4)], %f14
	ldd	[%o0 + 160+(16*4)], %f16
	ldd	[%o0 + 160+(18*4)], %f18
	ldd	[%o0 + 160+(20*4)], %f20
	ldd	[%o0 + 160+(22*4)], %f22
	ldd	[%o0 + 160+(24*4)], %f24
	ldd	[%o0 + 160+(26*4)], %f26
	ldd	[%o0 + 160+(28*4)], %f28
	ldd	[%o0 + 160+(30*4)], %f30
	ld	[%o0 + 160+(33*4)], %fsr	! restore old fsr
2:

	! The locals and in are restored from the stack, so we have to put
	! them there.
	ld	[%o0 + 8], %o1
        ldd     [%o0 + 32+(16*4)], %l0
        ldd     [%o0 + 32+(18*4)], %l2
        ldd     [%o0 + 32+(20*4)], %l4
        ldd     [%o0 + 32+(22*4)], %l6
        ldd     [%o0 + 32+(24*4)], %i0
        ldd     [%o0 + 32+(26*4)], %i2
        ldd     [%o0 + 32+(28*4)], %i4
        ldd     [%o0 + 32+(30*4)], %i6
	std	%l0, [%o1 + (0*4)]
	std	%l2, [%o1 + (2*4)]
	std	%l4, [%o1 + (4*4)]
	std	%l6, [%o1 + (6*4)]
	std	%i0, [%o1 + (8*4)]
	std	%i2, [%o1 + (10*4)]
	std	%i4, [%o1 + (12*4)]
	std	%i6, [%o1 + (14*4)]

        ! Restore the globals and outs.  Don't restore %g1, %o0, or %sp
	! 'cause they get restored from the sigcontext.
        ldd     [%o0 + 32+(2*4)], %g2
        ldd     [%o0 + 32+(4*4)], %g4
        ldd     [%o0 + 32+(6*4)], %g6
        ld      [%o0 + 32+(9*4)], %o1
        ldd     [%o0 + 32+(10*4)], %o2
        ldd     [%o0 + 32+(12*4)], %o4
        ld      [%o0 + 32+(15*4)], %o7

	set	139, %g1		! sigcleanup system call
	t	0
	unimp	0			! just in case it returns
	/*NOTREACHED*/
