
#ifdef SOLARIS
#define _ASM
#include <sys/asm_linkage.h>
#include <sys/psw.h>
#include <sys/trap.h>
#ifdef __STDC__
#define FUNCDEF(x)	.type x, \#function
#else
#define FUNCDEF(x)	.type x, #function
#endif
#else
#endif

#define LANGUAGE_ASSEMBLY
#include "lispregs.h"
#include "internals.h"
#include "globals.h"

#define load(sym, reg) \
        sethi %hi(sym), reg; ld [reg+%lo(sym)], reg
#define store(reg, sym) \
        sethi %hi(sym), reg_L0; st reg, [reg_L0+%lo(sym)]

/*
 * Our frame size needs to be large enough to hold our window, 
 * the structure return pointer, and some temp space.  The temp space
 * is to hold the 64-bit %o registers that might be converted to %i 
 * registers.  A task switch will will not preserve all 64 bits of the
 * %i registers, so we need to save our %o registers before entering C.
 * Since %o0 and %o1 contain the return results, we do not have to save 
 * these.
 */
#ifdef v8plus	
#define FRAMESIZE (SA(WINDOWSIZE+4 + 6*8))
#else	
#define FRAMESIZE (SA(MINFRAME))
#endif

/*
 * Tell assembler we're using %g2 and %g3 here.
 */
	.register	%g2, #scratch
	.register	%g3, #scratch


        .seg    "text"
        .global call_into_lisp
	FUNCDEF(call_into_lisp)
call_into_lisp:
        save    %sp, -FRAMESIZE, %sp
	/* Flush all of C's register windows to the stack. */
	ta	ST_FLUSH_WINDOWS

        /* Save the return address. */
        st      %i7, [%fp-4]

        /* Clear the descriptor regs. (See sparc/vm.lisp) */
        mov     reg_ZERO, reg_A0
        mov     reg_ZERO, reg_A1
        mov     reg_ZERO, reg_A2
        mov     reg_ZERO, reg_A3
        mov     reg_ZERO, reg_A4
        mov     reg_ZERO, reg_A5
        mov     reg_ZERO, reg_OCFP
        mov     reg_ZERO, reg_LRA
        mov     reg_ZERO, reg_CODE

        /* Establish NIL */
        set     NIL, reg_NIL

	/* Set the pseudo-atomic flag. */
	set	pseudo_atomic_Value, reg_ALLOC

	/* Turn off foreign function call. */
        sethi   %hi(foreign_function_call_active), reg_NL0
        st      reg_ZERO, [reg_NL0+%lo(foreign_function_call_active)]

        /* Load the rest of lisp state. */
        load(current_dynamic_space_free_pointer, reg_NL0)
	add	reg_NL0, reg_ALLOC, reg_ALLOC
        load(current_binding_stack_pointer, reg_BSP)
        load(current_control_stack_pointer, reg_CSP)
        load(current_control_frame_pointer, reg_OCFP)

        /* No longer atomic, and check for interrupt. */
	andn	reg_ALLOC, pseudo_atomic_Value, reg_ALLOC
	andcc	reg_ALLOC, pseudo_atomic_InterruptedValue, reg_ZERO
	tne	trap_PseudoAtomic

        /* Pass in the args. */
        sll     %i2, 2, reg_NARGS
        mov     %i1, reg_CFP
	mov	%i0, reg_LEXENV
        ld      [reg_CFP+0], reg_A0
        ld      [reg_CFP+4], reg_A1
        ld      [reg_CFP+8], reg_A2
        ld      [reg_CFP+12], reg_A3
        ld      [reg_CFP+16], reg_A4
        ld      [reg_CFP+20], reg_A5

        /* Calculate LRA */
        set     lra + type_OtherPointer, reg_LRA

        /* Indirect closure */
        ld      [reg_LEXENV+CLOSURE_FUNCTION_OFFSET], reg_CODE

        jmp     reg_CODE+FUNCTION_CODE_OFFSET
        nop

        .align  8
lra:
        .word   type_ReturnPcHeader

        /* Blow off any extra values. */
        mov     reg_OCFP, reg_CSP
        nop

        /* Return the one value. */
        mov     reg_A0, %i0

        /* Turn on pseudo_atomic */
	or	reg_ALLOC, pseudo_atomic_Value, reg_ALLOC

        /* Store LISP state */
	andn	reg_ALLOC, lowtag_Mask, reg_NL1
        store(reg_NL1,current_dynamic_space_free_pointer)
        store(reg_BSP,current_binding_stack_pointer)
        store(reg_CSP,current_control_stack_pointer)
        store(reg_CFP,current_control_frame_pointer)

        /* No longer in Lisp. */
        store(reg_NL1,foreign_function_call_active)

        /* Were we interrupted? */
	andn	reg_ALLOC, pseudo_atomic_Value, reg_ALLOC
	andcc	reg_ALLOC, pseudo_atomic_InterruptedValue, reg_ZERO
	tne	trap_PseudoAtomic

        /* Back to C we go. */
	ld	[%sp+FRAMESIZE-4], %i7
        ret
        restore	%sp, FRAMESIZE, %sp
	SET_SIZE(call_into_lisp)



        .global call_into_c
	FUNCDEF(call_into_c)
call_into_c:
#ifdef v8plus
	stx	%o2, [%fp - 8 - 1*8]
	stx	%o3, [%fp - 8 - 2*8]
	stx	%o4, [%fp - 8 - 3*8]
	stx	%o5, [%fp - 8 - 4*8]
	stx	%o6, [%fp - 8 - 5*8]
	stx	%o7, [%fp - 8 - 6*8]
#endif		
        /* Build a lisp stack frame */
        mov     reg_CFP, reg_OCFP
        mov     reg_CSP, reg_CFP
        add     reg_CSP, 32, reg_CSP
        st      reg_OCFP, [reg_CFP]
        st      reg_CODE, [reg_CFP+8]

        /* Turn on pseudo-atomic. */
	or	reg_ALLOC, pseudo_atomic_Value, reg_ALLOC

	/* Convert the return address to an offset and save it on the stack. */
	sub	reg_LIP, reg_CODE, reg_L0
	add	reg_L0, type_OtherPointer, reg_L0
	st	reg_L0, [reg_CFP+4]

        /* Store LISP state */
        store(reg_BSP,current_binding_stack_pointer)
        store(reg_CSP,current_control_stack_pointer)
        store(reg_CFP,current_control_frame_pointer)

	/* Use reg_CFP as a work register, and restore it */
	andn	reg_ALLOC, lowtag_Mask, reg_CFP
        store(reg_CFP,current_dynamic_space_free_pointer)
	load(current_control_frame_pointer, reg_CFP)

        /* No longer in Lisp. */
        store(reg_CSP,foreign_function_call_active)

        /* Were we interrupted? */
	andn	reg_ALLOC, pseudo_atomic_Value, reg_ALLOC
	andcc	reg_ALLOC, pseudo_atomic_InterruptedValue, reg_ZERO
	tne	trap_PseudoAtomic

        /* Into C we go. */
        call    reg_CFUNC
        nop

	/*
	 * Note: C calling conventions (32-bit) say that %o0 and %o1
	 * are used to return function results.  In particular 64-bit
	 * results are in %o0 (hi) and %o1 (low).  
	 */
	
        /* Re-establish NIL */
        set     NIL, reg_NIL

	/* Atomic. */
	set	pseudo_atomic_Value, reg_ALLOC

        /* No longer in foreign function call. */
        sethi   %hi(foreign_function_call_active), reg_NL2
        st      reg_ZERO, [reg_NL2+%lo(foreign_function_call_active)]

        /* Load the rest of lisp state. */
        load(current_dynamic_space_free_pointer, reg_NL2)
	add	reg_NL2, reg_ALLOC, reg_ALLOC
        load(current_binding_stack_pointer, reg_BSP)
        load(current_control_stack_pointer, reg_CSP)
        load(current_control_frame_pointer, reg_CFP)

	/* Get the return address back. */
	ld	[reg_CFP+4], reg_LIP
	ld	[reg_CFP+8], reg_CODE
	add	reg_LIP, reg_CODE, reg_LIP
	sub	reg_LIP, type_OtherPointer, reg_LIP

        /* No longer atomic. */
	andn	reg_ALLOC, pseudo_atomic_Value, reg_ALLOC
	andcc	reg_ALLOC, pseudo_atomic_InterruptedValue, reg_ZERO
	tne	trap_PseudoAtomic

        /* Reset the lisp stack. */
        /* Note: OCFP is in one of the locals, it gets preserved across C. */
        mov     reg_CFP, reg_CSP
        mov     reg_OCFP, reg_CFP

#ifdef v8plus
	ldx	[%fp - 8 - 1*8], %o2
	ldx	[%fp - 8 - 2*8], %o3
	ldx	[%fp - 8 - 3*8], %o4
	ldx	[%fp - 8 - 4*8], %o5
	ldx	[%fp - 8 - 5*8], %o6
	ldx	[%fp - 8 - 6*8], %o7
#endif		
        /* And back into lisp. */
        ret
        nop

	SET_SIZE(call_into_c)

#if 0
/* undefined_tramp and closure_tramp are now Lisp assembly routines.
 * so we don't need these anymore.  Leave them here for a bit so 
 * we can look at the "real" versions for a while.  But eventually,
 * remove these.
 */
        .global _undefined_tramp
	FUNCDEF(_undefined_tramp)
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
        /* Number of argument bytes */
        .byte	4
        .byte	UNDEFINED_SYMBOL_ERROR
        /* Escape to create 16bit number from following two bytes, 
	   in little-endian order */
        .byte	254
        /* SC_OFFSET(sc_DescriptorReg,reg_FDEFN) */
	.byte	SC_OFFSET_LO(sc_DescriptorReg,reg_FDEFN_NUM)
	.byte	SC_OFFSET_HI(sc_DescriptorReg,reg_FDEFN_NUM)
		
	.align	4
1:
	ld	[reg_FDEFN+FDEFN_RAW_ADDR_OFFSET], reg_CODE
	jmp	reg_CODE+FUNCTION_CODE_OFFSET
	nop
	SET_SIZE(_undefined_tramp)

	.global	_closure_tramp
	FUNCDEF(_closure_tramp)
	.align	8
	.byte	0
_closure_tramp:
	.byte	0, 0, type_FunctionHeader
	.word	_closure_tramp
	.word	NIL
        .word   NIL
	.word	NIL
	.word	NIL

	ld	[reg_FDEFN+FDEFN_FUNCTION_OFFSET], reg_LEXENV
	ld	[reg_LEXENV+CLOSURE_FUNCTION_OFFSET], reg_CODE
	jmp	reg_CODE+FUNCTION_CODE_OFFSET
	nop
	SET_SIZE(_closure_tramp)
#endif


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
	mov	reg_CSP, reg_OCFP
	add	4, reg_CSP, reg_CSP
	mov	4, reg_NARGS
	mov	reg_NIL, reg_A1
	mov	reg_NIL, reg_A2
	mov	reg_NIL, reg_A3
	mov	reg_NIL, reg_A4
	mov	reg_NIL, reg_A5
1:

	.global	function_end_breakpoint_trap
function_end_breakpoint_trap:
	unimp	trap_FunctionEndBreakpoint
	b	1b
	nop

	.global	function_end_breakpoint_end
function_end_breakpoint_end:

	.global flush_icache
	FUNCDEF(flush_icache)
flush_icache:
        add %o0,%o1,%o2
1:      iflush %o0			! flush instruction cache
        add %o0,8,%o0
        cmp %o0,%o2
        blt 1b
        nop
	retl				! return from leaf routine
        nop
	SET_SIZE(flush_icache)

        .global do_pending_interrupt
	FUNCDEF(do_pending_interrupt)
do_pending_interrupt:
	unimp trap_PendingInterrupt
	retl
	nop
	SET_SIZE(do_pending_interrupt)

#ifdef trap_DynamicSpaceOverflowError
	.global do_dynamic_space_overflow_error
	FUNCDEF(do_dynamic_space_overflow_error)
do_dynamic_space_overflow_error:	
	unimp trap_DynamicSpaceOverflowError
	retl
	nop
	SET_SIZE(do_dynamic_space_overflow_error)		
#endif				

#ifdef trap_DynamicSpaceOverflowWarning
	.global do_dynamic_space_overflow_warning
	FUNCDEF(do_dynamic_space_overflow_warning)
do_dynamic_space_overflow_warning:	
	unimp trap_DynamicSpaceOverflowWarning
	retl
	nop
	SET_SIZE(do_dynamic_space_overflow_warning)		
#endif				
		
#ifdef LINKAGE_TABLE
/*
 * Call into C code to resolve a linkage entry.
 *
 * We get here by Lisp calling call_into_c with an address of the
 * desired function which is contained in the register reg_CFUNC (aka
 * %i4, aka %r28).  This is the address of the entry in the linkage
 * table, which is what we need to figure out what function we really
 * wanted.
 *
 * Note that because we get here from call_into_c, all necessary live
 * registers have been saved, including FP registers.  Hence, no need
 * to save them.
 */
	.global lazy_resolve_linkage
	.global resolve_linkage_tramp
	FUNCDEF(resolve_linkage_tramp)
resolve_linkage_tramp:
	/*
	 * At this point, all of the global %g registers have been
	 * saved by call_into_c, so we can use them as temps.  %g2,
	 * aka reg_NIL, aka null-tn is a good choice.  reg_L0 contains
	 * the address of the jmpl instruction in the linkage jump
	 * table.  (See sparc-arch.c.)
	 */

	mov	reg_L0, reg_NIL
		
	/* 
	 * New stack frame so the %o regs become %i.  We can't touch
	 * the original %o because they contain the parameters to the
	 * function!
	 */
	save	%sp, -FRAMESIZE, %sp

	/* %g2 tells where we came from in the linkage table */
	call	lazy_resolve_linkage
	mov	reg_NIL, %o0		! in the delay slot

	mov	%o0, reg_NIL
	restore	%sp, FRAMESIZE, %sp

	/* And away we go! */
	jmp	reg_NIL
	nop
	
	SET_SIZE(resolve_linkage_tramp)						
		
	.global undefined_foreign_symbol_trap
	FUNCDEF(undefined_foreign_symbol_trap)
/*
 * When we get called, %o0 contains the address of the data_vector object 
 * which is a string naming the bad symbol.
 */
undefined_foreign_symbol_trap:
	/*
	   Need to restore all the global registers with the Lisp values that 
	   were saved away in call_into_c.  (This routine is only called from 
	   os_link_one_symbol, which is called from resolve_linkage_tramp, which 
	   is called from call_into_c.)

	   The global registers are volatile across function calls, so who
	   knows what values have been they contain now!
		
	*/

	load(current_dynamic_space_free_pointer, reg_ALLOC)
        load(current_binding_stack_pointer, reg_BSP)
        load(current_control_stack_pointer, reg_CSP)
        load(current_control_frame_pointer, reg_CFP)
		
	set	NIL, reg_NIL

	mov	%o0, reg_A0			
	unimp	trap_Error
	.byte	4	/* Number of argument bytes */
	.byte	UNDEFINED_FOREIGN_SYMBOL_ERROR
        /* Escape to create 16bit number from following two bytes, in 
	   little-endian order */
        .byte	254
	.byte	SC_OFFSET_LO(sc_DescriptorReg, reg_A0_NUM)
	.byte	SC_OFFSET_HI(sc_DescriptorReg, reg_A0_NUM)
	.align	4
					
#endif
/*
 * Save the FPU state.  %o0 contains a pointer to where we can 
 * store our state.
 */			

/* 
 * Note we only save the 16 double-float registers (which saves
 * the 32 single-float values too, I think).  If we're compiling for 
 * a sparc v9, the Lisp code can actually use all 32 double-float 
 * registers.  For later.
 */
	.global fpu_save
	FUNCDEF(fpu_save)
fpu_save:			
	std	%f0, [%o0 + 4*0]
	std	%f2, [%o0 + 4*2]	
	std	%f4, [%o0 + 4*4]	
	std	%f6, [%o0 + 4*6]	
	std	%f8, [%o0 + 4*8]	
	std	%f10, [%o0 + 4*10]	
	std	%f12, [%o0 + 4*12]	
	std	%f14, [%o0 + 4*14]	
	std	%f16, [%o0 + 4*16]	
	std	%f18, [%o0 + 4*18]	
	std	%f20, [%o0 + 4*20]	
	std	%f22, [%o0 + 4*22]	
	std	%f24, [%o0 + 4*24]	
	std	%f26, [%o0 + 4*26]	
	std	%f28, [%o0 + 4*28]
	std	%f30, [%o0 + 4*30]	
#ifdef FEATURE_SPARC_V9
	std	%f32, [%o0 + 4*32]	
	std	%f34, [%o0 + 4*34]	
	std	%f36, [%o0 + 4*36]	
	std	%f38, [%o0 + 4*38]	
	std	%f40, [%o0 + 4*40]	
	std	%f42, [%o0 + 4*42]	
	std	%f44, [%o0 + 4*44]	
	std	%f46, [%o0 + 4*46]	
	std	%f48, [%o0 + 4*48]	
	std	%f50, [%o0 + 4*50]	
	std	%f52, [%o0 + 4*52]	
	std	%f54, [%o0 + 4*54]	
	std	%f56, [%o0 + 4*56]	
	std	%f58, [%o0 + 4*58]	
	std	%f60, [%o0 + 4*60]	
	std	%f62, [%o0 + 4*62]	
	st	%fsr, [%o0 + 4*64]
#else
	st	%fsr, [%o0 + 4*32]
#endif
	retl
	nop
	SET_SIZE(fpu_save)						

	.global fpu_restore
	FUNCDEF(fpu_restore)
fpu_restore:			
	ldd	[%o0 + 4*0], %f0
	ldd	[%o0 + 4*2], %f2	
	ldd	[%o0 + 4*4], %f4	
	ldd	[%o0 + 4*6], %f6	
	ldd	[%o0 + 4*8], %f8	
	ldd	[%o0 + 4*10], %f10	
	ldd	[%o0 + 4*12], %f12	
	ldd	[%o0 + 4*14], %f14	
	ldd	[%o0 + 4*16], %f16	
	ldd	[%o0 + 4*18], %f18	
	ldd	[%o0 + 4*20], %f20	
	ldd	[%o0 + 4*22], %f22	
	ldd	[%o0 + 4*24], %f24	
	ldd	[%o0 + 4*26], %f26	
	ldd	[%o0 + 4*28], %f28
	ldd	[%o0 + 4*30], %f30	
#ifdef FEATURE_SPARC_V9
	ldd	[%o0 + 4*32], %f32	
	ldd	[%o0 + 4*34], %f34	
	ldd	[%o0 + 4*36], %f36	
	ldd	[%o0 + 4*38], %f38	
	ldd	[%o0 + 4*40], %f40	
	ldd	[%o0 + 4*42], %f42	
	ldd	[%o0 + 4*44], %f44	
	ldd	[%o0 + 4*46], %f46	
	ldd	[%o0 + 4*48], %f48	
	ldd	[%o0 + 4*50], %f50	
	ldd	[%o0 + 4*52], %f52	
	ldd	[%o0 + 4*54], %f54	
	ldd	[%o0 + 4*56], %f56	
	ldd	[%o0 + 4*58], %f58	
	ldd	[%o0 + 4*60], %f60	
	ldd	[%o0 + 4*62], %f62	
	ld	[%o0 + 4*64], %fsr
#else
	ld	[%o0 + 4*32], %fsr
#endif
	retl
	nop
	SET_SIZE(fpu_restore)						

	.global save_context
	FUNCDEF(save_context)
save_context:
	ta	ST_FLUSH_WINDOWS	! flush register windows
	retl				! return from leaf routine
	nop
	SET_SIZE(save_context)
/*
 * Local variables:
 * tab-width: 8
 * End:
 */						
		
