/*
 * This code written as part of the CMUCL project and has been placed
 * in the public domain.
 *
 * AArch64 (ARM64) assembly support for Linux (e.g. Fedora).
 *
 * Derived from arm-assem.S, sparc-assem.S, and ppc-assem.S.
 *
 * ABI notes (AAPCS64):
 *   - Arguments:   x0..x7  (integer/pointer), v0..v7 (FP/SIMD)
 *   - Return vals: x0, x1  (integer/pointer), v0, v1 (FP)
 *   - Callee-saved GPRs: x19..x28, x29 (FP), x30 (LR)
 *   - Callee-saved FP:   d8..d15
 *   - x16, x17: intra-procedure-call scratch (IP0, IP1)
 *   - x18: platform register (avoid on Linux)
 *   - SP must be 16-byte aligned at all public interfaces
 *   - Frame record: [x29, x30] stored at top of frame
 */

#define LANGUAGE_ASSEMBLY

#include "internals.h"
#include "lispregs.h"
#include "globals.h"

/* -----------------------------------------------------------------------
 * Macro: GFUNCDEF(x)
 *   Emit a globally-visible, ELF-typed function symbol.
 * ----------------------------------------------------------------------- */
#define GFUNCDEF(x)		\
	.globl x		; \
	.align 2		; \
	.type  x, %function	; \
x:

/* -----------------------------------------------------------------------
 * Macro: ENDFUNC(x)
 *   Close out an ELF function (sets .size).
 *   No literal pool needed on AArch64 (LDR pseudo expands automatically).
 * ----------------------------------------------------------------------- */
#define ENDFUNC(x)		\
	.size x, .-x

/* -----------------------------------------------------------------------
 * Macro: NOT_IMPLEMENTED(name)
 *   Emit an undefined-instruction trap annotated with a name string.
 *   AArch64 has a full 16-bit immediate field in the UDF encoding,
 *   so we use udf directly (as on ARM32) rather than brk.
 * ----------------------------------------------------------------------- */
#define NOT_IMPLEMENTED(name)		\
	udf	#trap_NotImplemented	; \
	b	1f			; \
	.ascii	name			; \
	.align	4, 0			; \
1:

	.text

/* =======================================================================
 * call_into_lisp(function, args, count)
 *
 * C prototype:
 *   lispobj call_into_lisp(lispobj fun, lispobj *args, int count);
 *
 * Per AAPCS64:
 *   x0 = function  (→ reg_NL0, which is x2 per lispregs.h; adjusted below)
 *   x1 = args
 *   x2 = count
 *
 * We save all callee-saved GPRs (x19..x30) and FP regs (d8..d15) on
 * entry so the C caller's state is fully preserved across the Lisp call.
 * ======================================================================= */
GFUNCDEF(call_into_lisp)

	/* ------------------------------------------------------------------
	 * Save callee-saved registers.
	 * We need 10 GPR pairs: (x19,x20) … (x29,x30) = 80 bytes GPR
	 * plus 4 FP pairs: (d8,d9) … (d14,d15) = 64 bytes FP
	 * Total = 144 bytes; round to 160 for 16-byte alignment.
	 * Frame layout (SP+0 is lowest address):
	 *   [SP+  0] d8  [SP+  8] d9
	 *   [SP+ 16] d10 [SP+ 24] d11
	 *   [SP+ 32] d12 [SP+ 40] d13
	 *   [SP+ 48] d14 [SP+ 56] d15
	 *   [SP+ 64] x19 [SP+ 72] x20
	 *   [SP+ 80] x21 [SP+ 88] x22
	 *   [SP+ 96] x23 [SP+104] x24
	 *   [SP+112] x25 [SP+120] x26
	 *   [SP+128] x27 [SP+136] x28
	 *   [SP+144] x29 [SP+152] x30  (frame record)
	 * ------------------------------------------------------------------ */
	sub	sp,  sp,  #160
	stp	d8,  d9,  [sp, #0]
	stp	d10, d11, [sp, #16]
	stp	d12, d13, [sp, #32]
	stp	d14, d15, [sp, #48]
	stp	x19, x20, [sp, #64]
	stp	x21, x22, [sp, #80]
	stp	x23, x24, [sp, #96]
	stp	x25, x26, [sp, #112]
	stp	x27, x28, [sp, #128]
	stp	x29, x30, [sp, #144]
	add	x29, sp,  #144		/* set frame pointer */

	/* ------------------------------------------------------------------
	 * Establish NIL (reg_NULL = x26).
	 * ------------------------------------------------------------------ */
	ldr	reg_NULL, =NIL

	/* ------------------------------------------------------------------
	 * Zero all tagged Lisp registers so the GC never sees junk.
	 * ------------------------------------------------------------------ */
	mov	reg_CODE,   #0
	mov	reg_A0,     #0
	mov	reg_A1,     #0
	mov	reg_A2,     #0
	mov	reg_LRA,    #0
	mov	reg_LEXENV, #0
	mov	reg_CFP,    #0
	mov	reg_LIP,    #0

	/* ------------------------------------------------------------------
	 * Turn on pseudo-atomic and load Lisp state.
	 * reg_ALLOC = current_dynamic_space_free_pointer + pseudo_atomic_Value
	 * ------------------------------------------------------------------ */
	/* Turn off foreign function call. */
	ldr	reg_NL0, =foreign_function_call_active
	str	xzr, [reg_NL0]

	/* Load the rest of Lisp state. */
	ldr	reg_NL0, =current_dynamic_space_free_pointer
	ldr	reg_NL0, [reg_NL0]
	add	reg_ALLOC, reg_NL0, #pseudo_atomic_Value
	ldr	reg_NL0, =current_binding_stack_pointer
	ldr	reg_BSP, [reg_NL0]
	ldr	reg_NL0, =current_control_stack_pointer
	ldr	reg_CSP, [reg_NL0]
	ldr	reg_NL0, =current_control_frame_pointer
	ldr	reg_OCFP, [reg_NL0]

	/* ------------------------------------------------------------------
	 * No longer atomic; check for interrupt.
	 * Strip pseudo_atomic_Value, test interrupted bit, trap if set.
	 * ------------------------------------------------------------------ */
	bic	reg_NL0, reg_ALLOC, #pseudo_atomic_Value
	tst	reg_NL0, #pseudo_atomic_InterruptedValue
	b.eq	1f
	udf	#trap_PseudoAtomic
1:	mov	reg_ALLOC, reg_NL0

	/* ------------------------------------------------------------------
	 * Set up the current frame.
	 * ------------------------------------------------------------------ */
	mov	reg_CFP, reg_OCFP

	/* ------------------------------------------------------------------
	 * Pass in arguments.
	 * AAPCS64: x0=function, x1=args ptr, x2=count.
	 * Per lispregs.h for AArch64:
	 *   reg_NL0 = x0  (holds 'function' on entry)
	 *   reg_NL1 = x1  (holds 'args' on entry)
	 *   reg_NARGS = x2 (holds 'count' on entry — already in place)
	 *
	 * The function pointer is the closure, so copy to reg_LEXENV.
	 * Args are accessed through reg_CFP (set above).
	 * ------------------------------------------------------------------ */
	mov	reg_LEXENV, reg_NL0	/* function → LEXENV */

	/* Load up to 3 args from the args array (reg_NL1 / x1 on entry).
	 * We use reg_CFP which was loaded from current_control_frame_pointer;
	 * the args pointer (x1) was saved in reg_NL1 before zeroing.
	 * For AArch64 we follow the ARM 32-bit pattern: args are positive
	 * offsets (64-bit words) from the args pointer.               */
	cmp	reg_NARGS, #1
	b.lt	1f
	ldr	reg_A0, [reg_NL1, #0]
1:
	cmp	reg_NARGS, #2
	b.lt	2f
	ldr	reg_A1, [reg_NL1, #8]
2:
	cmp	reg_NARGS, #3
	b.lt	3f
	ldr	reg_A2, [reg_NL1, #16]
3:

	/* ------------------------------------------------------------------
	 * Indirect closure: load code object from LEXENV.
	 * ------------------------------------------------------------------ */
	ldr	reg_CODE, [reg_LEXENV, #CLOSURE_FUNCTION_OFFSET]

	/* ------------------------------------------------------------------
	 * Calculate LRA (return address tag).
	 * adr gives PC-relative address; we tag it as OtherPointer.
	 * ------------------------------------------------------------------ */
	adr	reg_LRA, lra
	add	reg_LRA, reg_LRA, #type_OtherPointer

	/* ------------------------------------------------------------------
	 * Convert arg count to fixnum (shift left by 3 on 64-bit CMUCL,
	 * matching the lowtag width; use 2 if the port uses 32-bit fixnums).
	 * ------------------------------------------------------------------ */
	lsl	reg_NARGS, reg_NARGS, #2

	/* ------------------------------------------------------------------
	 * Switch to the Lisp control stack pointer.
	 * ------------------------------------------------------------------ */
	ldr	reg_NL0, =current_control_stack_pointer
	ldr	reg_CSP, [reg_NL0]

	/* ------------------------------------------------------------------
	 * Jump into Lisp code (FUNCTION_CODE_OFFSET into the code object).
	 * ------------------------------------------------------------------ */
	add	reg_NL0, reg_CODE, #FUNCTION_CODE_OFFSET
	br	reg_NL0

	/* ------------------------------------------------------------------
	 * LRA — Lisp return point.
	 * Must be aligned to 8 bytes and prefixed with type_ReturnPcHeader.
	 * ------------------------------------------------------------------ */
	.align	3
lra:
	.quad	type_ReturnPcHeader

	NOT_IMPLEMENTED("call_into_lisp_return_to_c")

	/* ------------------------------------------------------------------
	 * Blow off any extra values.
	 * ------------------------------------------------------------------ */
	mov	reg_CSP, reg_OCFP

	/* Return the one value to C in x0. */
	mov	x0, reg_A0

	/* ------------------------------------------------------------------
	 * Turn on pseudo-atomic.
	 * ------------------------------------------------------------------ */
	add	reg_ALLOC, reg_ALLOC, #pseudo_atomic_Value

	/* ------------------------------------------------------------------
	 * Store Lisp state.
	 * Strip lowtag bits from reg_ALLOC to get the clean free pointer.
	 * ------------------------------------------------------------------ */
	bic	reg_NL0, reg_ALLOC, #lowtag_Mask
	ldr	reg_NL1, =current_dynamic_space_free_pointer
	str	reg_NL0, [reg_NL1]
	ldr	reg_NL1, =current_binding_stack_pointer
	str	reg_BSP, [reg_NL1]
	ldr	reg_NL1, =current_control_stack_pointer
	str	reg_CSP, [reg_NL1]
	ldr	reg_NL1, =current_control_frame_pointer
	str	reg_CFP, [reg_NL1]

	/* No longer in Lisp. */
	ldr	reg_NL1, =foreign_function_call_active
	str	reg_NL0, [reg_NL1]

	/* ------------------------------------------------------------------
	 * No longer atomic; check for interrupt.
	 * ------------------------------------------------------------------ */
	bic	reg_NL0, reg_ALLOC, #pseudo_atomic_Value
	tst	reg_NL0, #pseudo_atomic_InterruptedValue
	b.eq	2f
	udf	#trap_PseudoAtomic
2:	mov	reg_ALLOC, reg_NL0

	/* ------------------------------------------------------------------
	 * Restore callee-saved registers and return to C.
	 * ------------------------------------------------------------------ */
	ldp	d8,  d9,  [sp, #0]
	ldp	d10, d11, [sp, #16]
	ldp	d12, d13, [sp, #32]
	ldp	d14, d15, [sp, #48]
	ldp	x19, x20, [sp, #64]
	ldp	x21, x22, [sp, #80]
	ldp	x23, x24, [sp, #96]
	ldp	x25, x26, [sp, #112]
	ldp	x27, x28, [sp, #128]
	ldp	x29, x30, [sp, #144]
	add	sp,  sp,  #160
	ret
	ENDFUNC(call_into_lisp)


/* =======================================================================
 * call_into_c
 *
 * Called from Lisp to invoke a C function.  The C function address is
 * expected in reg_CFUNC (see lispregs.h).  Any integer arguments that
 * would go in x0..x7 must already be in those registers (placed there
 * by the Lisp caller).
 *
 * Stack layout on entry (Lisp stack grows downward):
 *   The Lisp caller pushes arguments; we do NOT pop them here —
 *   AAPCS64 argument passing is purely register-based for the first 8
 *   integer args, and the Lisp VOPs should have set up x0..x7 already.
 * ======================================================================= */
GFUNCDEF(call_into_c)

	/* ------------------------------------------------------------------
	 * Build a Lisp stack frame to preserve unwinding state.
	 * Frame layout (reg_CFP points here after setup):
	 *   [reg_CFP +  0] saved reg_OCFP
	 *   [reg_CFP +  8] saved LIP offset (return address as tagged offset)
	 *   [reg_CFP + 16] saved reg_CODE
	 * ------------------------------------------------------------------ */
	mov	reg_OCFP, reg_CFP
	mov	reg_CFP,  reg_CSP
	add	reg_CSP,  reg_CSP, #32
	str	reg_OCFP, [reg_CFP]
	str	reg_CODE, [reg_CFP, #16]

	/* ------------------------------------------------------------------
	 * Turn on pseudo-atomic.
	 * ------------------------------------------------------------------ */
	add	reg_ALLOC, reg_ALLOC, #pseudo_atomic_Value

	/* ------------------------------------------------------------------
	 * Convert the Lisp return address (reg_LIP) to a code-relative
	 * offset, tagged as OtherPointer, and save it in the frame.
	 * ------------------------------------------------------------------ */
	sub	reg_NL0, reg_LIP, reg_CODE
	add	reg_NL0, reg_NL0, #type_OtherPointer
	str	reg_NL0, [reg_CFP, #8]

	/* ------------------------------------------------------------------
	 * Store Lisp state.
	 * Strip lowtag bits from reg_ALLOC to get the clean free pointer.
	 * ------------------------------------------------------------------ */
	bic	reg_NL0, reg_ALLOC, #lowtag_Mask
	ldr	reg_NFP, =current_dynamic_space_free_pointer
	str	reg_NL0, [reg_NFP]
	ldr	reg_NFP, =current_binding_stack_pointer
	str	reg_BSP, [reg_NFP]
	ldr	reg_NFP, =current_control_stack_pointer
	str	reg_CSP, [reg_NFP]
	ldr	reg_NFP, =current_control_frame_pointer
	str	reg_CFP, [reg_NFP]

	/* No longer in Lisp. */
	ldr	reg_NFP, =foreign_function_call_active
	str	reg_CSP, [reg_NFP]

	/* ------------------------------------------------------------------
	 * Disable pseudo-atomic; check for pending interrupt.
	 * ------------------------------------------------------------------ */
	bic	reg_NL0, reg_ALLOC, #pseudo_atomic_Value
	tst	reg_NL0, #pseudo_atomic_InterruptedValue
	b.eq	1f
	udf	#trap_PseudoAtomic
1:	mov	reg_ALLOC, reg_NL0

	/* ------------------------------------------------------------------
	 * Call the C function.  Its address is in reg_CFUNC.
	 * AAPCS64: integer arguments are already in x0..x7.
	 * ------------------------------------------------------------------ */
	blr	reg_CFUNC

	/* ------------------------------------------------------------------
	 * Integer results are in x0 (x1 for 128-bit); do NOT clobber.
	 * ------------------------------------------------------------------ */

	/* ------------------------------------------------------------------
	 * Re-establish NIL and zero tagged registers that C may have trashed.
	 * (The GC could have run during a callback; don't leave stale tagged
	 *  pointers in Lisp registers before we re-enable interrupts.)
	 * ------------------------------------------------------------------ */
	ldr	reg_NULL, =NIL
	mov	reg_CODE,   #0
	mov	reg_LEXENV, #0
	mov	reg_LRA,    #0
	mov	reg_A0,     #0
	mov	reg_A1,     #0
	mov	reg_A2,     #0
	mov	reg_A3,     #0
	mov	reg_LIP,    #0

	/* ------------------------------------------------------------------
	 * Atomic — set pseudo_atomic_Value in reg_ALLOC.
	 * ------------------------------------------------------------------ */
	add	reg_ALLOC, reg_ALLOC, #pseudo_atomic_Value

	/* No longer in foreign function call. */
	ldr	reg_NL0, =foreign_function_call_active
	str	xzr, [reg_NL0]

	/* The free pointer may have moved (e.g. due to a GC callback). */
	ldr	reg_NL0, =current_dynamic_space_free_pointer
	ldr	reg_NL0, [reg_NL0]
	add	reg_ALLOC, reg_NL0, #pseudo_atomic_Value

	/* The BSP wasn't preserved by C; reload it. */
	ldr	reg_NL0, =current_binding_stack_pointer
	ldr	reg_BSP, [reg_NL0]

	/* Restore Lisp stack and frame pointers (preserved across C call). */
	ldr	reg_NL0, =current_control_stack_pointer
	ldr	reg_CSP, [reg_NL0]
	ldr	reg_NL0, =current_control_frame_pointer
	ldr	reg_CFP, [reg_NL0]

	/* ------------------------------------------------------------------
	 * Restore Lisp return address and code pointer from the frame.
	 * ------------------------------------------------------------------ */
	ldr	reg_NL0,  [reg_CFP, #8]
	ldr	reg_CODE, [reg_CFP, #16]
	add	reg_LIP,  reg_NL0, reg_CODE
	sub	reg_LIP,  reg_LIP, #type_OtherPointer

	/* ------------------------------------------------------------------
	 * No longer atomic; check for interrupt.
	 * ------------------------------------------------------------------ */
	bic	reg_NL0, reg_ALLOC, #pseudo_atomic_Value
	tst	reg_NL0, #pseudo_atomic_InterruptedValue
	b.eq	2f
	udf	#trap_PseudoAtomic
2:	mov	reg_ALLOC, reg_NL0

	/* ------------------------------------------------------------------
	 * Reset the Lisp stack and return to Lisp.
	 * ------------------------------------------------------------------ */
	mov	reg_CSP,  reg_CFP
	mov	reg_CFP,  reg_OCFP

	br	reg_LIP
	ENDFUNC(call_into_c)


/* =======================================================================
 * undefined_tramp
 *
 * Trampoline for calls to undefined functions.  Laid out with a Lisp
 * function header so the GC can treat it as a proper function object.
 * ======================================================================= */
	.globl	undefined_tramp
	.byte	type_FunctionHeader
undefined_tramp:
	.byte	0, 0, 0
	.quad	undefined_tramp		/* self slot */
	.quad	NIL			/* next slot */
	.quad	NIL			/* name slot */
	.quad	NIL			/* arglist slot */
	.quad	NIL			/* type slot */

	NOT_IMPLEMENTED("undefined_tramp")


/* =======================================================================
 * closure_tramp
 *
 * Trampoline for calls through an FDEFN whose function slot holds a
 * closure rather than a simple function.
 * ======================================================================= */
	.globl	closure_tramp
	.byte	type_FunctionHeader
closure_tramp:
	.byte	0, 0, 0			/* header padding */
	.quad	closure_tramp
	.quad	NIL
	.quad	NIL
	.quad	NIL
	.quad	NIL

	NOT_IMPLEMENTED("closure_tramp")


/* =======================================================================
 * Function-end breakpoint magic.
 * See MAKE-BOGUS-LRA in code/debug-int.lisp.
 * ======================================================================= */
	.globl	function_end_breakpoint_guts
	.align	3
function_end_breakpoint_guts:
	.quad	type_ReturnPcHeader
	NOT_IMPLEMENTED("function_end_breakpoint_guts")

	.globl	function_end_breakpoint_trap
function_end_breakpoint_trap:
	NOT_IMPLEMENTED("function_end_breakpoint_trap")

	.globl	function_end_breakpoint_end
function_end_breakpoint_end:


/* =======================================================================
 * do_pending_interrupt
 *
 * Trigger the pending-interrupt trap so the runtime can deliver a
 * deferred signal.  Matches the pattern from ppc-assem.S.
 * ======================================================================= */
GFUNCDEF(do_pending_interrupt)
	udf	#trap_PendingInterrupt
	ret
	ENDFUNC(do_pending_interrupt)


#ifdef LINKAGE_TABLE
/* =======================================================================
 * resolve_linkage_tramp
 *
 * Lazy linkage resolver.  Called (via call_into_c) when a linkage-table
 * entry has not yet been resolved.  reg_CFUNC holds the address of the
 * unresolved linkage entry; we call lazy_resolve_linkage(entry_addr) and
 * then jump to the resolved address.
 *
 * Because we arrive here through call_into_c all Lisp state has already
 * been saved.  We do need to preserve the C argument registers x0..x7
 * and FP argument registers v0..v7 across the resolver call.
 * ======================================================================= */
	.globl	lazy_resolve_linkage
	.globl	resolve_linkage_tramp
GFUNCDEF(resolve_linkage_tramp)

	/* ------------------------------------------------------------------
	 * Save C argument registers (x0..x7) and FP argument regs (d0..d7).
	 * Allocate: 8 GPRs × 8 + 8 FP × 8 = 128 bytes; keep 16-byte aligned.
	 * ------------------------------------------------------------------ */
	sub	sp, sp, #128
	stp	x0,  x1,  [sp, #0]
	stp	x2,  x3,  [sp, #16]
	stp	x4,  x5,  [sp, #32]
	stp	x6,  x7,  [sp, #48]
	stp	d0,  d1,  [sp, #64]
	stp	d2,  d3,  [sp, #80]
	stp	d4,  d5,  [sp, #96]
	stp	d6,  d7,  [sp, #112]

	/* ------------------------------------------------------------------
	 * Save LR (x30) and set up a mini-frame (16 bytes).
	 * ------------------------------------------------------------------ */
	stp	x29, x30, [sp, #-16]!
	mov	x29, sp

	/* ------------------------------------------------------------------
	 * reg_CFUNC (the linkage entry address) → first argument x0.
	 * ------------------------------------------------------------------ */
	mov	x0, reg_CFUNC

	/* ------------------------------------------------------------------
	 * Call lazy_resolve_linkage; result (resolved address) in x0.
	 * ------------------------------------------------------------------ */
	bl	lazy_resolve_linkage

	/* ------------------------------------------------------------------
	 * Stash resolved address in a callee-saved reg before restoring args.
	 * ------------------------------------------------------------------ */
	mov	x16, x0			/* IP0 = resolved function address */

	/* ------------------------------------------------------------------
	 * Restore frame pointer and link register.
	 * ------------------------------------------------------------------ */
	ldp	x29, x30, [sp], #16

	/* ------------------------------------------------------------------
	 * Restore argument registers.
	 * ------------------------------------------------------------------ */
	ldp	x0,  x1,  [sp, #0]
	ldp	x2,  x3,  [sp, #16]
	ldp	x4,  x5,  [sp, #32]
	ldp	x6,  x7,  [sp, #48]
	ldp	d0,  d1,  [sp, #64]
	ldp	d2,  d3,  [sp, #80]
	ldp	d4,  d5,  [sp, #96]
	ldp	d6,  d7,  [sp, #112]
	add	sp, sp, #128

	/* ------------------------------------------------------------------
	 * Jump directly to the resolved function.
	 * ------------------------------------------------------------------ */
	br	x16
	ENDFUNC(resolve_linkage_tramp)


/* =======================================================================
 * undefined_foreign_symbol_trap
 *
 * Called (indirectly via resolve_linkage_tramp → os_link_one_symbol)
 * when a foreign symbol cannot be resolved.  x0 holds the address of
 * a Lisp string naming the missing symbol.
 *
 * We need to restore all global Lisp registers because they may have
 * been clobbered by the C resolver calls.
 * ======================================================================= */
GFUNCDEF(undefined_foreign_symbol_trap)

	/* Restore global Lisp state */
	ldr	reg_NL0, =current_dynamic_space_free_pointer
	ldr	reg_ALLOC, [reg_NL0]
	ldr	reg_NL0, =current_binding_stack_pointer
	ldr	reg_BSP, [reg_NL0]
	ldr	reg_NL0, =current_control_stack_pointer
	ldr	reg_CSP, [reg_NL0]
	ldr	reg_NL0, =current_control_frame_pointer
	ldr	reg_CFP, [reg_NL0]

	ldr	reg_NULL, =NIL

	/* x0 already holds the symbol name (first C arg); move to reg_A0 */
	mov	reg_A0, x0

	udf	#trap_Error
	.byte	4		/* number of argument bytes */
	.byte	UNDEFINED_FOREIGN_SYMBOL_ERROR
	/* Escape to create 16-bit number from the following two bytes,
	   little-endian order */
	.byte	254
	.byte	SC_OFFSET_LO(sc_DescriptorReg, reg_A0_NUM)
	.byte	SC_OFFSET_HI(sc_DescriptorReg, reg_A0_NUM)
	.align	4
	ENDFUNC(undefined_foreign_symbol_trap)

#endif /* LINKAGE_TABLE */


/* =======================================================================
 * flush_icache(void *addr, unsigned long nbytes)
 *
 * Flush the instruction cache for the range [addr, addr+nbytes).
 * On Linux/AArch64 the recommended approach is the __clear_cache
 * syscall interface, but we can also use the DC/IC instructions
 * directly.  Use the DC CVAU + IC IVAU sequence per the ARMv8-A manual.
 *
 * x0 = start address, x1 = byte count
 * ======================================================================= */
GFUNCDEF(flush_icache)
	cbz	x1, 2f			/* nothing to do if count == 0 */

	/* Read the cache geometry from the CTR_EL0 system register */
	mrs	x3, ctr_el0
	/* D-cache line size: bits [19:16], encoded as log2(words) */
	ubfx	x2, x3, #16, #4
	mov	x4, #4
	lsl	x2, x4, x2		/* x2 = d-cache line size in bytes */
	/* I-cache line size: bits [3:0] */
	and	x3, x3, #0xf
	lsl	x3, x4, x3		/* x3 = i-cache line size in bytes */

	/* Align start down to d-cache line boundary */
	sub	x4, x2, #1
	bic	x4, x0, x4

	/* DC CVAU loop */
1:	dc	cvau, x4
	add	x4, x4, x2
	cmp	x4, x0
	add	x5, x0, x1		/* end = start + count */
	cmp	x4, x5
	b.lt	1b

	dsb	ish			/* ensure DC ops complete */

	/* Align start down to i-cache line boundary */
	sub	x4, x3, #1
	bic	x4, x0, x4

	/* IC IVAU loop */
1:	ic	ivau, x4
	add	x4, x4, x3
	add	x5, x0, x1
	cmp	x4, x5
	b.lt	1b

	dsb	ish
	isb				/* synchronize instruction pipeline */
2:	ret
	ENDFUNC(flush_icache)
