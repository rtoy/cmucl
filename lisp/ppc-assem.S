#define LANGUAGE_ASSEMBLY

#include "internals.h"
#include "lispregs.h"
#include "globals.h"

#if defined DARWIN
#define FUNCDEF(x)	.text @ \
			.align 3 @ \
_##x:
#define GFUNCDEF(x)	.globl _/**/x @ \
	FUNCDEF(x)
#else
#define FUNCDEF(x)	.text ; \
			.align 3 ; \
			.type x,@function ; \
x:
#define GFUNCDEF(x)	.globl _/**/x @ \
	FUNCDEF(x)
#endif

#if defined DARWIN
#define SET_SIZE(x)
#else
#define SET_SIZE(x) .size x,.-x
#endif

/* Load a register from a global, using the register as an intermediary */
/* The register will be a fixnum for one instruction, so this is gc-safe */

#if defined DARWIN
#define load(reg,global) \
	lis reg,ha16(global) @ \
	lwz reg,lo16(global)(reg) ; Comment
#define store(reg,temp,global) \
	lis temp,ha16(global) @\
	stw reg,lo16(global)(temp) ; Comment
#else
#define load(reg,global) \
	lis reg,global@ha; lwz reg,global@l(reg)
#define store(reg,temp,global) \
	lis temp,global@ha; stw reg,global@l(temp)
#endif
	
#define	FIRST_SAVE_FPR	14	/* lowest-numbered non-volatile FPR */
#define	FIRST_SAVE_GPR	13	/* lowest-numbered non-volatile GPR */
#define	NFPR_SAVE_BYTES(n) ((32-(n))*8)
#if defined DARWIN
#define NGPR_SAVE_BYTES(n) ((32-(n))*4)
#define FRAME_ARG_BYTES(n)  (((((n)+6)*4)+15)&~15)
#else
#define NGPR_SAVE_BYTES(n) ((32-(~1&((n)+1)))*4)
#define FRAME_ARG_BYTES(n)  (((((n)+2)*4)+15)&~15)
#endif

#if defined DARWIN
#define FRAME_SIZE(first_g,first_f,out_arg_words,savecr) \
(NFPR_SAVE_BYTES(first_f)+ NGPR_SAVE_BYTES(first_g)+ FRAME_ARG_BYTES(out_arg_words))
#define SAVE_FPR(n) stfd f##n,-8*(32- n)(r11)
#define SAVE_GPR(n) stw r##n,-4*(32- n)(r11)
#define FULL_FRAME_SIZE FRAME_SIZE(FIRST_SAVE_GPR,FIRST_SAVE_FPR,8,1)
#else
#define FRAME_SIZE(first_g,first_f,out_arg_words,savecr) \
(NFPR_SAVE_BYTES(first_f)+ NGPR_SAVE_BYTES(first_g)+ FRAME_ARG_BYTES(out_arg_words+savecr))
#define SAVE_FPR(n) stfd n,-8*(32-(n))(11)
#define SAVE_GPR(n) stw n,-4*(32-(n))(11)
#define FULL_FRAME_SIZE FRAME_SIZE(FIRST_SAVE_GPR,FIRST_SAVE_FPR,0,1)
#endif
	
#if defined DARWIN
#define RESTORE_FPR(n) lfd f##n,-8*(32- n)(r11)
#define RESTORE_GPR(n) lwz r##n,-4*(32- n)(r11)
#else
#define RESTORE_FPR(n) lfd f##n,-8*(32-(r##n))(r11)
#define RESTORE_GPR(n) lwz r##n,-4*(32-(r##n))(r11)
#endif
#define C_FULL_PROLOG \
	nop @\
	nop @ \
	mfcr REG(0) @ \
	stw REG(0),4(REG(1)) @ \
	mflr REG(0) @ \
	stw REG(0),8(REG(1)) @ \
	mr REG(11),REG(1) @ \
	stwu REG(1),-FULL_FRAME_SIZE(REG(1)) @ \
	SAVE_FPR(14) @ \
	SAVE_FPR(15) @ \
	SAVE_FPR(16) @ \
	SAVE_FPR(17) @ \
	SAVE_FPR(18) @ \
	SAVE_FPR(19) @ \
	SAVE_FPR(20) @ \
	SAVE_FPR(21) @ \
	SAVE_FPR(22) @ \
	SAVE_FPR(23) @ \
	SAVE_FPR(24) @ \
	SAVE_FPR(25) @ \
	SAVE_FPR(26) @ \
	SAVE_FPR(27) @ \
	SAVE_FPR(28) @ \
	SAVE_FPR(29) @ \
	SAVE_FPR(30) @ \
	SAVE_FPR(31) @ \
	la REG(11),-NFPR_SAVE_BYTES(FIRST_SAVE_FPR)(REG(11)) @ \
	SAVE_GPR(13) @ \
	SAVE_GPR(14) @ \
	SAVE_GPR(15) @ \
	SAVE_GPR(16) @ \
	SAVE_GPR(17) @ \
	SAVE_GPR(18) @ \
	SAVE_GPR(19) @ \
	SAVE_GPR(20) @ \
	SAVE_GPR(21) @ \
	SAVE_GPR(22) @ \
	SAVE_GPR(23) @ \
	SAVE_GPR(24) @ \
	SAVE_GPR(25) @ \
	SAVE_GPR(26) @ \
	SAVE_GPR(27) @ \
	SAVE_GPR(28) @ \
	SAVE_GPR(29) @ \
	SAVE_GPR(30) @ \
	SAVE_GPR(31)


#define C_FULL_EPILOG \
	la REG(11),FULL_FRAME_SIZE-NFPR_SAVE_BYTES(FIRST_SAVE_FPR)(REG(1)) @ \
	RESTORE_GPR(13) @ \
	RESTORE_GPR(14) @ \
	RESTORE_GPR(15) @ \
	RESTORE_GPR(16) @ \
	RESTORE_GPR(17) @ \
	RESTORE_GPR(18) @ \
	RESTORE_GPR(19) @ \
	RESTORE_GPR(20) @ \
	RESTORE_GPR(21) @ \
	RESTORE_GPR(22) @ \
	RESTORE_GPR(23) @ \
	RESTORE_GPR(24) @ \
	RESTORE_GPR(25) @ \
	RESTORE_GPR(26) @ \
	RESTORE_GPR(27) @ \
	RESTORE_GPR(28) @ \
	RESTORE_GPR(29) @ \
	RESTORE_GPR(30) @ \
	RESTORE_GPR(31) @ \
	la REG(11),NFPR_SAVE_BYTES(FIRST_SAVE_FPR)(REG(11)) @ \
	RESTORE_FPR(14) @ \
	RESTORE_FPR(15) @ \
	RESTORE_FPR(16) @ \
	RESTORE_FPR(17) @ \
	RESTORE_FPR(18) @ \
	RESTORE_FPR(19) @ \
	RESTORE_FPR(20) @ \
	RESTORE_FPR(21) @ \
	RESTORE_FPR(22) @ \
	RESTORE_FPR(23) @ \
	RESTORE_FPR(24) @ \
	RESTORE_FPR(25) @ \
	RESTORE_FPR(26) @ \
	RESTORE_FPR(27) @ \
	RESTORE_FPR(28) @ \
	RESTORE_FPR(29) @ \
	RESTORE_FPR(30) @ \
	RESTORE_FPR(31) @ \
	lwz REG(1),0(REG(1)) @ \
	lwz REG(0),4(REG(1)) @ \
	mtcr REG(0) @ \
	lwz REG(0),8(REG(1)) @ \
	mtlr REG(0) @ \
	


	
	.text

/*
 * Function to transfer control into lisp.  The lisp object to invoke is
 * passed as the first argument, which puts it in NL0
 */

	GFUNCDEF(call_into_lisp)
	C_FULL_PROLOG
	/* store(reg_POLL,11,saver2) */
	/* Initialize tagged registers */
	li reg_ZERO,0
	li reg_CODE,0
	li reg_CNAME,0
	li reg_LEXENV,0
	li reg_FDEFN,0
	li reg_OCFP,0
	li reg_LRA,0
	li reg_A0,0
	li reg_A1,0
	li reg_A2,0
	li reg_A3,0
	li reg_L0,0
	li reg_L1,0
	li reg_L2,0
	li reg_LIP,0
	lis reg_NULL,hi16(NIL)
	ori reg_NULL,reg_NULL,lo16(NIL)

	/* Turn on pseudo-atomic */

	li reg_NL3,-4
	li reg_ALLOC,4
	store(reg_ZERO,reg_NL4,_foreign_function_call_active)
	load(reg_NL4,_current_dynamic_space_free_pointer)
	add reg_ALLOC,reg_ALLOC,reg_NL4
	load(reg_BSP,_current_binding_stack_pointer)
	load(reg_CSP,_current_control_stack_pointer)
	load(reg_OCFP,_current_control_frame_pointer)

	/* No longer atomic, and check for interrupt */
	add reg_ALLOC,reg_ALLOC,reg_NL3
	twlti reg_ALLOC,0

	/* Pass in the arguments */

	mr reg_CFP,reg_NL1
	mr reg_LEXENV,reg_NL0
	lwz reg_A0,0(reg_CFP)
	lwz reg_A1,4(reg_CFP)
	lwz reg_A2,8(reg_CFP)
	lwz reg_A3,12(reg_CFP)

	/* Calculate LRA */
	lis reg_LRA,ha16(lra)
	addi reg_LRA,reg_LRA,lo16(lra)
	addi reg_LRA,reg_LRA,type_OtherPointer

	/* Function is an indirect closure */
	lwz reg_CODE,CLOSURE_FUNCTION_OFFSET(reg_LEXENV)
#ifdef PPC_FUN-HACK
	mtctr reg_CODE
#else
	addi reg_LIP,reg_CODE,6*4-type_FunctionPointer
	mtctr reg_LIP
#endif
	slwi reg_NARGS,reg_NL2,2
	bctr
	.align 3
lra:
	.long type_ReturnPcHeader

	/* Blow off any extra values. */
	mr reg_CSP,reg_OCFP
	nop

	/* Return the one value. */

	mr REG(3),reg_A0

	/* Turn on  pseudo-atomic */
	li reg_NL3,-4
	la reg_ALLOC,4(reg_ALLOC)

	/* Store lisp state */
	clrrwi reg_NL1,reg_ALLOC,3
	store(reg_NL1,reg_NL2,_current_dynamic_space_free_pointer)
	/* store(reg_POLL,reg_NL2,poll_flag) */
	/* load(reg_NL2,current_thread) */
	store(reg_BSP,reg_NL2,_current_binding_stack_pointer)
	store(reg_CSP,reg_NL2,_current_control_stack_pointer)
	store(reg_CFP,reg_NL2,_current_control_frame_pointer)
	/* load(reg_POLL,saver2) */

	/* No longer in Lisp. */
	store(reg_NL1,reg_NL2,_foreign_function_call_active)

	/* Check for interrupt */
	add reg_ALLOC,reg_ALLOC,reg_NL3
	twlti reg_ALLOC,0

	/* Back to C */
	C_FULL_EPILOG
	blr
	SET_SIZE(call_into_lisp)


	GFUNCDEF(call_into_c)
	/* We're kind of low on unboxed, non-dedicated registers here:
	most of the unboxed registers may have outgoing C args in them.
	CFUNC is going to have to go in the CTR in a moment, anyway
	so we'll free it up soon.  reg_NFP is preserved by lisp if it
	has a meaningful value in it, so we can use it.  reg_NARGS is
	free when it's not holding a copy of the "real" reg_NL3, which
	gets tied up by the pseudo-atomic mechanism */
	mtctr reg_CFUNC
	mflr reg_LIP
	/* Build a lisp stack frame */
	mr reg_OCFP,reg_CFP
	mr reg_CFP,reg_CSP
	la reg_CSP,32(reg_CSP)
	stw reg_OCFP,0(reg_CFP)
	stw reg_CODE,8(reg_CFP)
	/* The pseudo-atomic mechanism wants to use reg_NL3, but that
	may be an outgoing C argument.  Copy reg_NL3 to something that's
	unboxed and -not- one of the C argument registers */
	mr reg_NARGS,reg_NL3
	
	/* Turn on pseudo-atomic */
	li reg_NL3,-4
	la reg_ALLOC,4(reg_ALLOC)

	/* Convert the return address to an offset and save it on the stack. */
	sub reg_NFP,reg_LIP,reg_CODE
	la reg_NFP,type_OtherPointer(reg_NFP)
	stw reg_NFP,4(reg_CFP)

	/* Store Lisp state */
	clrrwi reg_NFP,reg_ALLOC,3
	store(reg_NFP,reg_CFUNC,_current_dynamic_space_free_pointer)
	/* load(reg_CFUNC,current_thread) */
	
	store(reg_BSP,reg_CFUNC,_current_binding_stack_pointer)
	store(reg_CSP,reg_CFUNC,_current_control_stack_pointer)
	store(reg_CFP,reg_CFUNC,_current_control_frame_pointer)

	/* No longer in Lisp */
	store(reg_CSP,reg_CFUNC,_foreign_function_call_active)
	/* load(reg_POLL,saver2) */
	/* Disable pseudo-atomic; check pending interrupt */
	add reg_ALLOC,reg_ALLOC,reg_NL3
	twlti reg_ALLOC,0
	mr reg_NL3,reg_NARGS

	/* PowerOpen (i.e. OS X) requires the callee address in r12
           (a.k.a. CFUNC), so move it back there, too. */
	mfctr reg_CFUNC
        /* Into C we go. */
	bctrl

	/* Re-establish NIL */
	lis reg_NULL,hi16(NIL)
	ori reg_NULL,reg_NULL,lo16(NIL)
	/* And reg_ZERO */
	li reg_ZERO,0

	/* If we GC'ed during the FF code (as the result of a callback ?)
	the tagged lisp registers may now contain garbage (since the
	registers were saved by C and not seen by the GC.)  Put something
	harmless in all such registers before allowing an interrupt */
	li reg_CODE,0
	li reg_CNAME,0
	li reg_LEXENV,0
	/* reg_OCFP was pointing to a control stack frame & was preserved by C */
	li reg_LRA,0
	li reg_A0,0
	li reg_A1,0
	li reg_A2,0
	li reg_A3,0
	li reg_L0,0
	li reg_L1,0
	li reg_L2,0
	li reg_LIP,0

	/* Atomic ... */
	li reg_NL3,-4
	li reg_ALLOC,4

	/* No long in foreign function call. */
	store(reg_ZERO,reg_NL2,_foreign_function_call_active)

	/* The free pointer may have moved */
	load(reg_NL4,_current_dynamic_space_free_pointer)
	add reg_ALLOC,reg_ALLOC,reg_NL4

	/* The BSP wasn't preserved by C, so load it */
	load(reg_BSP,_current_binding_stack_pointer)

	/* Other lisp stack/frame pointers were preserved by C.
	I can't imagine why they'd have moved */

	/* Get the return address back. */
	lwz reg_LIP,4(reg_CFP)
	lwz reg_CODE,8(reg_CFP)
	add reg_LIP,reg_CODE,reg_LIP
	la reg_LIP,-type_OtherPointer(reg_LIP)

	/* No longer atomic */
	add reg_ALLOC,reg_ALLOC,reg_NL3
	twlti reg_ALLOC,0
	mtlr reg_LIP
	
	/* Reset the lisp stack. */
	mr reg_CSP,reg_CFP
	mr reg_CFP,reg_OCFP
	
	/* And back into Lisp. */
	blr

	SET_SIZE(call_into_c)

	GFUNCDEF(xundefined_tramp)
	.globl _undefined_tramp
	.byte 0,0,0,type_FunctionHeader

	/*
	 * The next 4 bytes are the encoding for a PPC jump instruction.
	 * The jump should go to the twllei instruction below.
	 */
	.byte 18<<2
_undefined_tramp:	
	.byte 0,0,24
	.long _undefined_tramp
	.long NIL
	.long NIL
	.long NIL
	.long NIL
	twllei reg_ZERO,trap_Cerror
        /* Number of argument bytes */
	.byte 4
	.byte UNDEFINED_SYMBOL_ERROR
        /* Escape to create 16bit number from following two bytes, 
	   in little-endian order */
	.byte 254
	.byte SC_OFFSET_LO(sc_DescriptorReg, reg_FDEFN_NUM)
	.byte SC_OFFSET_HI(sc_DescriptorReg, reg_FDEFN_NUM)
	/*
	.byte sc_DescriptorReg+0x40
	.byte 1
	*/
	.align 2
#ifdef PPC_FUN_HACK_MAYBE
1:	lwz reg_CODE,FDEFN_RAW_ADDR_OFFSET(reg_CNAME)
#else
1:	lwz reg_CODE,FDEFN_RAW_ADDR_OFFSET(reg_FDEFN)
#endif
	la reg_LIP,FUNCTION_CODE_OFFSET(reg_CODE)
	mtctr reg_LIP
	bctr

	mr reg_CSP, reg_CFP
	b 1b
	
	SET_SIZE(xundefined_tramp)

	GFUNCDEF(xclosure_tramp)
	.globl _closure_tramp
	.byte 0,0,0,type_FunctionHeader
	.byte 18<<2
_closure_tramp:
	.byte 0,0,24
	.long _closure_tramp
	.long NIL 
	.long NIL
	.long NIL
	.long NIL
#ifdef PPC_FUN_HACK_MAYBE
	lwz reg_LEXENV,FDEFN_FUNCTION_OFFSET(reg_CNAME)
#else
	lwz reg_LEXENV,FDEFN_FUNCTION_OFFSET(reg_FDEFN)
#endif
	lwz reg_CODE,CLOSURE_FUNCTION_OFFSET(reg_LEXENV)
	la reg_LIP,FUNCTION_CODE_OFFSET(reg_CODE)
	mtctr reg_LIP
	bctr

	SET_SIZE(xclosure_tramp)

	GFUNCDEF(function_end_breakpoint_trap)
	.long 0
	SET_SIZE(function_end_breakpoint_trap)

	GFUNCDEF(function_end_breakpoint)
	.long 0
	SET_SIZE(function_end_breakpoint)

	GFUNCDEF(function_end_breakpoint_guts)
	.long 0
	SET_SIZE(function_end_breakpoint_guts)

	GFUNCDEF(function_end_breakpoint_end)
	.long 0
	SET_SIZE(function_end_breakpoint_end)


	GFUNCDEF(ppc_flush_cache_line)
	dcbf 0,REG(3)
	sync
	icbi 0,REG(3)
	sync
	isync
	blr
	SET_SIZE(ppc_flush_cache_line)

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
	.globl _lazy_resolve_linkage
	GFUNCDEF(resolve_linkage_tramp)
	/*
	 * We need to call lazy_resolve_linkage.  reg_A0 (= r24)
	 * (see ppc-arch.c) contains the address of the linkage entry.  
	 */
	/*
	 * Need to save all parameter regs to the stack because we 
	 * need them for later.
	 */
	subi	r1, r1, 8*4+8
	stw	r3, 0(r1)
	stw	r4, 4(r1)
	stw	r5, 8(r1)
	stw	r6, 12(r1)
	stw	r7, 16(r1)
	stw	r8, 20(r1)
	stw	r9, 24(r1)
	stw	r10, 28(r1)
	
	mr	reg_NL0, reg_A0

	stwu	r1, (-(24+4))(r1)
	/*
	 * At this point reg_NIL should be available to us.  
	 * Call lazy_resolve_linkage to figure out the real function address.
	 */
	lis	reg_NULL, hi16(_lazy_resolve_linkage)
	ori	reg_NULL, reg_NULL, lo16(_lazy_resolve_linkage)
	mtctr	reg_NULL
	mflr	reg_CODE
	bctrl

	addi	r1,r1,24+4
	
	/*
	 * The desired function is in r3 (NL0), so save it
	 * and restore the real arg parameters
	 */

	mtctr	reg_NL0
	lwz	r3, 0(r1)
	lwz	r4, 4(r1)
	lwz	r5, 8(r1)
	lwz	r6, 12(r1)
	lwz	r7, 16(r1)
	lwz	r8, 20(r1)
	lwz	r9, 24(r1)
	lwz	r10, 28(r1)

	/*
	 * Got it, so we can now jump directly to the desired function.
	 * reg_NL0 contains the result.  Restore the stack and go!
	 */

	mtlr	reg_CODE
	
	/* Back to C */
	/*	mtlr	reg_CODE*/
	bctr
	
	SET_SIZE(resolve_linkage_tramp)
	

/*
 * When we get called, r3 (reg_NL0) contains the address of the
 * data_vector object which is a string naming the bad symbol.
 */
	GFUNCDEF(undefined_foreign_symbol_trap)

	/*
	   Need to restore all the global registers with the Lisp values that 
	   were saved away in call_into_c.  (This routine is only called from 
	   os_link_one_symbol, which is called from resolve_linkage_tramp, which 
	   is called from call_into_c.)

	   The global registers are volatile across function calls, so who
	   knows what values have been they contain now!
		
	*/

	load(reg_ALLOC, _current_dynamic_space_free_pointer)
        load(reg_BSP, _current_binding_stack_pointer)
        load(reg_CSP, _current_control_stack_pointer)
        load(reg_CFP, _current_control_frame_pointer)

	lis reg_NULL,hi16(NIL)
	ori reg_NULL,reg_NULL,lo16(NIL)
	/* And reg_ZERO */
	li reg_ZERO,0

	mr	reg_NL0, reg_A0			
	twllei	reg_ZERO, trap_Error
	.byte	4	/* Number of argument bytes */
	.byte	UNDEFINED_FOREIGN_SYMBOL_ERROR
        /* Escape to create 16bit number from following two bytes, in 
	   little-endian order */
        .byte	254
	.byte	SC_OFFSET_LO(sc_DescriptorReg, reg_A0_NUM)
	.byte	SC_OFFSET_HI(sc_DescriptorReg, reg_A0_NUM)
	.align	4
					
#endif	