/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/breakpoint.c,v 1.7 1997/11/25 15:53:30 dtc Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#include <signal.h>

#include "lisp.h"
#include "os.h"
#include "internals.h"
#include "interrupt.h"
#include "arch.h"
#include "lispregs.h"
#include "globals.h"
#include "alloc.h"
#include "breakpoint.h"

#define REAL_LRA_SLOT 0
#define KNOWN_RETURN_P_SLOT 1
#define BOGUS_LRA_CONSTANTS 2

static void *compute_pc(lispobj code_obj, int pc_offset)
{
    struct code *code;

    code = (struct code *)PTR(code_obj);
    return (void *)((char *)code + HeaderValue(code->header)*sizeof(lispobj)
		    + pc_offset);
}

unsigned long breakpoint_install(lispobj code_obj, int pc_offset)
{
    return arch_install_breakpoint(compute_pc(code_obj, pc_offset));
}

void breakpoint_remove(lispobj code_obj, int pc_offset,
		       unsigned long orig_inst)
{
    arch_remove_breakpoint(compute_pc(code_obj, pc_offset), orig_inst);
}

void breakpoint_do_displaced_inst(struct sigcontext *scp,
				  unsigned long orig_inst)
{
#if !defined(hpux) && !defined(irix) && !defined(i386)
    undo_fake_foreign_function_call(scp);
#endif
    arch_do_displaced_inst(scp, orig_inst);
}

#ifndef i386
static lispobj find_code(struct sigcontext *scp)
{
#ifdef reg_CODE
    lispobj code = SC_REG(scp, reg_CODE), header;

    if (LowtagOf(code) != type_OtherPointer)
	return NIL;

    header = *(lispobj *)(code-type_OtherPointer);

    if (TypeOf(header) == type_CodeHeader)
	return code;
    else
	return code - HeaderValue(header)*sizeof(lispobj);
#else
    return NIL;
#endif
}
#endif

#ifdef i386
static lispobj find_code(struct sigcontext *scp)
{
  lispobj codeptr = component_ptr_from_pc(SC_PC(scp));

  if (codeptr==NULL)
    return NIL;
  return (codeptr+type_OtherPointer);
}
#endif

static int compute_offset(struct sigcontext *scp, lispobj code)
{
    if (code == NIL)
	return 0;
    else {
	unsigned long code_start;
	struct code *codeptr = (struct code *)PTR(code);
#ifdef parisc
	unsigned long pc = SC_PC(scp) & ~3;
#else
	unsigned long pc = SC_PC(scp);
#endif

	code_start = (unsigned long)codeptr
	    + HeaderValue(codeptr->header)*sizeof(lispobj);
	if (pc < code_start)
	    return 0;
	else {
	    int offset = pc - code_start;
	    if (offset >= codeptr->code_size)
		return 0;
	    else
		return make_fixnum(offset);
	}
    }
}

void handle_breakpoint(int signal, int subcode, struct sigcontext *scp)
{
    lispobj code, scp_sap=alloc_sap(scp);

    fake_foreign_function_call(scp);

    code = find_code(scp);

#ifdef i386
    /* Don't disallow recursive breakpoint traps.  Otherwise, we can't */
    /* use debugger breakpoints anywhere in here. */
    sigsetmask(scp->sc_mask);
#endif

    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(scp, code),
	     code,
	     scp_sap);

    undo_fake_foreign_function_call(scp);
}

void *handle_function_end_breakpoint(int signal, int subcode,
				     struct sigcontext *scp)
{
    lispobj code, lra, scp_sap=alloc_sap(scp);
    struct code *codeptr;

    fake_foreign_function_call(scp);

    code = find_code(scp);
    codeptr = (struct code *)PTR(code);

#ifdef i386
    /* Don't disallow recursive breakpoint traps.  Otherwise, we can't */
    /* use debugger breakpoints anywhere in here. */
    sigsetmask(scp->sc_mask);
#endif

    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(scp, code),
	     code,
	     scp_sap);

    lra = codeptr->constants[REAL_LRA_SLOT];
#ifdef reg_CODE
    if (codeptr->constants[KNOWN_RETURN_P_SLOT] == NIL)
	SC_REG(scp, reg_CODE) = lra;
#endif

    undo_fake_foreign_function_call(scp);

#ifdef i386
    /* On the x86 the saved lra is a SAP; extract the return
       address. */
    if (!Pointerp(lra) || !(LowtagOf(lra)==type_OtherPointer))
      fprintf(stderr,"* Return address not a SAP!\n");
    {
      struct sap *sap = (struct sap *)PTR(lra);
      if (TypeOf(sap->header)!=type_Sap)
	fprintf(stderr,"* Return address not a SAP!\n");
      return (void *)(sap->pointer);
    }
#else
    return (void *)(lra-type_OtherPointer+sizeof(lispobj));
#endif
}
