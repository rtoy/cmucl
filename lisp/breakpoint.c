#include <stdio.h>
#include <signal.h>

#include "lisp.h"
#include "os.h"
#include "internals.h"
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
    arch_do_displaced_inst(scp, orig_inst);
}

static lispobj find_code(struct sigcontext *scp)
{
#ifdef CODE
    lispobj code = SC_REG(scp, CODE), header;

    if (LowtagOf(code) != type_OtherPointer)
	return NIL;

    header = *(lispobj *)PTR(code);

    if (TypeOf(header) == type_CodeHeader)
	return code;
    else
	return code - HeaderValue(code)*sizeof(lispobj);
#else
    return NIL;
#endif
}

static void internal_handle_breakpoint(struct sigcontext *scp, lispobj code)
{
    int offset;

    if (code == NIL)
	offset = 0;
    else {
	unsigned long code_start;
	struct code *codeptr = (struct code *)PTR(code);

	code_start = (unsigned long)codeptr
	    + HeaderValue(codeptr->header)*sizeof(lispobj);
	if (SC_PC(scp) < code_start)
	    offset = 0;
	else {
	    offset = SC_PC(scp) - code_start;
	    if (offset >= codeptr->code_size)
		offset = 0;
	}
    }

    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     make_fixnum(offset),
	     code,
	     alloc_sap(scp));
    scp->sc_mask = sigblock(0);
}

void handle_breakpoint(int signal, int subcode, struct sigcontext *scp)
{
    internal_handle_breakpoint(scp, find_code(scp));
}

void *handle_function_end_breakpoint(int signal, int subcode,
				     struct sigcontext *scp)
{
    lispobj code = find_code(scp);
    struct code *codeptr = (struct code *)PTR(code);
    lispobj lra;

    internal_handle_breakpoint(scp, code);

    lra = codeptr->constants[REAL_LRA_SLOT];
#ifdef CODE
    if (codeptr->constants[KNOWN_RETURN_P_SLOT] == NIL)
	SC_REG(scp, CODE) = lra;
#endif
    return (void *)(lra - type_OtherPointer+sizeof(lispobj));
}
