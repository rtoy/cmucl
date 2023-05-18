/*

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
#if defined(GENCGC)
#include "gencgc.h"
#else
#include "cgc.h"
#endif


/*
 * See MAKE-BOGUS-LRA in code/debug-int.lisp for these values.
 *
 * Ideally, internals.h should have the correct values.  We leave
 * these defaults here for now.
 */
#ifndef REAL_LRA_SLOT
#define REAL_LRA_SLOT 0
#endif

#ifndef KNOWN_RETURN_P_SLOT
#ifndef i386
#define KNOWN_RETURN_P_SLOT 1
#else
#define KNOWN_RETURN_P_SLOT 2
#endif
#endif

#ifndef BOGUS_LRA_CONSTANTS
#ifndef i386
#define BOGUS_LRA_CONSTANTS 2
#else
#define BOGUS_LRA_CONSTANTS 3
#endif
#endif


static void *
compute_pc(lispobj code_obj, int pc_offset)
{
    struct code *code;

    code = (struct code *) PTR(code_obj);
    return (void *) ((char *) code + HeaderValue(code->header) * sizeof(lispobj)
		     + pc_offset);
}

unsigned long
breakpoint_install(lispobj code_obj, int pc_offset)
{
    return arch_install_breakpoint(compute_pc(code_obj, pc_offset));
}

void
breakpoint_remove(lispobj code_obj, int pc_offset, unsigned long orig_inst)
{
    arch_remove_breakpoint(compute_pc(code_obj, pc_offset), orig_inst);
}

void
breakpoint_do_displaced_inst(os_context_t * scp, unsigned long orig_inst)
{
#if !defined(hpux) && !defined(irix) && !defined(i386)
    undo_fake_foreign_function_call(scp);
#endif
    arch_do_displaced_inst(scp, orig_inst);
}

#if !defined(i386)
static lispobj
find_code(os_context_t * scp)
{
#ifdef reg_CODE
    lispobj code = SC_REG(scp, reg_CODE), header;

    if (LowtagOf(code) != type_OtherPointer)
	return NIL;

    header = *(lispobj *) (code - type_OtherPointer);

    if (TypeOf(header) == type_CodeHeader)
	return code;
    else
	return code - HeaderValue(header) * sizeof(lispobj);
#else
    return NIL;
#endif
}
#endif

#if defined(i386)
static lispobj
find_code(os_context_t * scp)
{
    lispobj *codeptr = component_ptr_from_pc((lispobj *) SC_PC(scp));

    if (codeptr == NULL)
	return NIL;
    else
	return (lispobj) codeptr | type_OtherPointer;
}
#endif

#if (defined(DARWIN) && defined(__ppc__)) || (defined(sparc))
/*
 * During a function-end-breakpoint, the pc is sometimes less than the
 * code address, which bypasses the function end stuff.  Then the
 * offset is zero for a function-end-breakpoint, and we can't find the
 * breakpoint data, causing an error during tracing.  But we know this
 * is a function-end breakpoint, because function_end is set to true.
 *
 * (This condition of pc < code address seems to occur only if a GC
 * happens during tracing.  I guess the function-end code object
 * sometimes gets moved to a lower address than the corresponding
 * code.)
 *
 * Hence this replacement looks at the function end flag first
 * to see if it is a function-end breakpoint and does the function-end
 * stuff anyway.  If not, we do the normal stuff.
 */
static int
compute_offset(os_context_t * scp, lispobj code, boolean function_end)
{
    if (code == NIL)
	return 0;
    if (function_end) {
        /*
         * We're in a function end breakpoint.  Compute the
         * offset from the (known) breakpoint location and the
         * beginning of the breakpoint guts.  (See *-assem.S.)
         *
         * Then make the offset negative so the caller knows
         * that the offset is not from the code object.
         */
        extern char function_end_breakpoint_trap;
        extern char function_end_breakpoint_guts;
        int offset;
            
        offset =
            &function_end_breakpoint_trap -
            &function_end_breakpoint_guts;
#if 0
        fprintf(stderr, "compute_offset\n");
        fprintf(stderr, " function end offset  = %d\n", offset);
#endif
        return make_fixnum(-offset);
    } else {
	unsigned long code_start;
	struct code *codeptr = (struct code *) PTR(code);
	unsigned long pc = SC_PC(scp);

	code_start = (unsigned long) codeptr
	    + HeaderValue(codeptr->header) * sizeof(lispobj);
#if 0
        fprintf(stderr, "compute_offset\n");
        fprintf(stderr, " pc = %d\n", pc);
        fprintf(stderr, " code_start = %d\n", code_start);
        fprintf(stderr, " function_end = %d\n", function_end);
#endif
        if (pc < code_start) {
	    return 0;
        } else {
	    int offset = pc - code_start;

#if 0
            fprintf(stderr, " offset = %d\n", offset);
            fprintf(stderr, " codeptr->code_size = %d\n", codeptr->code_size);
            fprintf(stderr, " function_end = %d\n", function_end);
#endif
	    if (offset >= codeptr->code_size) {
                return 0;
	    } else {
                return make_fixnum(offset);
            }
	}
    }
}
#else
static int
compute_offset(os_context_t * scp, lispobj code, boolean function_end)
{
    DPRINTF(debug_handlers, (stderr, "compute_offset: code = 0x%lx\n", code));
    
    if (code == NIL)
	return 0;
    else {
	unsigned long code_start;
	struct code *codeptr = (struct code *) PTR(code);

#ifdef parisc
	unsigned long pc = SC_PC(scp) & ~3;
#else
	unsigned long pc = SC_PC(scp);
#endif

	code_start = (unsigned long) codeptr
	    + HeaderValue(codeptr->header) * sizeof(lispobj);

        DPRINTF(debug_handlers,
                (stderr, "compute_offset: pc = 0x%lx, code_start = 0x%lx\n",
                 pc, code_start));
        
	if (pc < code_start)
	    return 0;
	else {
	    int offset = pc - code_start;

            DPRINTF(debug_handlers,
                    (stderr, "compute_offset: offset %d, size = %ld\n",
                     offset, codeptr->code_size));
            
	    if (offset >= codeptr->code_size) {
                return 0;
	    } else {
		return make_fixnum(offset);
	    }
	}
    }
}
#endif

#ifndef i386
void
handle_breakpoint(int signal, int subcode, os_context_t * scp)
{
    lispobj code;

    fake_foreign_function_call(scp);

    code = find_code(scp);

#if 0
    fprintf(stderr, "handle_breakpoint\n");
    fprintf(stderr, " offset = %d\n", compute_offset(scp, code, 0));
#endif    
    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(scp, code, 0), code, alloc_sap(scp));

    undo_fake_foreign_function_call(scp);
}
#else
void
handle_breakpoint(int signal, int subcode, os_context_t * scp)
{
    lispobj code, scp_sap = alloc_sap(scp);

    fake_foreign_function_call(scp);

    code = find_code(scp);

    DPRINTF(debug_handlers,
            (stderr, "handle breakpoint: offset %d\n",
             compute_offset(scp, code, 0)));
    
    /*
     * Don't disallow recursive breakpoint traps.  Otherwise, we can't
     * use debugger breakpoints anywhere in here.
     */

    sigprocmask(SIG_SETMASK, &scp->uc_sigmask, NULL);
    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(scp, code, 0), code, scp_sap);

    undo_fake_foreign_function_call(scp);
}
#endif

#ifndef i386
void *
handle_function_end_breakpoint(int signal, int subcode, os_context_t * scp)
{
    lispobj code, lra;
    struct code *codeptr;
    int offset;
    int known_return_p;
    
    fake_foreign_function_call(scp);

    code = find_code(scp);
    codeptr = (struct code *) PTR(code);
    offset = compute_offset(scp, code, 1);
#if 0
    printf("handle_function_end:\n");
    printf(" code    = 0x%08x\n", code);
    printf(" codeptr = %p\n", codeptr);
    printf(" offset  = %d\n", fixnum_value(offset));
    fflush(stdout);
#endif

    if (offset < 0) {
	/*
	 * We were in the function end breakpoint.  Which means we are
	 * in a bogus LRA, so compute where the code-component of this
	 * bogus lra object starts.  Adjust code, and codeptr
	 * appropriately so the breakpoint handler can do the right
	 * thing.
	 */
	unsigned int pc;

	pc = SC_PC(scp);

	offset = -offset;
	/*
	 * Some magic here.  pc points to the trap instruction.  The
	 * offset gives us where the function_end_breakpoint_guts
	 * begins.  But we need to back up some more to get to the
	 * code-component object.  See MAKE-BOGUS-LRA in
	 * debug-int.lisp
	 */
	code = pc - fixnum_value(offset);
	code -= sizeof(struct code) + BOGUS_LRA_CONSTANTS * sizeof(lispobj);

	code += type_OtherPointer;
	codeptr = (struct code *) PTR(code);
#if 0
	printf("  pc   = 0x%08x\n", pc);
	printf("  code    = 0x%08x\n", code);
	printf("  codeptr = %p\n", codeptr);
	fflush(stdout);
#endif
    }

    lra = codeptr->constants[REAL_LRA_SLOT];

    known_return_p = codeptr->constants[KNOWN_RETURN_P_SLOT] != NIL;
    
    {
	lispobj *args = current_control_stack_pointer;

	/*
	 * Because HANDLE_BREAKPOINT can GC, the LRA could move, and
	 * we need to know where it went so we can return to the
	 * correct place.  We do this by saving the LRA on the Lisp
	 * stack.  If GC moves the LRA, the stack entry will get
	 * updated appropriately too.
	 */
	current_control_stack_pointer += 1;
	args[0] = lra;

        funcall3(SymbolFunction(HANDLE_BREAKPOINT), offset, code, alloc_sap(scp));

	/*
	 * Breakpoint handling done.  Get the (possibly changed) LRA
	 * value off the stack so we know where to return to.
	 */
	lra = args[0];
	current_control_stack_pointer -= 1;

#ifdef reg_CODE	
	/*
	 * With the known-return convention, we definitely do NOT want
	 * to mangle the CODE register because it isn't pointing to
	 * the bogus LRA but to the actual routine.
	 */
	if (!known_return_p) {
	    SC_REG(scp, reg_CODE) = lra;
	}
#endif
    }

    undo_fake_foreign_function_call(scp);
    return (void *) (lra - type_OtherPointer + sizeof(lispobj));
}
#else
void *
handle_function_end_breakpoint(int signal, int subcode, os_context_t * scp)
{
    lispobj code, scp_sap = alloc_sap(scp);
    struct code *codeptr;

    fake_foreign_function_call(scp);

    code = find_code(scp);
    codeptr = (struct code *) PTR(code);

    /*
     * Don't disallow recursive breakpoint traps.  Otherwise, we can't
     * use debugger breakpoints anywhere in here.
     */

    sigprocmask(SIG_SETMASK, &scp->uc_sigmask, NULL);
    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(scp, code, 1), code, scp_sap);

    undo_fake_foreign_function_call(scp);

    return compute_pc(codeptr->constants[REAL_LRA_SLOT],
		      fixnum_value(codeptr->constants[REAL_LRA_SLOT + 1]));
}
#endif
