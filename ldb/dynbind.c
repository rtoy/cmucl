/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/dynbind.c,v 1.3 1991/02/16 00:59:56 wlott Exp $
 * 
 * Support for dynamic binding from C.
 */

#include "ldb.h"
#include "lisp.h"
#include "globals.h"

#ifdef ibmrt
#define GetBSP() ((struct binding *)SymbolValue(BINDING_STACK_POINTER))
#define SetBSP(value) SetSymbolValue(BINDING_STACK_POINTER, (lispobj)(value))
#else
#define GetBSP() ((struct binding *)current_binding_stack_pointer)
#define SetBSP(value) (current_binding_stack_pointer=(lispobj *)(value))
#endif

bind_variable(symbol, value)
lispobj symbol, value;
{
	lispobj old_value;
	struct binding *binding;

	old_value = SymbolValue(symbol);
	binding = GetBSP();
	SetBSP(binding+1);

	binding->value = old_value;
	binding->symbol = symbol;
	SetSymbolValue(symbol, value);
}

unbind()
{
	struct binding *binding;
	lispobj symbol;
	
	binding = GetBSP() - 1;
		
	symbol = binding->symbol;

	SetSymbolValue(symbol, binding->value);

	binding->symbol = 0;

	SetBSP(binding);
}

unbind_to_here(bsp)
lispobj *bsp;
{
    struct binding *target = (struct binding *)bsp;
    struct binding *binding = GetBSP();
    lispobj symbol;

    while (target < binding) {
	binding--;

	symbol = binding->symbol;

	if (symbol) {
	    SetSymbolValue(symbol, binding->value);
	    binding->symbol = 0;
	}

    }
    SetBSP(binding);
}
