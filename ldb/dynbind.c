/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/dynbind.c,v 1.2 1990/03/29 21:13:23 ch Exp $
 * 
 * Support for dynamic binding from C.
 */

#include "ldb.h"
#include "lisp.h"
#include "globals.h"

bind_variable(symbol, value)
lispobj symbol, value;
{
	lispobj old_value;
	struct binding *binding;

	old_value = SymbolValue(symbol);
	binding = (struct binding *) current_binding_stack_pointer;
	current_binding_stack_pointer += (sizeof(struct binding) / sizeof(lispobj));
	binding->value = old_value;
	binding->symbol = symbol;
	SetSymbolValue(symbol, value);
}

unbind()
{
	struct binding *binding;
	lispobj symbol;
	
	binding = ((struct binding *) current_binding_stack_pointer) - 1;
		
	symbol = binding->symbol;

	SetSymbolValue(symbol, binding->value);

	binding->symbol = 0;

	current_binding_stack_pointer -= (sizeof(struct binding) / sizeof(lispobj));
}

unbind_to_here(bsp)
lispobj *bsp;
{
	while (bsp > current_binding_stack_pointer) {
		struct binding *binding;
		lispobj symbol;

		binding = ((struct binding *) current_binding_stack_pointer) - 1;
		
		symbol = binding->symbol;

		if (symbol) {
			SetSymbolValue(symbol, binding->value);
			binding->symbol = 0;
		}

		current_binding_stack_pointer -=
			(sizeof(struct binding) / sizeof(lispobj));
	}
}
