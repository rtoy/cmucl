/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/backtrace.c,v 1.1 1990/04/04 18:26:27 ch Exp $
 *
 * Simple backtrace facility.  More or less from Rob's lisp version.
 */

#include <stdio.h>
#include "lisp.h"
#include "globals.h"

/* Sigh ... I know what the call frame looks like and it had
   better not change. */

struct call_frame {
	struct call_frame *old_cont;
	lispobj saved_lra;
	lispobj other_state[6];
};

#define HEADER_LENGTH(header) ((header)>>8)

static struct code *
code_pointer(object)
lispobj object;
{
	lispobj *headerp, header;
	int type;

	headerp = (lispobj *) PTR(object);
	header = *headerp;
	type = TypeOf(header);

	switch (type) {
	case type_CodeHeader:
		break;
	case type_ReturnPcHeader:
	case type_FunctionHeader:
	case type_ClosureFunctionHeader:
		headerp -= HEADER_LENGTH(header);
		break;
	default:
		return (struct code *) 0;
	}

	return (struct code *) headerp;
}

static
struct call_frame *
current_cont()
{
	return ((struct call_frame *) current_control_stack_pointer) - 1;
}

static
cs_valid_pointer_p(pointer)
struct call_frame *pointer;
{
	return (((char *) control_stack <= (char *) pointer) &&
		((char *) pointer < (char *) current_control_stack_pointer));
}

void
backtrace(nframes)
int nframes;
{
	struct call_frame *callee;

	callee = current_cont();
	while ((callee != (struct call_frame *) 0) && (nframes-- > 0)) {
		struct call_frame *caller;
		lispobj lra;
		struct code *code;
		
		if (!cs_valid_pointer_p(callee)) {
			printf("Bogus callee value (0x%08x).\n",
			       (unsigned long) callee);
			return;
		}

		if (callee->old_cont == (struct call_frame *) 0)
			return;

		lra = callee->saved_lra;
		code = code_pointer(lra);

		printf("<Frame 0x%08x, CODE: 0x%08x, ",
		       (unsigned long) callee, (unsigned long) code);
		if (code != (struct code *) 0) {
			lispobj function;

			function = code->entry_points;
			while (function != NIL) {
				struct function_header *header;
				lispobj name;

				header = (struct function_header *) PTR(function);
				name = header->name;

				if (LowtagOf(name) == type_OtherPointer) {
					lispobj *object;

					object = (lispobj *) PTR(name);

					if (TypeOf(*object) == type_SymbolHeader) {
						struct symbol *symbol;

						symbol = (struct symbol *) object;
						object = (lispobj *) PTR(symbol->name);
					}
					if (TypeOf(*object) == type_SimpleString) {
						struct vector *string;
						
						string = (struct vector *) object;
						printf("%s, ", (char *) string->data);
					} else
						printf("(Not simple string???), ");
				} else
					printf("(Not other pointer???), ");


				function = header->next;
			}
		}
		printf("LRA: 0x%08x, PC: 0x%04x>\n",
		       lra, (PTR(lra) - (lispobj) code) / sizeof(lispobj));

		callee = callee->old_cont;
	}
}
