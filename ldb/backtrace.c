/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/backtrace.c,v 1.2 1990/04/05 00:45:06 wlott Exp $
 *
 * Simple backtrace facility.  More or less from Rob's lisp version.
 */

#include <stdio.h>
#include <signal.h>
#include "ldb.h"
#include "lisp.h"
#include "globals.h"
#include "interrupt.h"
#include "lispregs.h"

/* Sigh ... I know what the call frame looks like and it had
   better not change. */

struct call_frame {
	struct call_frame *old_cont;
	lispobj saved_lra;
	lispobj other_state[6];
};

struct call_info {
    struct call_frame *frame;
    struct code *code;
    lispobj lra;
    int pc; /* Note: this is the trace file offset, not the actual pc. */
};

#define HEADER_LENGTH(header) ((header)>>8)

static struct code *
code_pointer(object)
lispobj object;
{
	lispobj *headerp, header;
	int type, len;

	headerp = (lispobj *) PTR(object);
	header = *headerp;
	type = TypeOf(header);

	switch (type) {
	case type_CodeHeader:
		break;
	case type_ReturnPcHeader:
	case type_FunctionHeader:
	case type_ClosureFunctionHeader:
		len = HEADER_LENGTH(header);
        	if (len == 0)
                	headerp = NULL;
		else
			headerp -= len;
		break;
	default:
        	headerp = NULL;
	}

	return (struct code *) headerp;
}

static
struct call_frame *
current_cont()
{
        int free;

	free = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
	
        if (free == 0)
            return NULL;
        else {
            struct sigcontext *csp;
            struct call_frame *cont;

            csp = lisp_interrupt_contexts[free-1];
            cont = (struct call_frame *)csp->sc_regs[CONT];
            if ((lispobj *)cont == current_control_stack_pointer)
                /* We were attempting to call an undefined function. */
                ;
            return cont;
        }
}

static
cs_valid_pointer_p(pointer)
struct call_frame *pointer;
{
	return (((char *) control_stack <= (char *) pointer) &&
		((char *) pointer < (char *) current_control_stack_pointer));
}

static void
info_from_sigcontext(info, csp)
struct call_info *info;
struct sigcontext *csp;
{
    unsigned long pc;

    if (csp->sc_regs[CONT] == csp->sc_regs[CSP]) {
        /* We were attempting to call an undefined function. */
        info->frame = (struct call_frame *)csp->sc_regs[OLDCONT];
        info->lra = (lispobj)csp->sc_regs[LRA];
        info->code = code_pointer(info->lra);
        pc = (unsigned long)PTR(info->lra);
    }
    else {
        info->frame = (struct call_frame *)csp->sc_regs[CONT];
        info->code = code_pointer(csp->sc_regs[CODE]);
        info->lra = 0;
        pc = csp->sc_pc;
    }
    info->pc = (pc - (unsigned long) info->code) / sizeof(lispobj) - HEADER_LENGTH(info->code->header);
}

static int
previous_info(info)
struct call_info *info;
{
    if (!cs_valid_pointer_p(info->frame)) {
        printf("Bogus callee value (0x%08x).\n", (unsigned long)info->frame);
        return 0;
    }

    info->lra = info->frame->saved_lra;
    info->frame = info->frame->old_cont;

    if (info->frame == NULL)
        return 0;

    info->code = code_pointer(info->lra);
    info->pc = ((unsigned long)PTR(info->lra) - (unsigned long) info->code) / sizeof(lispobj) - HEADER_LENGTH(info->code->header);

    return 1;
}

void
backtrace(nframes)
int nframes;
{
    struct call_info info;
    int free;

    free = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
	
    if (free == 0) {
        printf("Nothing to backtrace.\n");
        return;
    }

    info_from_sigcontext(&info, lisp_interrupt_contexts[free-1]);

    do {
        printf("<Frame 0x%08x, CODE: 0x%08x, ",
               (unsigned long) info.frame,
               (unsigned long) info.code);

        if (info.code != (struct code *) 0) {
            lispobj function;

            function = info.code->entry_points;
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

        if (info.lra != 0)
            printf("LRA: 0x%08x, ", (unsigned long)info.lra);
        else
            printf("<no LRA>, ");

        printf("PC: %d\n", info.pc);

    } while (--nframes > 0 && previous_info(&info));
}
