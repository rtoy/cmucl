/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/backtrace.c,v 1.4 1990/05/24 17:46:03 wlott Exp $
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
    int interrupted;
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
cs_valid_pointer_p(pointer)
struct call_frame *pointer;
{
	return (((char *) control_stack <= (char *) pointer) &&
		((char *) pointer < (char *) current_control_stack_pointer));
}

static void
info_from_lisp_state(info)
struct call_info *info;
{
    info->frame = (struct call_frame *)current_control_frame_pointer;
    info->interrupted = 0;
    info->code = NULL;
    info->lra = 0;
    info->pc = 0;

    previous_info(info);
}

static void
info_from_sigcontext(info, csp)
struct call_info *info;
struct sigcontext *csp;
{
    unsigned long pc;

    if (LowtagOf(csp->sc_regs[CODE]) == type_FunctionPointer) {
        /* We tried to call a function, but crapped out before $CODE could be fixed up.  Probably an undefined function. */
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
    if (info->code != NULL)
        info->pc = pc - (unsigned long) info->code - HEADER_LENGTH(info->code->header);
    else
        info->pc = 0;
}

static int
previous_info(info)
struct call_info *info;
{
    struct call_frame *this_frame;
    int free;
    struct sigcontext *csp;

    if (!cs_valid_pointer_p(info->frame)) {
        printf("Bogus callee value (0x%08x).\n", (unsigned long)info->frame);
        return 0;
    }

    this_frame = info->frame;
    info->lra = this_frame->saved_lra;
    info->frame = this_frame->old_cont;

    if (info->frame == NULL || info->frame == this_frame)
        return 0;

    info->code = code_pointer(info->lra);

    if (info->code == (struct code *)PTR(info->lra)) {
        /* We were interrupted.  Find the correct sigcontext. */
        free = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
        while (free-- > 0) {
            csp = lisp_interrupt_contexts[free];
            if ((struct call_frame *)(csp->sc_regs[CONT]) == info->frame)
                info_from_sigcontext(info, csp);
        }
        info->interrupted = 1;
    }
    else if (info->code != NULL)
        info->pc = ((unsigned long)PTR(info->lra) - (unsigned long) info->code) / sizeof(lispobj) - HEADER_LENGTH(info->code->header);
    else
        info->pc = 0;

        

    return 1;
}

void
backtrace(nframes)
int nframes;
{
    struct call_info info;
	
    info_from_lisp_state(&info);

    do {
        printf("<Frame 0x%08x%s, ", (unsigned long) info.frame,
                info.interrupted ? " [interrupted]" : "");
        
        if (info.code != (struct code *) 0) {
            lispobj function;

            printf("CODE: 0x%08x, ", (unsigned long) info.code | type_OtherPointer);

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
        else
            printf("CODE: ???, ");

        if (info.lra != 0)
            printf("LRA: 0x%08x, ", (unsigned long)info.lra);
        else
            printf("<no LRA>, ");

        if (info.pc)
            printf("PC: 0x%x>\n", info.pc);
        else
            printf("PC: ???>\n");

    } while (--nframes > 0 && previous_info(&info));
}
