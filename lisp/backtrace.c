/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/backtrace.c,v 1.4 1994/10/25 17:31:52 ram Exp $
 *
 * Simple backtrace facility.  More or less from Rob's lisp version.
 */

#include <stdio.h>
#include <signal.h>
#include "lisp.h"
#include "internals.h"
#include "globals.h"
#include "os.h"
#include "interrupt.h"
#include "lispregs.h"

#ifndef i386

/* Sigh ... I know what the call frame looks like and it had
   better not change. */

struct call_frame {
#ifndef alpha
	struct call_frame *old_cont;
#else
        u32 old_cont;
#endif
	lispobj saved_lra;
        lispobj code;
	lispobj other_state[5];
};

struct call_info {
#ifndef alpha
    struct call_frame *frame;
#else
    u32 frame;
#endif
    int interrupted;
#ifndef alpha
    struct code *code;
#else
    u32 code;
#endif
    lispobj lra;
    int pc; /* Note: this is the trace file offset, not the actual pc. */
};

#define HEADER_LENGTH(header) ((header)>>8)

static int previous_info(struct call_info *info);

static struct code *
code_pointer(lispobj object)
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

static boolean
cs_valid_pointer_p(struct call_frame *pointer)
{
	return (((char *) control_stack <= (char *) pointer) &&
		((char *) pointer < (char *) current_control_stack_pointer));
}

static void
info_from_lisp_state(struct call_info *info)
{
    info->frame = (struct call_frame *)current_control_frame_pointer;
    info->interrupted = 0;
    info->code = NULL;
    info->lra = 0;
    info->pc = 0;

    previous_info(info);
}

static void
info_from_sigcontext(struct call_info *info, struct sigcontext *csp)
{
    unsigned long pc;

    info->interrupted = 1;
    if (LowtagOf(SC_REG(csp, reg_CODE)) == type_FunctionPointer) {
        /* We tried to call a function, but crapped out before $CODE could be fixed up.  Probably an undefined function. */
        info->frame = (struct call_frame *)SC_REG(csp, reg_OCFP);
        info->lra = (lispobj)SC_REG(csp, reg_LRA);
        info->code = code_pointer(info->lra);
        pc = (unsigned long)PTR(info->lra);
    }
    else {
        info->frame = (struct call_frame *)SC_REG(csp, reg_CFP);
        info->code = code_pointer(SC_REG(csp, reg_CODE));
        info->lra = NIL;
        pc = SC_PC(csp);
    }
    if (info->code != NULL)
        info->pc = pc - (unsigned long) info->code -
#ifndef alpha
            (HEADER_LENGTH(info->code->header) * sizeof(lispobj));
#else
            (HEADER_LENGTH(((struct code *)info->code)->header) * sizeof(lispobj));
#endif
    else
        info->pc = 0;
}

static int
previous_info(struct call_info *info)
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
    info->interrupted = 0;

    if (info->frame == NULL || info->frame == this_frame)
        return 0;

    if (info->lra == NIL) {
        /* We were interrupted.  Find the correct sigcontext. */
        free = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
        while (free-- > 0) {
            csp = lisp_interrupt_contexts[free];
            if ((struct call_frame *)(SC_REG(csp, reg_CFP)) == info->frame) {
                info_from_sigcontext(info, csp);
                break;
            }
        }
    }
    else {
        info->code = code_pointer(info->lra);
        if (info->code != NULL)
            info->pc = (unsigned long)PTR(info->lra) -
                (unsigned long)info->code -
#ifndef alpha
                (HEADER_LENGTH(info->code->header) * sizeof(lispobj));
#else
                (HEADER_LENGTH(((struct code *)info->code)->header) * sizeof(lispobj));
#endif
        else
            info->pc = 0;
    }

    return 1;
}

void
backtrace(int nframes)
{
    struct call_info info;
	
    info_from_lisp_state(&info);

    do {
        printf("<Frame 0x%08x%s, ", (unsigned long) info.frame,
                info.interrupted ? " [interrupted]" : "");
        
        if (info.code != (struct code *) 0) {
            lispobj function;

            printf("CODE: 0x%08X, ", (unsigned long) info.code | type_OtherPointer);

#ifndef alpha
            function = info.code->entry_points;
#else
            function = ((struct code *)info.code)->entry_points;
#endif
            while (function != NIL) {
                struct function *header;
                lispobj name;

                header = (struct function *) PTR(function);
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
                        printf("(Not simple string??\?), ");
                } else
                    printf("(Not other pointer??\?), ");


                function = header->next;
            }
        }
        else
            printf("CODE: ???, ");

        if (info.lra != NIL)
            printf("LRA: 0x%08x, ", (unsigned long)info.lra);
        else
            printf("<no LRA>, ");

        if (info.pc)
            printf("PC: 0x%x>\n", info.pc);
        else
            printf("PC: ??\?>\n");

    } while (--nframes > 0 && previous_info(&info));
}

#else

void
backtrace(nframes)
int nframes;
{
    printf("Can't backtrace on this hardware platform.\n");
}

#endif
