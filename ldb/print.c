/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/print.c,v 1.1 1990/02/24 19:37:28 wlott Exp $ */
#include <stdio.h>

#include "ldb.h"
#include "print.h"
#include "lisp.h"
#include "vars.h"

static int max_lines = 20, cur_lines = 0;
static int max_depth = 4, cur_depth = 0;
static boolean dont_decend = FALSE, skip_newline = FALSE;
static cur_clock = 0;

static void print_obj();

#define NEWLINE if (continue_p()) newline(NULL); else return;

char *lowtag_Names[] = {
    "even fixnum",
    "function pointer",
    "other immediate",
    "list pointer",
    "odd fixnum",
    "structure pointer",
    "short float",
    "other pointer"
};

char *subtype_Names[] = {
    "subtype 0",
    "bignum",
    "ratio",
    "single float",
    "double float",
    "complex",
    "simple-array",
    "simple-string",
    "simple-bit-vector",
    "simple-vector",
    "(simple-array (unsigned-byte 2) (*))",
    "(simple-array (unsigned-byte 4) (*))",
    "(simple-array (unsigned-byte 8) (*))",
    "(simple-array (unsigned-byte 16) (*))",
    "(simple-array (unsigned-byte 32) (*))",
    "(simple-array single-float (*))",
    "(simple-array double-float (*))",
    "complex-string",
    "complex-bit-vector",
    "(array * (*))",
    "array",
    "code header",
    "function header",
    "closure function header",
    "return PC header",
    "closure header",
    "value cell header",
    "symbol header",
    "character",
    "SAP",
    "unbound marker"
};

static void indent(in)
int in;
{
    static char *spaces = "                                                                ";

    while (in > 64) {
        fputs(spaces, stdout);
        in -= 64;
    }
    if (in != 0)
        fputs(spaces + 64 - in, stdout);
}

static boolean continue_p()
{
    char buffer[256];

    if (cur_depth >= max_depth || dont_decend)
        return FALSE;

    if (skip_newline)
        skip_newline = FALSE;
    else
        putchar('\n');

    if (cur_lines >= max_lines) {
        printf("More? [y] ");
        fflush(stdout);

        gets(buffer);

        if (buffer[0] == 'n' || buffer[0] == 'N')
            throw_to_monitor();
        else
            cur_lines = 0;
    }

    return TRUE;
}

static void newline(label)
char *label;
{
    cur_lines++;
    if (label != NULL)
        fputs(label, stdout);
    putchar('\t');
    indent(cur_depth * 2);
}

static void print_fixnum(obj)
lispobj obj;
{
    printf(": %d", obj>>2);
}

static void print_otherimm(obj)
lispobj obj;
{
    int c;

    printf(", %s", subtype_Names[TypeOf(obj)>>3]);

    switch (TypeOf(obj)) {
        case type_Character:
            printf(": font=0x%x, bits=0x%x, char=0x%x", (obj>>24)&0xff, (obj>>16)&0xff, c = ((obj>>8)&0xff));
            if (c >= ' ' && c <= '~')
                printf(" (%c)", c);
            break;

        case type_Sap:
        case type_UnboundMarker:
            break;

        default:
            printf(": data=%d", (obj>>8)&0xffffff);
            break;
    }
}

static void print_list(obj)
lispobj obj;
{
    if (!valid_addr(obj))
        printf(" (invalid address)");
    else if (obj == NIL)
        printf(" (NIL)");
    else {
        struct cons *cons = (struct cons *)PTR(obj);

        print_obj("car: ", cons->car);
        print_obj("cdr: ", cons->cdr);
    }
}

static void print_struct(obj)
lispobj obj;
{
}

static void print_unused(obj)
lispobj obj;
{
}

static void print_slots(slots, count, ptr)
char **slots;
int count;
long *ptr;
{
    while (count-- > 0)
        if (*slots)
            print_obj(*slots++, *ptr++);
        else
            print_obj("???: ", *ptr++);
}

static char *symbol_slots[] = {"value: ", "function: ", "plist: ", "name: ", "package: ", NULL};
static char *ratio_slots[] = {"numer: ", "denom: ", NULL};
static char *complex_slots[] = {"real: ", "imag: ", NULL};
static char *code_slots[] = {"words: ", "entry: ", "debug: ", NULL};
static char *fn_slots[] = {"self: ", "next: ", "name: ", "arglist: ", "type: ", NULL};
static char *closure_slots[] = {"fn: ", NULL};

static void print_otherptr(obj)
lispobj obj;
{
    if (!valid_addr(obj))
        printf(" (invalid address)");
    else {
        unsigned long *ptr = (unsigned long *)PTR(obj);
        unsigned long header = *ptr++;
        unsigned long length = (*ptr) >> 2;
        int count = header>>8, type = TypeOf(header), index;
        boolean raw;
        char *cptr, buffer[16];

        print_obj("header: ", header);
        if (LowtagOf(header) != type_OtherImmediate) {
            NEWLINE;
            printf("(invalid header object)");
            return;
        }

        switch (type) {
            case type_Bignum:
                ptr += count;
                NEWLINE;
                printf("0x");
                while (count-- > 0)
                    printf("%08x", *--ptr);
                break;

            case type_Ratio:
                print_slots(ratio_slots, count, ptr);
                break;

            case type_Complex:
                print_slots(complex_slots, count, ptr);
                break;

            case type_SymbolHeader:
                print_slots(symbol_slots, count, ptr);
                break;

            case type_SingleFloat:
                NEWLINE;
                printf("%f", *(float *)ptr);
                break;

            case type_DoubleFloat:
                NEWLINE;
                printf("%lf", *(double *)ptr);
                break;

            case type_SimpleString:
                NEWLINE;
                cptr = (char *)(ptr+1);
                putchar('\"');
                while (length-- > 0)
                    putchar(*cptr++);
                putchar('\"');
                break;

            case type_SimpleVector:
                NEWLINE;
                printf("length = %d", length);
                ptr++;
                index = 0;
                while (length-- > 0) {
                    sprintf(buffer, "%d: ", index++);
                    print_obj(buffer, *ptr++);
                }
                break;

            case type_SimpleArray:
            case type_SimpleBitVector:
            case type_SimpleArrayUnsignedByte2:
            case type_SimpleArrayUnsignedByte4:
            case type_SimpleArrayUnsignedByte8:
            case type_SimpleArrayUnsignedByte16:
            case type_SimpleArrayUnsignedByte32:
            case type_SimpleArraySingleFloat:
            case type_SimpleArrayDoubleFloat:
            case type_ComplexString:
            case type_ComplexBitVector:
            case type_ComplexVector:
            case type_ComplexArray:
                break;

            case type_CodeHeader:
                print_slots(code_slots, count-1, ptr);
                break;

            case type_FunctionHeader:
            case type_ClosureFunctionHeader:
                print_slots(fn_slots, 5, ptr);
                break;

            case type_ReturnPcHeader:
                break;

            case type_ClosureHeader:
                print_slots(closure_slots, count, ptr);
                break;

            case type_Sap:
                NEWLINE;
                printf("0x%08x", *ptr);
                break;

            case type_Character:
            case type_UnboundMarker:
                NEWLINE;
                printf("pointer to an immediate?\n");
                break;
        }
    }
}

static void print_obj(prefix, obj)
char *prefix;
lispobj obj;
{
    static void (*Fns[])() = {print_fixnum, print_otherptr, print_otherimm, print_list, print_fixnum, print_struct, print_unused, print_otherptr};
    int type = LowtagOf(obj);
    struct var *var = lookup_by_obj(obj);
    char buffer[256];

    if (!continue_p())
        return;

    if (var != NULL && var_clock(var) == cur_clock)
        dont_decend = TRUE;

    if (var == NULL && (obj & type_FunctionPointer & type_ListPointer & type_StructurePointer & type_OtherPointer) != 0)
        var = define_var(NULL, obj, FALSE);

    if (var != NULL) {
        var_setclock(var, cur_clock);
        sprintf(buffer, "$%s=", var_name(var));
        newline(buffer);
    }
    else
        newline(NULL);

    printf("%s0x%x: %s", prefix, obj, lowtag_Names[type]);
    cur_depth++;
    (*Fns[type])(obj);
    cur_depth--;
    dont_decend = FALSE;
}

void print(obj)
lispobj obj;
{
    cur_clock++;
    cur_depth = 0;
    cur_lines = 0;
    dont_decend = FALSE;
    skip_newline = TRUE;

    print_obj("", obj);

    putchar('\n');
}
