/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/print.c,v 1.6 1990/03/18 19:23:56 ch Exp $ */
#include <stdio.h>

#include "ldb.h"
#include "print.h"
#include "lisp.h"
#include "vars.h"

static int max_lines = 20, cur_lines = 0;
static int max_depth = 5, brief_depth = 2, cur_depth = 0;
static int max_length = 5;
static boolean dont_decend = FALSE, skip_newline = FALSE;
static cur_clock = 0;

static void print_obj();

#define NEWLINE if (continue_p(TRUE)) newline(NULL); else return;

char *lowtag_Names[] = {
    "even fixnum",
    "function pointer",
    "other immediate [0]",
    "list pointer",
    "odd fixnum",
    "structure pointer",
    "other immediate [1]",
    "other pointer"
};

char *subtype_Names[] = {
    "unused 0",
    "unused 1",
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

static boolean continue_p(newline)
boolean newline;
{
    char buffer[256];

    if (cur_depth >= max_depth || dont_decend)
        return FALSE;

    if (newline) {
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


static void brief_fixnum(obj)
lispobj obj;
{
    printf("%d", obj>>2);
}

static void print_fixnum(obj)
lispobj obj;
{
    printf(": %d", obj>>2);
}

static void brief_otherimm(obj)
lispobj obj;
{
    int type = TypeOf(obj);
    int c;
    char buffer[10];

    switch (type) {
        case type_BaseCharacter:
            c = (obj>>8)&0xff;
            switch (c) {
                case '\0':
                    printf("#\\Null");
                    break;
                case '\n':
                    printf("#\\Newline");
                    break;
                case '\b':
                    printf("#\\Backspace");
                    break;
                case '\177':
                    printf("#\\Delete");
                    break;
                default:
                    strcpy(buffer, "#\\");
                    if (c >= 128) {
                        strcat(buffer, "m-");
                        c -= 128;
                    }
                    if (c < 32) {
                        strcat(buffer, "c-");
                        c += '@';
                    }
                    printf("%s%c", buffer, c);
                    break;
            }
            break;

        case type_UnboundMarker:
            printf("<unbound marker>");
            break;

        default:
            printf("%s", subtype_Names[type>>2]);
            break;
    }
}

static void print_otherimm(obj)
lispobj obj;
{
    int c;

    printf(", %s", subtype_Names[TypeOf(obj)>>2]);

    switch (TypeOf(obj)) {
        case type_BaseCharacter:
            printf(": ");
            brief_otherimm(obj);
            break;

        case type_Sap:
        case type_UnboundMarker:
            break;

        default:
            printf(": data=%d", (obj>>8)&0xffffff);
            break;
    }
}

static void brief_list(obj)
lispobj obj;
{
    int space = FALSE;
    int length = 0;

    if (!valid_addr(obj))
	    printf("(invalid address)");
    else if (obj == NIL)
        printf("NIL");
    else {
        putchar('(');
        while (LowtagOf(obj) == type_ListPointer) {
            struct cons *cons = (struct cons *)PTR(obj);

            if (space)
                putchar(' ');
            if (++length >= max_length) {
                printf("...");
                obj = NIL;
                break;
            }
            print_obj(NULL, cons->car);
            obj = cons->cdr;
            space = TRUE;
            if (obj == NIL)
                break;
        }
        if (obj != NIL) {
            printf(" . ");
            print_obj(NULL, obj);
        }
        putchar(')');
    }
}

static void print_list(obj)
lispobj obj;
{
    if (!valid_addr(obj))
	    printf("(invalid address)");
    else if (obj == NIL)
        printf(" (NIL)");
    else {
        struct cons *cons = (struct cons *)PTR(obj);

        print_obj("car: ", cons->car);
        print_obj("cdr: ", cons->cdr);
    }
}

static void brief_struct(obj)
lispobj obj;
{
}

static void print_struct(obj)
lispobj obj;
{
}

static void brief_otherptr(obj)
lispobj obj;
{
    lispobj *ptr, header;
    int type;
    struct symbol *symbol;
    struct vector *vector;
    char *charptr;

    ptr = (lispobj *) PTR(obj);

    if (!valid_addr(obj)) {
	    printf("(invalid address)");
	    return;
    }

    header = *ptr;
    type = TypeOf(header);
    switch (type) {
        case type_SymbolHeader:
            symbol = (struct symbol *)ptr;
            vector = (struct vector *)PTR(symbol->name);
            for (charptr = (char *)vector->data; *charptr != '\0'; charptr++) {
                if (*charptr == '"')
                    putchar('\\');
                putchar(*charptr);
            }
            break;

        case type_SimpleString:
            vector = (struct vector *)ptr;
            putchar('"');
            for (charptr = (char *)vector->data; *charptr != '\0'; charptr++) {
                if (*charptr == '"')
                    putchar('\\');
                putchar(*charptr);
            }
            putchar('"');
            break;
            
        default:
            printf("#<ptr to ");
            brief_otherimm(header);
            putchar('>');
    }
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
	    printf("(invalid address)");
    else {
        unsigned long *ptr; 
        unsigned long header;
        unsigned long length;
        int count, type, index;
        boolean raw;
        char *cptr, buffer[16];

	ptr = (unsigned long *) PTR(obj);
	if (ptr == (unsigned long *) NULL) {
		printf(" (NULL Pointer)");
		return;
	}

	header = *ptr++;
	length = (*ptr) >> 2;
	count = header>>8;
	type = TypeOf(header);

        print_obj("header: ", header);
        if (LowtagOf(header) != type_OtherImmediate0 && LowtagOf(header) != type_OtherImmediate1) {
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

            case type_BaseCharacter:
            case type_UnboundMarker:
                NEWLINE;
                printf("pointer to an immediate?");
                break;
        }
    }
}

static void print_obj(prefix, obj)
char *prefix;
lispobj obj;
{
    static void (*verbose_fns[])() = {print_fixnum, print_otherptr, print_otherimm, print_list, print_fixnum, print_struct, print_otherimm, print_otherptr};
    static void (*brief_fns[])() = {brief_fixnum, brief_otherptr, brief_otherimm, brief_list, brief_fixnum, brief_struct, brief_otherimm, brief_otherptr};
    int type = LowtagOf(obj);
    struct var *var = lookup_by_obj(obj);
    char buffer[256];
    boolean verbose = cur_depth < brief_depth;

    
    if (!continue_p(verbose))
        return;

    if (var != NULL && var_clock(var) == cur_clock)
        dont_decend = TRUE;

    if (var == NULL && (obj & type_FunctionPointer & type_ListPointer & type_StructurePointer & type_OtherPointer) != 0)
        var = define_var(NULL, obj, FALSE);

    if (var != NULL)
        var_setclock(var, cur_clock);

    cur_depth++;
    if (verbose) {
        if (var != NULL) {
            sprintf(buffer, "$%s=", var_name(var));
            newline(buffer);
        }
        else
            newline(NULL);
        printf("%s0x%08x: ", prefix, obj);
        if (cur_depth < brief_depth) {
            fputs(lowtag_Names[type], stdout);
            (*verbose_fns[type])(obj);
        }
        else
            (*brief_fns[type])(obj);
    }
    else {
        if (dont_decend)
            printf("$%s", var_name(var));
        else {
            if (var != NULL)
                printf("$%s=", var_name(var));
            (*brief_fns[type])(obj);
        }
    }
    cur_depth--;
    dont_decend = FALSE;
}

void reset_printer()
{
    cur_clock++;
    cur_lines = 0;
    dont_decend = FALSE;
}

void print(obj)
lispobj obj;
{
    skip_newline = TRUE;
    cur_depth = 0;

    print_obj("", obj);

    putchar('\n');
}
