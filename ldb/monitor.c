/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/monitor.c,v 1.2 1990/02/28 18:23:28 wlott Exp $ */
#include <stdio.h>
#include <setjmp.h>


#include "ldb.h"
#include "lisp.h"
#include "vars.h"
#include "parse.h"

static void call_cmd(), dump_cmd(), print_cmd(), quit(), help(), flush_cmd(), search_cmd(), regs_cmd(), exit_cmd(), throw_cmd();

static struct cmd {
    char *cmd, *help;
    void (*fn)();
} Cmds[] = {
    {"help", "Display this info", help},
    {"?", NULL, help},
    {"call", "call FUNCTION with ARG1, ARG2, ...", call_cmd},
    {"dump", "dump memory starting at ADDRESS for COUNT words", dump_cmd},
    {"d", NULL, dump_cmd},
    {"exit", "Exit this instance of the monitor.", exit_cmd},
    {"flush", "flush all temp variables.", flush_cmd},
    {"print", "print object at ADDRESS", print_cmd},
    {"p", NULL, print_cmd},
    {"quit", "quit", quit},
    {"regs", "display current lisp regs.", regs_cmd},
    {"search", "search for TYPE starting at ADDRESS for a max of COUNT words.", search_cmd},
    {"s", NULL, search_cmd},
    {"throw", "Throw to the top level monitor.", throw_cmd},
    {NULL, NULL, NULL}
};


static int visable(c)
unsigned char c;
{
    if (c < ' ' || c > '~')
        return ' ';
    else
        return c;
}

static void dump_cmd(ptr)
char **ptr;
{
    static char *lastaddr = 0;
    static int lastcount = 20;

    char *addr = lastaddr;
    int count = lastcount, displacement;

    if (more_p(ptr)) {
        addr = parse_addr(ptr);

        if (more_p(ptr))
            count = parse_number(ptr);
    }

    if (count == 0) {
        printf("COUNT must be non-zero.\n");
        return;
    }
        
    lastcount = count;

    if (count > 0)
        displacement = 4;
    else {
        displacement = -4;
        count = -count;
    }

    while (count-- > 0) {
        printf("0x%08x: ", addr);
        if (valid_addr(addr)) {
            unsigned long *lptr = (unsigned long *)addr;
            unsigned short *sptr = (unsigned short *)addr;
            unsigned char *cptr = (unsigned char *)addr;

            printf("0x%08x   0x%04x 0x%04x   0x%02x 0x%02x 0x%02x 0x%02x    %c%c%c%c\n", lptr[0], sptr[0], sptr[1], cptr[0], cptr[1], cptr[2], cptr[3], visable(cptr[0]), visable(cptr[1]), visable(cptr[2]), visable(cptr[3]));
        }
        else
            printf("invalid address\n");

        addr += displacement;
    }

    lastaddr = addr;
}

static void print_cmd(ptr)
char **ptr;
{
    lispobj obj = parse_lispobj(ptr);
    
    print(obj);
}

static void regs_cmd(ptr)
char **ptr;
{
    printf("ALLOC=0x%x\n", SymbolValue(SAVED_ALLOCATION_POINTER));
    printf("CSP=0x%x\n", SymbolValue(SAVED_CONTROL_STACK_POINTER));
    printf("BSP=0x%x\n", SymbolValue(SAVED_BINDING_STACK_POINTER));
    printf("FLAGS=0x%x\n", SymbolValue(SAVED_FLAGS_REGISTER));
}

static void search_cmd(ptr)
char **ptr;
{
    static int lastval = 0, lastcount = 0;
    static lispobj *start = 0, *end = 0;
    int val, count;
    lispobj *addr, obj;

    if (more_p(ptr)) {
        val = parse_number(ptr);
        if (val < 0 || val > 0xff) {
            printf("Can only search for single bytes.\n");
            return;
        }
        if (more_p(ptr)) {
            addr = (lispobj *)PTR((long)parse_addr(ptr));
            if (more_p(ptr)) {
                count = parse_number(ptr);
            }
            else {
                /* Speced value and address, but no count. Only one. */
                count = -1;
            }
        }
        else {
            /* Speced a value, but no address, so search same range. */
            addr = start;
            count = lastcount;
        }
    }
    else {
        /* Speced nothing, search again for val. */
        val = lastval;
        addr = end;
        count = lastcount;
    }

    lastval = val;
    start = end = addr;
    lastcount = count;

    printf("searching for 0x%x at 0x%x\n", val, end);

    while ((count == -1 || (count-- > 0)) && valid_addr(end)) {
        obj = *end;
        addr = end;
        end += 2;

        if (((long)obj & 0xff) == val) {
            printf("found 0x%x at 0x%x:\n", val, addr);
            if (LowtagOf(obj) == type_OtherImmediate0 ||    LowtagOf(obj) == type_OtherImmediate1)
                print((long)addr | type_OtherPointer);
            else
                print(addr);
            if (count == -1)
                return;
        }
    }
}

static void call_cmd(ptr)
char **ptr;
{
    extern lispobj call_into_lisp();

    static lispobj args[16];

    lispobj call_name = parse_lispobj(ptr);
    lispobj function, result, arg, *argptr;
    int numargs;

    if (LowtagOf(call_name) == type_OtherPointer && TypeOf(call_name) == type_SymbolHeader) {
        struct symbol *sym = (struct symbol *)PTR(call_name);

        function = sym->function;
        if (LowtagOf(function) != type_FunctionPointer) {
            printf("undefined function: ``%s''\n", (char *)PTR(sym->name) + 8);
            return;
        }
    }
    else if (LowtagOf(call_name) != type_FunctionPointer) {
        printf("0x%x is not a function pointer.\n", call_name);
        return;
    }
    else
        function = call_name;

    numargs = 0;
    argptr = args;
    while (more_p(ptr)) {
        *argptr++ = parse_lispobj(ptr);
        numargs++;
    }
    while (argptr < args + 6)
        *argptr++ = NIL;

    result = call_into_lisp(call_name, function, args, numargs);

    print(result);
}

static void flush_cmd()
{
    flush_vars();
}

static void quit()
{
    char buf[10];

    printf("Really quit? [n] ");
    fflush(stdout);
    fgets(buf, sizeof(buf), stdin);
    if (buf[0] == 'y' || buf[0] == 'Y')
        exit(0);
}

static void help()
{
    struct cmd *cmd;

    for (cmd = Cmds; cmd->cmd != NULL; cmd++)
        if (cmd->help != NULL)
            printf("%s\t%s\n", cmd->cmd, cmd->help);
}

static void throw_cmd()
{
    void throw_to_top();
    char buf[10];

    printf("Really throw? [n] ");
    fflush(stdout);
    fgets(buf, sizeof(buf), stdin);
    if (buf[0] == 'y' || buf[0] == 'Y')
        throw_to_top(0);
}

static int done;

static void exit_cmd()
{
    done = TRUE;
}

static void sub_monitor(csp, bsp)
unsigned long csp, bsp;
{
    extern char *egets();
    struct cmd *cmd, *found;
    char *line, *ptr, *token;
    static char *last = NULL;
    int ambig;
    unsigned long new;

    while (!done) {
        if ((new = SymbolValue(SAVED_CONTROL_STACK_POINTER)) != csp) {
            printf("CSP changed from 0x%x to 0x%x; Restoring.\n", csp, new);
            SetSymbolValue(SAVED_CONTROL_STACK_POINTER, csp);
        }
        if ((new = SymbolValue(SAVED_BINDING_STACK_POINTER)) != bsp) {
            printf("BSP changed from 0x%x to 0x%x; Restoring.\n", bsp, new);
            SetSymbolValue(SAVED_BINDING_STACK_POINTER, bsp);
        }

        printf("ldb> ");
        fflush(stdout);
        line = egets();
        if (line == NULL) {
            last = NULL;
            putchar('\n');
            continue;
        }
        ptr = line;
        if ((token = parse_token(&ptr)) == NULL) {
            if (last == NULL)
                continue;
            token = last;
        }
        ambig = 0;
        found = NULL;
        for (cmd = Cmds; cmd->cmd != NULL; cmd++) {
            if (strcmp(token, cmd->cmd) == 0) {
                found = cmd;
                ambig = 0;
                break;
            }
            else if (strncmp(token, cmd->cmd, strlen(token)) == 0) {
                if (found)
                    ambig = 1;
                else
                    found = cmd;
            }
        }
        if (ambig)
            printf("``%s'' is ambiguous.\n", token);
        else if (found == NULL)
            printf("unknown command: ``%s''\n", token);
        else {
            last = found->cmd;
            (*found->fn)(&ptr);
        }
    }
}


static jmp_buf topbuf;
static jmp_buf curbuf;
static int level = 0;

void monitor()
{
    jmp_buf oldbuf;
    unsigned long csp, bsp;

    csp = SymbolValue(SAVED_CONTROL_STACK_POINTER);
    bsp = SymbolValue(SAVED_BINDING_STACK_POINTER);

    bcopy(curbuf, oldbuf, sizeof(oldbuf));

    if (level == 0) {
        setjmp(topbuf);
        level = 0;
    }

    level++;
    printf("LDB monitor (level=%d)\n", level);

    setjmp(curbuf);

    sub_monitor(csp, bsp);

    done = FALSE;

    bcopy(oldbuf, curbuf, sizeof(curbuf));

    level--;
}

void throw_to_monitor()
{
    longjmp(curbuf, 1);
}

void throw_to_top()
{
    longjmp(topbuf, 1);
}
