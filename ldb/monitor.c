/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/monitor.c,v 1.14 1991/02/16 01:00:36 wlott Exp $ */

#include <stdio.h>
#include <setjmp.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include "ldb.h"
#include "lisp.h"
#include "globals.h"
#include "vars.h"
#include "parse.h"
#include "interrupt.h"
#include "lispregs.h"

static void call_cmd(), dump_cmd(), print_cmd(), quit(), help();
static void flush_cmd(), search_cmd(), regs_cmd(), exit_cmd();
static void timed_call_cmd(), gc_cmd(), print_context_cmd();
static void backtrace_cmd(), purify_cmd(), catchers_cmd(), save_cmd();
static void grab_sigs_cmd(), restore_cmd();

static struct cmd {
    char *cmd, *help;
    void (*fn)();
} Cmds[] = {
    {"help", "Display this info", help},
    {"?", NULL, help},
    {"backtrace", "backtrace up to N frames", backtrace_cmd},
    {"call", "call FUNCTION with ARG1, ARG2, ...", call_cmd},
    {"catchers", "Print a list of all the active catchers.", catchers_cmd},
    {"context", "print interrupt context number I.", print_context_cmd},
    {"dump", "dump memory starting at ADDRESS for COUNT words.", dump_cmd},
    {"d", NULL, dump_cmd},
    {"exit", "Exit this instance of the monitor.", exit_cmd},
    {"flush", "flush all temp variables.", flush_cmd},
    {"gc", "collect garbage (caveat collector).", gc_cmd},
    {"grab-signals", "Set the signal handlers to call LDB.", grab_sigs_cmd},
    {"purify", "purify (caveat purifier).", purify_cmd},
    {"print", "print object at ADDRESS.", print_cmd},
    {"p", NULL, print_cmd},
    {"quit", "quit.", quit},
    {"regs", "display current lisp regs.", regs_cmd},
    {"restore", "restore the saved lisp image.", restore_cmd},
    {"save", "save the current lisp image.", save_cmd},
    {"search", "search for TYPE starting at ADDRESS for a max of COUNT words.", search_cmd},
    {"s", NULL, search_cmd},
    {"time", "call FUNCTION with ARG1, ARG2, ... and time it.", timed_call_cmd},
    {NULL, NULL, NULL}
};


static jmp_buf curbuf;


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
    printf("CSP\t=\t0x%08x\n", current_control_stack_pointer);
    printf("FP\t=\t0x%08x\n", current_control_frame_pointer);
#ifndef ibmrt
    printf("BSP\t=\t0x%08x\n", current_binding_stack_pointer);
#endif

    printf("DYNAMIC\t=\t0x%08x\n", current_dynamic_space);
#ifndef ibmrt
    printf("ALLOC\t=\t0x%08x\n", current_dynamic_space_free_pointer);
#endif
    printf("TRIGGER\t=\t0x%08x\n", current_auto_gc_trigger);
    printf("STATIC\t=\t0x%08x\n", SymbolValue(STATIC_SPACE_FREE_POINTER));
    printf("RDONLY\t=\t0x%08x\n", SymbolValue(READ_ONLY_SPACE_FREE_POINTER));

#ifdef MIPS
    printf("FLAGS\t=\t0x%08x\n", current_flags_register);
#endif
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

    while (search_for_type(val, &end, &count)) {
        printf("found 0x%x at 0x%x:\n", val, end);
        obj = *end;
        addr = end;
        end += 2;
        if (TypeOf(obj) == type_FunctionHeader)
            print((long)addr | type_FunctionPointer);
        else if (LowtagOf(obj) == type_OtherImmediate0 || LowtagOf(obj) == type_OtherImmediate1)
            print((long)addr | type_OtherPointer);
        else
            print(addr);
        if (count == -1)
            return;
    }
}

static void call_cmd(ptr)
char **ptr;
{
    extern lispobj call_into_lisp();

    lispobj call_name = parse_lispobj(ptr);
    lispobj function, result, *args;
    int numargs;

    if (LowtagOf(call_name) == type_OtherPointer) {
        struct symbol *sym = (struct symbol *)PTR(call_name);

        if (TypeOf(sym->header) == type_SymbolHeader) {
            function = sym->function;
            if (LowtagOf(function) != type_FunctionPointer) {
                printf("undefined function: ``%s''\n", (char *)PTR(sym->name) + 8);
                return;
            }
        }
        else {
            printf("0x%x is not a function pointer.\n", call_name);
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
    args = current_control_stack_pointer;
    while (more_p(ptr)) {
        current_control_stack_pointer++;
        current_control_stack_pointer[-1] = parse_lispobj(ptr);
        numargs++;
    }

    result = call_into_lisp(call_name, function, args, numargs);

    print(result);
}

static double tv_diff(x, y)
struct timeval *x, *y;
{
    return (((double) x->tv_sec + (double) x->tv_usec * 1.0e-6) -
	    ((double) y->tv_sec + (double) y->tv_usec * 1.0e-6));
}

static void timed_call_cmd(ptr)
char **ptr;
{
    extern lispobj call_into_lisp();

    lispobj args[16];

    lispobj call_name = parse_lispobj(ptr);
    lispobj function, result, *argptr;
    int numargs;
    struct timeval start_tv, stop_tv;
    struct rusage start_rusage, stop_rusage;
    double real_time, system_time, user_time;

    if (LowtagOf(call_name) == type_OtherPointer) {
        struct symbol *sym = (struct symbol *)PTR(call_name);

        if (TypeOf(sym->header) == type_SymbolHeader) {
            function = sym->function;
            if (LowtagOf(function) != type_FunctionPointer) {
                printf("undefined function: ``%s''\n", (char *)PTR(sym->name) + 8);
                return;
            }
        }
        else {
            printf("0x%x is not a function pointer.\n", call_name);
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

    getrusage(RUSAGE_SELF, &start_rusage);
    gettimeofday(&start_tv, (struct timezone *) 0);
    result = call_into_lisp(call_name, function, args, numargs);
    gettimeofday(&stop_tv, (struct timezone *) 0);
    getrusage(RUSAGE_SELF, &stop_rusage);

    print(result);

    real_time = tv_diff(&stop_tv, &start_tv) * 1000.0;
    user_time = tv_diff(&stop_rusage.ru_utime, &start_rusage.ru_utime) *
	    1000.0;
    system_time = tv_diff(&stop_rusage.ru_stime, &start_rusage.ru_stime) *
	    1000.0;

    printf("Call took:\n");
    printf("%20.8f msec of real time\n", real_time);
    printf("%20.8f msec of user time,\n", user_time);
    printf("%20.8f msec of system time.\n", system_time);
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

static int done;

static void exit_cmd()
{
    done = TRUE;
}

static void gc_cmd()
{
	collect_garbage();
}

static void purify_cmd()
{
	purify(NIL);
}

static void print_context(context)
struct sigcontext *context;
{
	int i;

	for (i = 0; i < NREGS; i++) {
		printf("%s:\t", lisp_register_names[i]);
		brief_print((lispobj) context->sc_regs[i]);
	}
	printf("PC:\t\t  0x%08x\n", context->sc_pc);
}

static void print_context_cmd(ptr)
char **ptr;
{
	int free;

	free = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
	
        if (more_p(ptr)) {
		int index;

		index = parse_number(ptr);

		if ((index >= 0) && (index < free)) {
			printf("There are %d interrupt contexts.\n", free);
			printf("Printing context %d\n", index);
			print_context(lisp_interrupt_contexts[index]);
		} else {
			printf("There aren't that many/few contexts.\n");
			printf("There are %d interrupt contexts.\n", free);
		}
	} else {
		if (free == 0)
			printf("There are no interrupt contexts!\n");
		else {
			printf("There are %d interrupt contexts.\n", free);
			printf("Printing context %d\n", free - 1);
			print_context(lisp_interrupt_contexts[free - 1]);
		}
	}
}

static void backtrace_cmd(ptr)
char **ptr;
{
	void backtrace();
	int n;

        if (more_p(ptr))
		n = parse_number(ptr);
	else
		n = 100;
	
	printf("Backtrace:\n");
	backtrace(n);
}

static void catchers_cmd()
{
    struct catch_block *catch;

    catch = (struct catch_block *)SymbolValue(CURRENT_CATCH_BLOCK);

    if (catch == NULL)
        printf("There are no active catchers!\n");
    else {
        while (catch != NULL) {
            printf("0x%08x:\n\tuwp: 0x%08x\n\tfp: 0x%08x\n\tcode: 0x%08x\n\tentry: 0x%08x\n\ttag: ", catch, catch->current_uwp, catch->current_cont, catch->current_code, catch->entry_pc);
            brief_print((lispobj)catch->tag);
            catch = catch->previous_catch;
        }
    }
}

static void save_cmd(ptr)
char **ptr;
{
    if (more_p(ptr))
        save(*ptr);
    else
        save("lisp.core");
}

static void restore_cmd(ptr)
     char **ptr;
{
    restore();
}

static void grab_sigs_cmd()
{
    printf("Grabbing signals.\n");
    test_init();
}

static void sub_monitor()
{
    extern char *egets();
    struct cmd *cmd, *found;
    char *line, *ptr, *token;
    int ambig;

    while (!done) {
        printf("ldb> ");
        fflush(stdout);
        line = egets();
        if (line == NULL) {
	    if (isatty(0)) {
		putchar('\n');
	        continue;
	    }
	    else {
		fprintf(stderr, "\nEOF on something other than a tty.\n");
		exit(0);
	    }
	}
        ptr = line;
        if ((token = parse_token(&ptr)) == NULL)
            continue;
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
            reset_printer();
            (*found->fn)(&ptr);
        }
    }
}

void ldb_monitor()
{
    jmp_buf oldbuf;

    bcopy(curbuf, oldbuf, sizeof(oldbuf));

    printf("LDB monitor\n");

    setjmp(curbuf);

    sub_monitor();

    done = FALSE;

    bcopy(oldbuf, curbuf, sizeof(curbuf));
}

void throw_to_monitor()
{
    longjmp(curbuf, 1);
}
