/*
 * main() entry point for a stand alone lisp image.
 *
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/lisp.c,v 1.9 1994/10/25 18:10:20 ram Exp $
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/stat.h>

#include "signal.h"

#include "lisp.h"
#include "internals.h"
#include "alloc.h"
#include "vars.h"
#include "globals.h"
#include "os.h"
#include "interrupt.h"
#include "arch.h"
#include "gc.h"
#include "monitor.h"
#include "validate.h"
#include "core.h"
#include "save.h"
#include "lispregs.h"

#ifdef irix
#include <string.h>
#include "interr.h"
#endif


/* SIGINT handler that invokes the monitor. */

static void sigint_handler(HANDLER_ARGS)
{
    SAVE_CONTEXT();

    printf("\nSIGINT hit at 0x%08X\n", SC_PC(context));
    ldb_monitor();
}

/* Not static, because we want to be able to call it from lisp land. */
void sigint_init(void)
{
    install_handler(SIGINT, sigint_handler);
}


/* Noise to convert argv and argp into lists. */

static lispobj alloc_str_list(char *list[])
{
    lispobj result, newcons;
    struct cons *ptr;

    if (*list == NULL)
        result = NIL;
    else {
        result = newcons = alloc_cons(alloc_string(*list++), NIL);

        while (*list != NULL) {
            ptr = (struct cons *)PTR(newcons);
            newcons = alloc_cons(alloc_string(*list++), NIL);
            ptr->cdr = newcons;
        }
    }

    return result;
}


/* And here be main. */

void main(int argc, char *argv[], char *envp[])
{
    char *arg, **argptr;
    char *core = NULL, *default_core;
    boolean monitor;
    lispobj initial_function;

#ifdef MACH
    mach_init();
#endif
#ifdef SVR4
    tzset();
#endif

    set_lossage_handler(ldb_monitor);

    define_var("nil", NIL, TRUE);
    define_var("t", T, TRUE);
    monitor = FALSE;

    argptr = argv;
    while ((arg = *++argptr) != NULL) {
        if (strcmp(arg, "-core") == 0) {
            if (core != NULL) {
                fprintf(stderr, "can only specify one core file.\n");
                exit(1);
            }
            core = *++argptr;
            if (core == NULL) {
                fprintf(stderr, "-core must be followed by the name of the core file to use.\n");
                exit(1);
            }
        }
	else if (strcmp(arg, "-monitor") == 0) {
	    monitor = TRUE;
	}
    }

    default_core = arch_init();
    if (default_core == NULL)
	default_core = "lisp.core";

    if (core == NULL) {
#ifdef MACH
	extern char *getenv(char *var);
#endif
	static char buf[MAXPATHLEN];
	char *lib = getenv("CMUCLLIB");

	if (lib != NULL) {
	    char *dst;
	    struct stat statbuf;

	    do {
		dst = buf;
		while (*lib != '\0' && *lib != ':')
		    *dst++ = *lib++;
		if (dst != buf && dst[-1] != '/')
		    *dst++ = '/';
		strcpy(dst, default_core);
		if (stat(buf, &statbuf) == 0) {
		    core = buf;
		    break;
		}
	    } while (*lib++ == ':');
	}
	if (core == NULL) {
	    /* Note: the /usr/misc/.cmucl/lib/ default path is also wired
	    /* into the lisp code in .../code/save.lisp. */
#ifdef hpux
	    strcpy(buf, "/usr/local/lib/cmucl/lib/");
#else
	    strcpy(buf, "/usr/misc/.cmucl/lib/");
#endif
	    strcat(buf, default_core);
	    core = buf;
	}
    }

    os_init();
    gc_init();
    validate();
    globals_init();

    initial_function = load_core_file(core);

#ifdef BINDING_STACK_POINTER
    SetSymbolValue(BINDING_STACK_POINTER, (lispobj)binding_stack);
#endif
#ifdef INTERNAL_GC_TRIGGER
    SetSymbolValue(INTERNAL_GC_TRIGGER, fixnum(-1));
#endif

    interrupt_init();

    arch_install_interrupt_handlers();
    os_install_interrupt_handlers();

    /* Convert the argv and envp to something Lisp can grok. */
    SetSymbolValue(LISP_COMMAND_LINE_LIST, alloc_str_list(argv));
    SetSymbolValue(LISP_ENVIRONMENT_LIST, alloc_str_list(envp));

#ifdef PSEUDO_ATOMIC_ATOMIC
    /* Turn on pseudo atomic for when we call into lisp. */
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, fixnum(1));
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, fixnum(0));
#endif

    /* Pick off sigint until the lisp system gets far enough along to */
    /* install it's own. */
    sigint_init();

    if (monitor)
	while (1)
	    ldb_monitor();
    else {
	funcall0(initial_function);
	printf("Initial function returned?\n");
	exit(1);
    }
}
