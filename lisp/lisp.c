/*
 * main() entry point for a stand alone lisp image.
 *
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/lisp.c,v 1.3 1993/02/09 14:02:37 wlott Exp $
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
#include "arch.h"
#include "gc.h"
#include "monitor.h"
#include "validate.h"
#include "interrupt.h"
#include "core.h"
#include "save.h"
#include "lispregs.h"

lispobj lisp_nil_reg = NIL;
char *lisp_csp_reg, *lisp_bsp_reg;


/* SIGINT handler that invokes the monitor. */

static void sigint_handler(int signal, int code, struct sigcontext *context)
{
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


/* stuff to start a kernel core */

extern void call_on_stack(void fn(), os_vm_address_t new_sp);

void call_initial_function()
{
    funcall0(SymbolFunction(INITIAL_FUNCTION));
    printf("%%INITIAL-FUNCTION returned?\n");
}

void call_ldb_monitor()
{
    while (1)
	ldb_monitor();
}



/* And here be main. */

void main(int argc, char *argv[], char *envp[])
{
    char *arg, **argptr;
    char *core = NULL, *default_core;
    boolean restore_state, monitor;

#ifdef MACH
    mach_init();
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

    /* Note: the /usr/misc/.cmucl/lib/ default path is also wired into */
    /* the lisp code in .../code/save.lisp. */

    if (core == NULL) {
	extern char *getenv(char *var);
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
	    strcpy(buf, "/usr/misc/.cmucl/lib/");
	    strcat(buf, default_core);
	    core = buf;
	}
    }

    os_init();
    gc_init();
    validate();
    globals_init();

    restore_state = load_core_file(core);

    if (!restore_state) {
#ifdef BINDING_STACK_POINTER
	SetSymbolValue(BINDING_STACK_POINTER, (lispobj)binding_stack);
#endif
#ifdef INTERNAL_GC_TRIGGER
	SetSymbolValue(INTERNAL_GC_TRIGGER, fixnum(-1));
#endif
    }

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

    if (restore_state) {
	if (monitor) {
	    printf("exit ldb monitor to restore\n");
	    ldb_monitor();
	}
	restore();
    }
    else
	call_on_stack(monitor ? call_ldb_monitor : call_initial_function,
#ifdef NUMBER_STACK_GROWS_UP
			NUMBER_STACK_START
#else
			NUMBER_STACK_START+NUMBER_STACK_SIZE
#endif
			);
}
