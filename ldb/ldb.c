/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/ldb.c,v 1.14 1991/09/04 15:35:16 wlott Exp $ */
/* Lisp kernel core debugger */

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/stat.h>

#include "ldb.h"
#include "lisp.h"
#include "alloc.h"
#include "vars.h"
#include "globals.h"

lispobj lisp_nil_reg = NIL;
char *lisp_csp_reg, *lisp_bsp_reg;

static lispobj alloc_str_list(list)
char *list[];
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


main(argc, argv, envp)
int argc;
char *argv[];
char *envp[];
{
    char *arg, **argptr;
    char *core = NULL;
    boolean restore_state, monitor;

    number_stack_start = (char *)&monitor;

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

    /* Note: the /usr/misc/.cmucl/lib/ default path is also wired into */
    /* the lisp code in .../code/save.lisp. */

    if (core == NULL) {
	extern char *getenv();
	char *lib = getenv("CMUCLLIB");

	if (lib == NULL)
	    core = "/usr/misc/.cmucl/lib/lisp.core";
	else {
	    static char buf[MAXPATHLEN];
	    char *dst;
	    struct stat statbuf;

	    do {
		dst = buf;
		while (*lib != '\0' && *lib != ':')
		    *dst++ = *lib++;
		if (dst != buf && dst[-1] != '/')
		    *dst++ = '/';
		strcpy(dst, "lisp.core");
		if (stat(buf, &statbuf) == 0) {
		    core = buf;
		    break;
		}
	    } while (*lib++ == ':');
	    if (core == NULL)
		core = "/usr/misc/.cmucl/lib/lisp.core";
	}
    }

    arch_init();
    os_init();

#if defined(EXT_PAGER)
    pager_init();
#endif

    gc_init();
    
    validate();

    globals_init();

    restore_state = load_core_file(core);

#ifdef ibmrt
    if (!restore_state)
	SetSymbolValue(BINDING_STACK_POINTER, (lispobj)binding_stack);
#endif

    interrupt_init();

    arch_install_interrupt_handlers();
    os_install_interrupt_handlers();

    /* Convert the argv and envp to something Lisp can grok. */
    SetSymbolValue(LISP_COMMAND_LINE_LIST, alloc_str_list(argv));
    SetSymbolValue(LISP_ENVIRONMENT_LIST, alloc_str_list(envp));

#ifndef mips
    /* Turn on pseudo atomic for when we call into lisp. */
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, fixnum(1));
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, fixnum(0));
#endif

    /* Snag a few of the signal */
    test_init();

    if (!monitor) {
	if (restore_state)
            restore();
	else
	    call_into_lisp(INITIAL_FUNCTION, SymbolFunction(INITIAL_FUNCTION),
			   current_control_stack_pointer, 0);
	printf("%INITIAL-FUNCTION returned?");
    }
    while (1)
	ldb_monitor();
}
