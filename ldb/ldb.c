/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/ldb.c,v 1.7 1990/07/01 04:42:29 wlott Exp $ */
/* Lisp kernel core debugger */

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <mach.h>

#include "ldb.h"
#include "lisp.h"
#include "alloc.h"
#include "vars.h"

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
    boolean restore_state;

    define_var("nil", NIL, TRUE);
    define_var("t", T, TRUE);

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
    }

    if (core == NULL)
        core = "test.core";

    os_init();

#if defined(EXT_PAGER)
    pager_init();
#endif

    gc_init();
    
    validate();

    globals_init();

    restore_state = load_core_file(core);

    interrupt_init();

    /* Convert the argv and envp to something Lisp can grok. */
    SetSymbolValue(LISP_COMMAND_LINE_LIST, alloc_str_list(argv));
    SetSymbolValue(LISP_ENVIRONMENT_LIST, alloc_str_list(envp));

    /* Snag a few of the signal */
    test_init();

    if (restore_state)
        restore();
    else
        while (1)
            ldb_monitor();
}
