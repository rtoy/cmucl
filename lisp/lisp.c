/*
 * main() entry point for a stand alone lisp image.
 *
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/lisp.c,v 1.27 2003/01/23 21:05:38 toy Exp $
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <limits.h>
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
#include "interr.h"
#if defined GENCGC
#include "gencgc.h"
#endif
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
#if ( defined( __linux__ ) && defined( i386 ) )
  GET_CONTEXT
#endif

    SAVE_CONTEXT();

    printf("\nSIGINT hit at 0x%08lX\n", SC_PC(context));
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

int main(int argc, char *argv[], char *envp[])
{
    char *arg, **argptr;
    char *core = NULL, *default_core;
    boolean monitor;
    lispobj initial_function;

#if defined(SVR4) || defined(__linux__)
    tzset();
#endif

    set_lossage_handler(ldb_monitor);

    monitor = FALSE;
#ifdef DEFAULT_DYNAMIC_SPACE_SIZE
    dynamic_space_size = DEFAULT_DYNAMIC_SPACE_SIZE;
#else
    dynamic_space_size = DYNAMIC_SPACE_SIZE;
#endif

    argptr = argv;
    while ((arg = *++argptr) != NULL)
      {
        if (strcmp(arg, "-core") == 0)
	  {
            if (core != NULL)
	      {
                fprintf(stderr, "can only specify one core file.\n");
                exit(1);
	      }
            core = *++argptr;
            if (core == NULL)
	      {
		fprintf(stderr, "-core must be followed by the name of the core file to use.\n");
                exit(1);
	      }
	  }
        else if (strcmp(arg, "-dynamic-space-size") == 0)
	  {
            char *str = *++argptr;
            if (str == NULL)
	      {
                fprintf(stderr, "-dynamic-space-size must be followed by the size to use in MBytes.\n");
                exit(1);
	      }
	    dynamic_space_size = atoi(str) * 1024 * 1024;
	    if (dynamic_space_size > DYNAMIC_SPACE_SIZE)
	      {
                fprintf(stderr, "-dynamic-space-size must be no greater than %d MBytes.\n",
			DYNAMIC_SPACE_SIZE / (1024 * 1024));
                exit(1);
	      }
	  }
	else if (strcmp(arg, "-monitor") == 0)
	  {
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
	static char buf[_POSIX_PATH_MAX];
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

	    if (core == NULL)
                fprintf(stderr, "WARNING: Couldn't find core in specified CMUCLLIB, using default path.\n");

	}
	if (core == NULL) {
	    /* Note: the /usr/misc/.cmucl/lib/ default path is also wired
	       into the lisp code in .../code/save.lisp. */
#ifdef MACH
	    strcpy(buf, "/usr/misc/.cmucl/lib/");
#else
	    strcpy(buf, "/usr/local/lib/cmucl/lib/");
#endif
	    strcat(buf, default_core);
	    core = buf;
	}
    }

    os_init();
    validate();
    gc_init();

    /* This is the first use of malloc() and must come after the
     * static memory layout is mmapped to avoid conflicts with possible
     * use of mmap() by malloc().
     */
    define_var("nil", NIL, TRUE);
    define_var("t"  ,   T, TRUE);

    globals_init();

    initial_function = load_core_file(core);
#if defined LINKAGE_TABLE
    os_foreign_linkage_init();
#endif /* LINKAGE_TABLE */

#if defined GENCGC
    gencgc_pickup_dynamic();
#else
#if defined WANT_CGC && defined X86_CGC_ACTIVE_P
    {
      extern int use_cgc_p;
      lispobj x = SymbolValue(X86_CGC_ACTIVE_P);
      if(x != type_UnboundMarker && x != NIL)
	use_cgc_p = 1;		/* enable allocator */
    }
#endif
#endif

#ifdef BINDING_STACK_POINTER
    SetSymbolValue(BINDING_STACK_POINTER, (lispobj)binding_stack);
#endif
#if defined INTERNAL_GC_TRIGGER && !defined i386
    SetSymbolValue(INTERNAL_GC_TRIGGER, make_fixnum(-1));
#endif

    interrupt_init();

    arch_install_interrupt_handlers();
    os_install_interrupt_handlers();

#ifdef PSEUDO_ATOMIC_ATOMIC
    /* Turn on pseudo atomic for when we call into lisp. */
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(1));
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(0));
#endif

    /* Convert the argv and envp to something Lisp can grok. */
    SetSymbolValue(LISP_COMMAND_LINE_LIST, alloc_str_list(argv));
    SetSymbolValue(LISP_ENVIRONMENT_LIST, alloc_str_list(envp));

    /*
     * Parse the command line again, picking up values that override
     * those loaded from the core.
     */

    argptr = argv;
    while ((arg = *++argptr) != NULL)
      {
	if (strcmp(arg, "-batch") == 0)
	  SetSymbolValue(BATCH_MODE, T);
      }

    /*
     * Pick off sigint until the lisp system gets far enough along to
     * install it's own.
     */
    sigint_init();

    if (monitor)
	while (1)
	    ldb_monitor();
    else {
	funcall0(initial_function);
	printf("Initial function returned?\n");
	exit(1);
    }
    return 0;                   /* not reached */
}
