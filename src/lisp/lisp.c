/*
 * main() entry point for a stand alone lisp image.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>

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
#if defined(FEATURE_EXECUTABLE)
#include "elf.h"
#endif

#ifdef __linux__
#include <sys/utsname.h>
#endif

#if defined(__linux__)
#include <time.h>
#endif

#include "cmucl-version.h"

#ifndef CMUCL_VERSION
#error CMUCL_VERSION not defined!
#endif



/* SIGINT handler that invokes the monitor. */

static void
sigint_handler(HANDLER_ARGS)
{
    os_context_t *os_context = (os_context_t *) context;
    
    SAVE_CONTEXT();

    printf("\nSIGINT hit at 0x%08lX\n", (unsigned long) SC_PC(os_context));
    ldb_monitor();
}

/* Not static, because we want to be able to call it from lisp land. */
void
sigint_init(void)
{
    install_handler(SIGINT, sigint_handler);
}


/* Noise to convert argv and envp into lists. */

static lispobj
alloc_str_list(const char *list[])
{
    lispobj result, newcons;
    struct cons *ptr;

    if (*list == NULL)
	result = NIL;
    else {
	result = newcons = alloc_cons(alloc_string(*list++), NIL);

	while (*list != NULL) {
	    ptr = (struct cons *) PTR(newcons);
	    newcons = alloc_cons(alloc_string(*list++), NIL);
	    ptr->cdr = newcons;
	}
    }

    return result;
}

/* Default paths for CMUCLLIB */

static char cmucl_version[] = CMUCL_VERSION;

static char *cmucllib_search_list[] = {
    "./../lib/cmucl/" CMUCL_VERSION "/lib",
    NULL
};

void
getcwd_or_die(char* buf, size_t size)
{
    char *result = getcwd(buf, size);

    if (result == NULL) {
        perror("Cannot get cwd");
        exit(1);
    }
}

/* Set this to see how we're doing our search */
int debug_lisp_search = TRUE;

/*
 * Define this to get some debugging printfs for searching for the
 * lisp core file.  Sometimes needed because you can't debug this with
 * gdb which always seems to set argv[0] to the full pathname.
 */

/* #define DEBUG_LISP_SEARCH */

/*
 * From the current location of the lisp executable, create a suitable
 * default for CMUCLLIB.  The result is a colon-separated list
 * directories to be used for finding the core file and for the cmucl
 * libraries.
 *
 * The caller must free the returned string.
 */
static const char *
default_cmucllib(const char *argv0arg)
{
    int total_len;
    int cwd_len;
    char **ptr;
    char *defpath;
    char *cwd;
    char *newpath = malloc(PATH_MAX);

    cwd = realpath(argv0arg, newpath);

    if (debug_lisp_search) {
	fprintf(stderr, "Realpath of %s = %s\n", argv0arg, newpath);
    }

    if (!cwd) {
	perror("Cannot determine realpath of lisp executable");
	exit(1);
    }

    /*
     * Delete the binary name from the full path, leaving just the
     * full directory to the executable.
     */
    cwd = strrchr(newpath, '/');
    if (cwd) {
	cwd[1] = '\0';
    }

    /* cwd is the path directory of the executable */
    cwd = newpath;

    if (debug_lisp_search) {
	fprintf(stderr, "Executable path %s\n", cwd);
    }

    /*
     * Create the appropriate value for CMUCLLIB by adding the
     * executable path (if needed) to each entry in
     * cmucllib_search_list.
     */

    /* First figure out how much space we need */

    total_len = 0;
    cwd_len = strlen(cwd);

    ptr = cmucllib_search_list;

    while (*ptr != NULL) {
	/*
	 * Plus 2 for the ":" and "/" we need to add and the cwd that
	 * might be added.
	 */
	total_len += strlen(*ptr) + cwd_len + 2;
	++ptr;
    }

    /* Create the colon separated list of directories */

    defpath = malloc(total_len + 1);
    *defpath = '\0';

    ptr = cmucllib_search_list;
    while (*ptr != NULL) {
	/*
	 * If it's relative, add the full executable path first to
	 * make the path absolute.
	 */
	if (*ptr[0] != '/') {
	    strcat(defpath, cwd);
	}

	strcat(defpath, *ptr);

	/* Add a colon if we're not at the last entry of the search list */
	if (ptr[1] != NULL) {
	    strcat(defpath, ":");
	}

	++ptr;
    }

    if (strlen(defpath) > total_len) {
	abort();
    }

    free(newpath);
    return (const char *) defpath;
}

/*
 * Search the a core file with the name given by default_core in the
 * colon-separated list of directories given by lib.
 *
 * Return the full path, if found, or NULL if not.
 */

char *
search_core(const char *lib, const char *default_core)
{
    char *buf;
    char *dst;

    /*
     * A buffer that's large enough to hold lib, default_core, a
     * slash, and a the string terminator
     */
    buf = malloc(strlen(lib) + strlen(default_core) + 2);

    do {
	dst = buf;
	/*
	 * Extract out everything to the first colon, then append a
	 * "/" and the core name.  See if the file exists.
	 */
	while (*lib != '\0' && *lib != ':')
	    *dst++ = *lib++;
	if (dst != buf && dst[-1] != '/')
	    *dst++ = '/';
	strcpy(dst, default_core);
	/* If it exists, we are done! */

	if (debug_lisp_search) {
	    fprintf(stderr, "Looking at `%s'\n", buf);
	}

	if (access(buf, R_OK) == 0) {
	    if (debug_lisp_search) {
		fprintf(stderr, "Found it!\n");
	    }

	    return buf;
	} else {
	    if (debug_lisp_search) {
		fprintf(stderr, "Does not exist, or can't read it if it does!\n");
	    }
	}
    } while (*lib++ == ':');

    free(buf);
    return NULL;
}

/*
 * Given the path to a core file, prepend the absolute location of the
 * core file to the lib path.
 *
 * Return the new lib path.
 */
static const char *
prepend_core_path(const char *lib, const char *corefile)
{
    char cwd[FILENAME_MAX];
    char *path;
    char *result;
    char *sep;

    if (*corefile == '/') {
	path = strdup(corefile);
    } else {
	/*
	 * We have a relative path for the corefile.  Prepend our current
	 * directory to get the full path.
	 */
	getcwd_or_die(cwd, FILENAME_MAX);
	path = malloc(FILENAME_MAX + strlen(corefile) + 2);
	strcpy(path, cwd);
	strcat(path, "/");
	strcat(path, corefile);
    }

    /*
     * Now remove the name portion by finding the last slash.
     */
    sep = strrchr(path, '/');
    if (sep != NULL) {
	*sep = '\0';
    }

    result = malloc(strlen(path) + strlen(lib) + 2);
    strcpy(result, path);
    strcat(result, ":");
    strcat(result, lib);

    free(path);
    return (const char *) result; /* Don't let the caller modify the buffer we built */
}

/*
 * The value of the variable builtin_image_flag indicate whether the
 * executable contains the lisp image or not.  The variable
 * initial_function_addr indicates the address of the initial
 * function.  How these are interpreted depends on the system.
 *
 * For Linux/x86, Darwin/x86, and Solaris/sparc, the
 * builtin_image_flag is a normal symbol mapped into a normal data
 * area.  If true, the executable contains the lisp image.  Likewise,
 * initial_function_addr is a symbol mapped into a normal data area.
 * The value of this variable is the address of the initial function.
 *
 * For other systems, we use the linker to set the value of the symbol.
 * But the symbol is an address, not a variable value.  So for this to
 * work as a flag, it must end up pointing to a valid place in memory
 * or we'll get a bus error or segmentation violation when we check
 * it.  If the lisp image is built in, we'll set this symbol to point
 * to the beginning of the process.
 *
 * We also use the linker to set initial_function_addr so that if the
 * lisp core is built in, taking the address of initial_function_addr
 * will give the address of the initial function.
 *
 * The details of how these variables are set up are in
 * tools/linker.sh and tools/linker-x86.sh.  Which script is used is
 * set in src/lisp/elf.h.
 */

extern int builtin_image_flag;
extern long initial_function_addr;

fpu_mode_t fpu_mode = SSE2;

static const char*
locate_core(const char* cmucllib, const char* core, const char* default_core)
{
    if (core == NULL) {
        if (getenv("CMUCLCORE") == NULL) {
            core = search_core(cmucllib, default_core);
        } else {
            core = getenv("CMUCLCORE");
        }
    }

    if (access(core, R_OK) != 0) {
      core = NULL;
    }
    
    return core;
}

static void
core_failure(const char* core, const char* argv[])
{
    
    fprintf(stderr, "Cannot find core file");
    if (core != NULL) {
        fprintf(stderr, ": `%s'", core);
    }
    fprintf(stderr, "\n");
    fprintf(stderr, "Based on lisp binary path `%s'\n", argv[0]);
    exit(1);
}

/*
 * Match the actual command line option "arg" with the arg name in
 * "argname".  The option matches if it is exacty the arg name
 * prefixed by either one or two "-" characters.
 *
 * Returns non-zero if it matches.
 */
int match_option(const char* arg, const char* argname)
{
    if ((strlen(arg) < 2) || strlen(argname) < 1) {
	/*
	 * The actual arg must be at least 2 characters.  The argname
	 * must have at least 1.
	 */
	return 0;
    }

    /* Must start with a "-" */
    if (arg[0] != '-') {
	return 0;
    }

    if (strcmp(arg + 1, argname) == 0) {
	/* We have "-" followed by the argname.  That's a match. */
	return 1;
    }
    
    if ((arg[1] == '-') && (strcmp(arg + 2, argname) == 0)) {
	/* We have "--" followed by the argname.  That's a match. */
	return 1;
    }

    return 0;
}
    
int
main(int argc, const char *argv[], const char *envp[])
{
    const char *arg, **argptr;
    const char *core = NULL;
    const char *default_core;
    const char *lib = NULL;
    const char *cmucllib = NULL;
    const char *unidata = NULL;
    
    fpu_mode_t fpu_type = SSE2;
    boolean monitor;
    lispobj initial_function = 0;

    if (builtin_image_flag != 0) {
#if defined(SOLARIS) || defined(DARWIN) || (defined(i386) && (defined(__linux__) || defined(__FreeBSD__) || defined(__NetBSD__)))
        initial_function = (lispobj) initial_function_addr;
#else
        initial_function = (lispobj) & initial_function_addr;
#endif
    }
    

    /*
     * Do any special OS initialization that needs to be done early.
     * In particular, on Linux, we might re-exec ourselves to set our
     * personality correctly.  Not normally a problem, but this does
     * cause any output to happen twice.  That can be confusing.
     *
     * So make sure we don't do any output before this point!
     */
    
    os_init0(argv, envp);
#if defined(SVR4)
    tzset();
#endif

    set_lossage_handler(ldb_monitor);

    monitor = FALSE;

#ifdef DEFAULT_DYNAMIC_SPACE_SIZE
    dynamic_space_size = DEFAULT_DYNAMIC_SPACE_SIZE;
#else
    dynamic_space_size = DYNAMIC_SPACE_SIZE;
#endif
#ifdef DEFAULT_READ_ONLY_SIZE
    read_only_space_size = DEFAULT_READ_ONLY_SIZE;
#else
    read_only_space_size = READ_ONLY_SPACE_SIZE;
#endif
#ifdef DEFAULT_STATIC_SIZE
    static_space_size = DEFAULT_STATIC_SIZE;
#else
    static_space_size = STATIC_SPACE_SIZE;
#endif
#ifdef DEFAULT_BINDING_SIZE
    binding_stack_size = DEFAULT_BINDING_SIZE;
#else
    binding_stack_size = BINDING_STACK_SIZE;
#endif    
#ifdef DEFAULT_CONTROL_SIZE
    control_stack_size = DEFAULT_CONTROL_SIZE;
#else
    control_stack_size = CONTROL_STACK_SIZE;
#endif

    argptr = argv;
    while ((arg = *++argptr) != NULL) {
	if (match_option(arg, "core")) {
	    if (builtin_image_flag) {
		fprintf(stderr,
			"Warning:  specifying a core file with an executable image is unusual,\nbut should work.\n");
                builtin_image_flag = 0;
	    }

	    if (core != NULL) {
		fprintf(stderr, "can only specify one core file.\n");
		exit(1);
	    }
	    core = *++argptr;
	    if (core == NULL) {
		fprintf(stderr,
			"%s must be followed by the name of the core file to use.\n",
			arg);
		exit(1);
	    }
	} else if (match_option(arg, "lib")) {
	    lib = *++argptr;
	    if (lib == NULL) {
		fprintf(stderr,
			"%s must be followed by a string denoting the CMUCL library path.\n",
			arg);
		exit(1);
	    }
        } else if (match_option(arg, "read-only-space-size")) {
            const char *str = *++argptr;

            if (str == NULL) {
                fprintf(stderr,
                        "%s must be followed by the size in MBytes.\n",
			arg);
                exit(1);
            }
            read_only_space_size = atoi(str) * 1024 * 1024;
            if (read_only_space_size > READ_ONLY_SPACE_SIZE) {
                fprintf(stderr,
                        "%s must be no greater than %lu MBytes.\n",
			arg,
                        READ_ONLY_SPACE_SIZE / (1024 * 1024UL));
                fprintf(stderr, "  Continuing with default size.\n");
                read_only_space_size = READ_ONLY_SPACE_SIZE;
            }
        } else if (match_option(arg, "static-space-size")) {
            const char *str = *++argptr;

            if (str == NULL) {
                fprintf(stderr,
                        "%s must be followed by the size in MBytes.\n",
			arg);
                exit(1);
            }
            static_space_size = atoi(str) * 1024 * 1024;
            if (static_space_size > STATIC_SPACE_SIZE) {
                fprintf(stderr,
                        "%s must be no greater than %lu MBytes.\n",
			arg,
                        STATIC_SPACE_SIZE / (1024 * 1024UL));
                fprintf(stderr, "  Continuing with default size.\n");
                static_space_size = STATIC_SPACE_SIZE;
            }
        } else if (match_option(arg, "binding-stack-size")) {
            const char *str = *++argptr;

            if (str == NULL) {
                fprintf(stderr,
                        "%s must be followed by the size in MBytes.\n",
			arg);
                exit(1);
            }
            binding_stack_size = atoi(str) * 1024 * 1024;
            if (binding_stack_size > BINDING_STACK_SIZE) {
                fprintf(stderr,
                        "%s must be no greater than %lu MBytes.\n",
			arg,
                        BINDING_STACK_SIZE / (1024 * 1024UL));
                fprintf(stderr, "  Continuing with default size.\n");
                binding_stack_size = BINDING_STACK_SIZE;
            }
        } else if (match_option(arg, "control-stack-size")) {
            const char *str = *++argptr;

            if (str == NULL) {
                fprintf(stderr,
                        "%s must be followed by the size in MBytes.\n",
			arg);
                exit(1);
            }
            control_stack_size = atoi(str) * 1024 * 1024;
            if (control_stack_size > CONTROL_STACK_SIZE) {
                fprintf(stderr,
                        "%s must be no greater than %lu MBytes.\n",
			arg,
                        CONTROL_STACK_SIZE / (1024 * 1024UL));
                fprintf(stderr, "  Continuing with default size.\n");
                control_stack_size = CONTROL_STACK_SIZE;
            }
	} else if (match_option(arg, "dynamic-space-size")) {
	    const char *str;

	    str = *++argptr;
	    if (str == NULL) {
		fprintf(stderr,
			"%s must be followed by the size to use in MBytes.\n",
			arg);
		exit(1);
	    }
#ifndef sparc
	    dynamic_space_size = atoi(str);

	    /*
	     * A size of 0 means using the largest possible space
	     */
	    if (dynamic_space_size == 0) {
		dynamic_space_size = DYNAMIC_SPACE_SIZE;
	    } else {
		dynamic_space_size *= 1024 * 1024;
	    }
#else
	    {
		int val;

		/*
		 * Martin Rydstrom says core sizes that aren't a
		 * multiple of 8 MB eventually causes GC lossage with
		 * gencgc on Solaris 10.  No one seems to understand why
		 * that is, but it is.  So here we enforce the 8 MB
		 * boundary by rounding up the size.  We print a warning
		 * message if we do have to round.
		 *
		 * We do this for all versions, since it doesn't hurt
		 * other versions of Solaris.
		 */
		val = atoi(str);
		dynamic_space_size = (val + 7) & ~7;

		if (val != dynamic_space_size) {
		    fprintf(stderr,
			    "Note:  Rounding dynamic-space-size from %d MB to %d MB\n",
			    val, dynamic_space_size);
		}
		if (dynamic_space_size == 0) {
		    dynamic_space_size = DYNAMIC_SPACE_SIZE;
		} else {
                    dynamic_space_size *= 1024 * 1024;
                }
	    }
#endif
	    if (dynamic_space_size > DYNAMIC_SPACE_SIZE) {
		fprintf(stderr,
			"%s must be no greater than %lu MBytes.\n",
			arg,
			DYNAMIC_SPACE_SIZE / (1024 * 1024UL));
		exit(1);
	    }
	} else if (match_option(arg, "monitor")) {
	    monitor = TRUE;
	} else if (match_option(arg, "debug-lisp-search")) {
	    debug_lisp_search = TRUE;
        } else if (match_option(arg, "unidata")) {
          unidata = *++argptr;
        } else if ((strcmp(arg, "-version") == 0) ||
		   (strcmp(arg, "--version") == 0)) {
	    /*
	     * Print the version and exit; we don't want to do
	     * anything else!
	     */
	    printf("%s\n", cmucl_version);
	    return 0;
	}
    }

    default_core = arch_init(fpu_mode);

    if (default_core == NULL)
	default_core = "lisp.core";

    os_init(argv, envp);
#if defined FEATURE_EXECUTABLE
    if (builtin_image_flag != 0)
	map_core_sections(argv[0]);
#endif

    /*
     * Validate the basic lisp spaces first like the heap and static
     * and read-only spaces.  Do this so that the stacks (if thy're
     * relocatable) don't get randomly allocated on top of our desired
     * lisp spaces.
     */
    validate();
    gc_init();
    validate_stacks();

    /* This is the first use of malloc() and must come after the
     * static memory layout is mmapped to avoid conflicts with possible
     * use of mmap() by malloc().
     */
    define_var("nil", NIL, TRUE);
    define_var("t", T, TRUE);

    /*
     * Basic algorithm for setting CMUCLLIB and CMUCLCORE, from Pierre
     * Mai.
     *
     * if CMUCLLIB envvar is not set
     *   CMUCLLIB = our list of places to look
     *   if -core option/CMUCLCORE given
     *      CMUCLLIB = CMUCLLIB + full path to the specified core file
     *   endif
     * endif
     *
     * if -core option/CMUCLCORE unset
     *   search for a core file (named whatever arch_init returns or
     *     lisp.core) somewhere in the CMUCLLIB list.
     * endif
     *
     * if core found
     *   use that
     * else
     *   give error message and die
     * endif
     *
     * CMUCLCORE = where the core file was found/specced
     */

    /*
     * Set cmucllib to the -lib option, or to CMUCLLIB envvar.  If
     * neither are set, set cmucllib to our default search path.
     */
    if (lib != NULL) {
	cmucllib = strdup(lib);
    } else {
	char *libvar;

	libvar = getenv("CMUCLLIB");
	if (libvar != NULL) {
	    cmucllib = strdup(libvar);
	} else {
	    /*
             * The following doesn't make sense for executables.  They
             * need to use the saved library path from the lisp from
             * which they were dumped.
             */
	    if (builtin_image_flag == 0) {
                const char *newlib = NULL;

                /*
                 * We need to use our default search path.  If a core file
                 * is given, we prepend the directory of the core file to
                 * the search path.
                 */
                cmucllib = default_cmucllib(argv[0]);
                if (core != NULL) {
                    newlib = prepend_core_path(cmucllib, core);
                } else if (getenv("CMUCLCORE") != NULL) {
                    core = getenv("CMUCLCORE");
                    newlib = prepend_core_path(cmucllib, core);
                }

                if (newlib != NULL) {
                    free((void *) cmucllib);
                    cmucllib = newlib;
                }
            }
        }
    }


    /* Only look for a core file if we're not using a built-in image. */
    if (builtin_image_flag == 0) {
	/*
	 * If no core file specified, search for it in CMUCLLIB
	 */
        const char* found_core;
            
        found_core = locate_core(cmucllib, core, default_core);
#ifdef FEATURE_SSE2
        if ((found_core == NULL) && (fpu_mode == AUTO)) {
            /*
             * If we support SSE2 but couldn't find the SSE2 core, try
             * to fall back to the x87 core.
             */
            found_core = locate_core(cmucllib, core, "lisp-x87.core");
            if (found_core == NULL) {
                core_failure(core, argv);
            }
            fprintf(stderr, "Warning:  Chip supports SSE2, but could not find SSE2 core.\n");
            fprintf(stderr, "  Falling back to x87 core.\n");
        }
#endif
        if (!found_core) {
            core_failure(core, argv);
        }
        core = found_core;
    } else {
	/*
         * The "core file" is the executable.  We have to save the
         * executable path because we operate on the executable file
         * later.
	 */
	core = argv[0];
    }

    globals_init();

    if (builtin_image_flag != 0) {
	extern int image_dynamic_space_size;
	long allocation_pointer =
	    (long) dynamic_0_space + (int) image_dynamic_space_size;
#if defined(ALLOCATION_POINTER) && !defined(ibmrt)
	SetSymbolValue(ALLOCATION_POINTER, (lispobj) allocation_pointer);
#else
	current_dynamic_space_free_pointer = (lispobj *) allocation_pointer;
#endif
    } else {
	initial_function = load_core_file(core, &fpu_type);
    }

#ifdef i386
    if ((fpu_type == SSE2) && (!arch_support_sse2() || !os_support_sse2())) {
	fprintf(stderr, "Core uses SSE2, but CPU/OS doesn't support SSE2.  Exiting\n");
	exit(1);
    }
    fpu_mode = fpu_type;
#endif

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

	if (x != type_UnboundMarker && x != NIL)
	    use_cgc_p = 1;	/* enable allocator */
    }
#endif
#endif

#ifdef BINDING_STACK_POINTER
    SetSymbolValue(BINDING_STACK_POINTER, (lispobj) binding_stack);
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

    /* Set cmucllib and cmuclcore appropriately */
    /*
     * This test will preserve the library: search list dumped with
     * the executable unless the user specifically overrides it with
     * the -lib flag or by setting the CMUCLLIB environment variable.
     */

    if (cmucllib) {
	SetSymbolValue(CMUCL_LIB, alloc_string(cmucllib));
    }
    
    SetSymbolValue(CMUCL_CORE_PATH, alloc_string(core));

    /*
     * Parse the command line again, picking up values that override
     * those loaded from the core.
     */

    argptr = argv;
    while ((arg = *++argptr) != NULL) {
	if (match_option(arg, "batch"))
	    SetSymbolValue(BATCH_MODE, T);
    }

#ifdef UNIDATA_PATH
    if (unidata) {
      SetSymbolValue(UNIDATA_PATH, alloc_string(unidata));
    }
#endif
    
#ifdef LISP_IMPLEMENTATION_VERSION
    SetSymbolValue(LISP_IMPLEMENTATION_VERSION, alloc_string(cmucl_version));
#endif

    /*
     * Pick off sigint until the lisp system gets far enough along to
     * install it's own.
     */
    sigint_init();

#ifdef DEBUG_BAD_HEAP
    /*
     * At this point, there should be exactly 4 objects in static
     * space pointing to apparently free pages.  These 4 objects were
     * just created above for *lisp-command-line-list*,
     * *lisp-environment-list*, *cmucl-lib*, and *cmucl-core-path*.
     */
    verify_gc();
#endif

#if defined(__linux__)
    /*
     * On newer (?) versions of Linux, tzset appears to call malloc.
     * We set up the timezone here so that malloc happens as late as
     * possible.
     */
    tzset();
#endif
    
    if (monitor) {
	while (1) {
	    ldb_monitor();
	}
    } else {
	funcall0(initial_function);
	printf("Initial function returned?\n");
	exit(1);
    }
    return 0;			/* not reached */
}
