/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/


#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>

#include "lisp.h"
#include "os.h"
#include "internals.h"
#include "core.h"
#include "globals.h"
#include "save.h"
#include "lispregs.h"
#include "validate.h"
#include "dynbind.h"

#ifdef GENCGC
#include "gencgc.h"
#endif

#ifdef FEATURE_EXECUTABLE
#include "elf.h"
#if !(defined(DARWIN) && defined(__ppc__))
#include <libgen.h>
#endif
#endif

extern int version;

static long
write_bytes(FILE * file, char *addr, long bytes)
{
    long count, here, data;

    bytes = (bytes + CORE_PAGESIZE - 1) & ~(CORE_PAGESIZE - 1);

    fflush(file);
    here = ftell(file);
    fseek(file, 0, 2);
    data = (ftell(file) + CORE_PAGESIZE - 1) & ~(CORE_PAGESIZE - 1);
    fseek(file, data, 0);

    while (bytes > 0) {
	count = fwrite(addr, 1, bytes, file);
	if (count > 0) {
	    bytes -= count;
	    addr += count;
	} else {
	    perror("Error writing to save file");
	    bytes = 0;
	}
    }
    fflush(file);
    fseek(file, here, 0);
    return data / CORE_PAGESIZE - 1;
}

static void
output_space(FILE * file, int id, lispobj * addr, lispobj * end)
{
    int words, bytes, data;
    static char *names[] = { NULL, "Dynamic", "Static", "Read-Only" };

    putw(id, file);
    words = end - addr;
    putw(words, file);

    bytes = words * sizeof(lispobj);

    printf("Writing %d bytes from the %s space at 0x%08lX.\n",
	   bytes, names[id], (unsigned long) addr);

    data = write_bytes(file, (char *) addr, bytes);

    putw(data, file);
    putw((long) addr / CORE_PAGESIZE, file);
    putw((bytes + CORE_PAGESIZE - 1) / CORE_PAGESIZE, file);
}

#ifdef DEBUG_BAD_HEAP
static void
dump_region(struct alloc_region *alloc_region)
{
    fprintf(stderr, "free_pointer = %p\n", alloc_region->free_pointer);
    fprintf(stderr, "end_addr     = %p\n", alloc_region->end_addr);
    fprintf(stderr, "first_page   = %d\n", alloc_region->first_page);
    fprintf(stderr, "last_page    = %d\n", alloc_region->last_page);
    fprintf(stderr, "start_addr   = %p\n", alloc_region->start_addr);

    fprintf(stderr, " page_table[%d]\n", alloc_region->first_page);
    fprintf(stderr, "   flags     = %x\n", page_table[alloc_region->first_page].flags);
    fprintf(stderr, "   offset    = %x\n", page_table[alloc_region->first_page].first_object_offset);
    fprintf(stderr, "   used      = %x\n", page_table[alloc_region->first_page].bytes_used);
}
#endif

boolean
save(char *filename, lispobj init_function, int sse2_mode)
{
    FILE *file;

#if defined WANT_CGC
    volatile lispobj *func_ptr = &init_function;
    char sbuf[128];

    strcpy(sbuf, filename);
    filename = sbuf;
    /* Get rid of remnant stuff. This is a MUST so that
     * the memory manager can get started correctly when
     * we restart after this save. Purify is going to
     * maybe move the args so we need to consider them volatile,
     * especially if the gcc optimizer is working!!
     */
    purify(NIL, NIL);

    init_function = *func_ptr;
    /* Set dynamic space pointer to base value so we don't write out
     * MBs of just cleared heap.
     */
    if (SymbolValue(X86_CGC_ACTIVE_P) != NIL)
	SetSymbolValue(ALLOCATION_POINTER, DYNAMIC_0_SPACE_START);
#endif
    /* Open the file: */
    remove(filename);
    file = fopen(filename, "w");
    if (file == NULL) {
	perror(filename);
	return TRUE;
    }
    printf("[Undoing binding stack... ");
    fflush(stdout);
    unbind_to_here((lispobj *) BINDING_STACK_START);
    SetSymbolValue(CURRENT_CATCH_BLOCK, 0);
    SetSymbolValue(CURRENT_UNWIND_PROTECT_BLOCK, 0);
    SetSymbolValue(EVAL_STACK_TOP, 0);
    printf("done]\n");
#if defined WANT_CGC && defined X86_CGC_ACTIVE_P
    SetSymbolValue(X86_CGC_ACTIVE_P, T);
#endif
    printf("[Saving current lisp image into %s:\n", filename);

    putw(CORE_MAGIC, file);

    putw(CORE_VERSION, file);
#if defined(i386) && defined(FEATURE_SSE2)
    putw(4, file);
#else
    putw(3, file);
#endif
    putw(version, file);

#if defined(i386) && defined(FEATURE_SSE2)
    if (sse2_mode) {
        putw(SSE2, file);
    } else {
        putw(X87, file);
    }
#endif
    
    putw(CORE_NDIRECTORY, file);
    putw((5 * 3) + 2, file);

    output_space(file, READ_ONLY_SPACE_ID, read_only_space,
		 (lispobj *) SymbolValue(READ_ONLY_SPACE_FREE_POINTER));
    output_space(file, STATIC_SPACE_ID, static_space,
		 (lispobj *) SymbolValue(STATIC_SPACE_FREE_POINTER));
#ifdef GENCGC
    /* Flush the current_region updating the tables. */
#ifdef DEBUG_BAD_HEAP
    fprintf(stderr, "before ALLOC_POINTER = %p\n", (lispobj *) SymbolValue(ALLOCATION_POINTER));
    dump_region(&boxed_region);
#endif
    gc_alloc_update_page_tables(0, &boxed_region);
    gc_alloc_update_page_tables(1, &unboxed_region);
#ifdef DEBUG_BAD_HEAP
    fprintf(stderr, "boxed_region after update\n");
    dump_region(&boxed_region);

    print_ptr((lispobj*) 0x2805a184);
#endif
    
#ifdef DEBUG_BAD_HEAP
    /*
     * For some reason x86 has a heap corruption problem.  I (rtoy)
     * have not been able to figure out how that occurs, but what is
     * happening is that when a core is loaded, there is some static
     * object pointing to an object that is on a free page.  In normal
     * usage, at startup there should be 4 objects in static space
     * pointing to a free page, because these are newly allocated
     * objects created by the C runtime.  However, there is an
     * additional object.
     *
     * I do not know what this object should be or how it got there,
     * but it will often cause CMUCL to fail to save a new core file.
     *
     * Disabling this call to update_dynamic_space_free_pointer is a
     * work around.  What is happening is that u_d_s_f_p is resetting
     * ALLOCATION_POINTER, but that weird object is in the current
     * region, but after resetting the pointer, that object isn't
     * saved to the core file.  By not resetting the pointer, the
     * object (or at least enough of it) gets saved in the core file
     * that we don't have problems when reloading.
     *
     * Note that on sparc and ppc, u_d_s_f_p doesn't actually do
     * anything because the call to reset ALLOCATION_POINTER is a nop
     * on sparc and ppc.  And sparc and ppc dont' have the heap
     * corruption issue.  That's not conclusive evidence, though.
     *
     * This needs more work and investigation.
     */
    update_dynamic_space_free_pointer();
#endif

#ifdef DEBUG_BAD_HEAP    
    fprintf(stderr, "after ALLOC_POINTER = %p\n", (lispobj *) SymbolValue(ALLOCATION_POINTER));
#endif    
#endif

#ifdef reg_ALLOC
    output_space(file, DYNAMIC_SPACE_ID, current_dynamic_space,
		 current_dynamic_space_free_pointer);
#else
    output_space(file, DYNAMIC_SPACE_ID, current_dynamic_space,
		 (lispobj *) SymbolValue(ALLOCATION_POINTER));
#endif

    putw(CORE_INITIAL_FUNCTION, file);
    putw(3, file);
    putw(init_function, file);

    putw(CORE_END, file);
    fclose(file);

    printf("done.]\n");

    exit(0);
}


#ifdef FEATURE_EXECUTABLE
boolean
save_executable(char *filename, lispobj init_function)
{
    char *dir_name;

#if defined WANT_CGC
    volatile lispobj *func_ptr = &init_function;
    char sbuf[128];

    strcpy(sbuf, filename);
    filename = sbuf;
    /* Get rid of remnant stuff. This is a MUST so that
     * the memory manager can get started correctly when
     * we restart after this save. Purify is going to
     * maybe move the args so we need to consider them volatile,
     * especially if the gcc optimizer is working!!
     */
    purify(NIL, NIL);

    init_function = *func_ptr;
    /* Set dynamic space pointer to base value so we don't write out
     * MBs of just cleared heap.
     */
    if(SymbolValue(X86_CGC_ACTIVE_P) != NIL)
        SetSymbolValue(ALLOCATION_POINTER, DYNAMIC_0_SPACE_START);
#endif
    dir_name = dirname(strdup(filename));

    printf("[Undoing binding stack... ");
    fflush(stdout);
    unbind_to_here((lispobj *)BINDING_STACK_START);
    SetSymbolValue(CURRENT_CATCH_BLOCK, 0);
    SetSymbolValue(CURRENT_UNWIND_PROTECT_BLOCK, 0);
    SetSymbolValue(EVAL_STACK_TOP, 0);
    printf("done]\n");
#if defined WANT_CGC && defined X86_CGC_ACTIVE_P
    SetSymbolValue(X86_CGC_ACTIVE_P, T);
#endif
    printf("[Saving current lisp image as executable into \"%s\":\n", filename);

    printf("\t[Writing core objects\n");
    fflush(stdout);
    write_space_object(dir_name, READ_ONLY_SPACE_ID, (os_vm_address_t)read_only_space,
                       (os_vm_address_t)SymbolValue(READ_ONLY_SPACE_FREE_POINTER));
    write_space_object(dir_name, STATIC_SPACE_ID, (os_vm_address_t)static_space,
                       (os_vm_address_t)SymbolValue(STATIC_SPACE_FREE_POINTER));
#ifdef GENCGC
    /* Flush the current_region updating the tables. */
#ifdef DEBUG_BAD_HEAP
    fprintf(stderr, "before ALLOC_POINTER = %p\n", (lispobj *) SymbolValue(ALLOCATION_POINTER));
    dump_region(&boxed_region);
#endif
    gc_alloc_update_page_tables(0,&boxed_region);
    gc_alloc_update_page_tables(1,&unboxed_region);
#ifdef DEBUG_BAD_HEAP
    fprintf(stderr, "boxed_region after update\n");
    dump_region(&boxed_region);

    print_ptr((lispobj*) 0x2805a184);
#endif
#ifdef DEBUG_BAD_HEAP
    /*
     * For some reason x86 has a heap corruption problem.  I (rtoy)
     * have not been able to figure out how that occurs, but what is
     * happening is that when a core is loaded, there is some static
     * object pointing to an object that is on a free page.  In normal
     * usage, at startup there should be 4 objects in static space
     * pointing to a free page, because these are newly allocated
     * objects created by the C runtime.  However, there is an
     * additional object.
     *
     * I do not know what this object should be or how it got there,
     * but it will often cause CMUCL to fail to save a new core file.
     *
     * Disabling this call to update_dynamic_space_free_pointer is a
     * work around.  What is happening is that u_d_s_f_p is resetting
     * ALLOCATION_POINTER, but that weird object is in the current
     * region, but after resetting the pointer, that object isn't
     * saved to the core file.  By not resetting the pointer, the
     * object (or at least enough of it) gets saved in the core file
     * that we don't have problems when reloading.
     *
     * Note that on sparc and ppc, u_d_s_f_p doesn't actually do
     * anything because the call to reset ALLOCATION_POINTER is a nop
     * on sparc and ppc.  And sparc and ppc dont' have the heap
     * corruption issue.  That's not conclusive evidence, though.
     *
     * This needs more work and investigation.
     */
    update_dynamic_space_free_pointer();
#endif

#ifdef DEBUG_BAD_HEAP    
    fprintf(stderr, "after ALLOC_POINTER = %p\n", (lispobj *) SymbolValue(ALLOCATION_POINTER));
#endif    
#endif

#ifdef reg_ALLOC
    write_space_object(dir_name, DYNAMIC_SPACE_ID, (os_vm_address_t)current_dynamic_space,
                       (os_vm_address_t)current_dynamic_space_free_pointer);
#else
    write_space_object(dir_name, DYNAMIC_SPACE_ID, (os_vm_address_t)current_dynamic_space,
                       (os_vm_address_t)SymbolValue(ALLOCATION_POINTER));
#endif

    printf("\tdone]\n");
    fflush(stdout);
    
    printf("Linking executable...\n");
    fflush(stdout);
    obj_run_linker(init_function, filename);
    printf("done.\n");
    exit(0);
}
#endif
