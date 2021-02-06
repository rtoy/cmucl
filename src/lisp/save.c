/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/


#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

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

/* Like (ceiling x y), but y is constrained to be a power of two */
#define CEILING(x,y) (((x) + ((y) - 1)) & (~((y) - 1)))

#define NWORDS(x,y) (CEILING((x),(y)) / (y))

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

    unbind_to_here((lispobj *) binding_stack);

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
    char *dir_copy;
    int rc;

    void init_asmtab(void);
    void write_asm_object(const char *dir, int id, os_vm_address_t start, os_vm_address_t end);

    init_asmtab();
    
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
    dir_copy = strdup(filename);
    dir_name = dirname(dir_copy);

    printf("[Undoing binding stack... ");
    fflush(stdout);

    unbind_to_here((lispobj *)binding_stack);

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


    write_asm_object(dir_name, READ_ONLY_SPACE_ID, (os_vm_address_t)read_only_space,
                     (os_vm_address_t)SymbolValue(READ_ONLY_SPACE_FREE_POINTER));
    write_asm_object(dir_name, STATIC_SPACE_ID, (os_vm_address_t)static_space,
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
    rc = obj_run_linker(init_function, filename);
    printf("done.\n");
    free(dir_copy);
    exit(rc);
}
#endif



/*
 * Table for each header type value.  The entry is a function to
 * handle the type value when printing out the assembly code.
 */
static int (*asmtab[256])(lispobj* where, lispobj object, FILE* f);

void
asm_label(lispobj* ptr, lispobj object, FILE* f) 
{
    fprintf(f, "L%lx:\n", (unsigned long) ptr);
}

void
asm_word(lispobj* ptr, lispobj object, FILE* f)
{
    unsigned long val = (unsigned long) object;
    
    fprintf(f, "\t.4byte\t0x%lx\t# %ld\n", val, val);
}

void
asm_lispobj(lispobj* ptr, lispobj object, FILE* f)
{
    fprintf(f, "\t.4byte\t");
    if ((object & 3) == 0) {
        /* A fixnum */
        fprintf(f, "0x%lx\t# fixnum %ld\n",
                (long) object,
                ((long) object) >> 2);
    } else {
        fprintf(f, "L%lx + %lu\n", PTR(object), LowtagOf(object));
    }
}

void
asm_align(FILE* f)
{
    /*
     * Align the location counter to an 8-byte boundary that we need
     * for lisp, since all objects must be multiples of 8 bytes.
     *
     * This should be called
     */
    fprintf(f, "\t.align\t8\n");
}

void
asm_header_word(lispobj* ptr, lispobj object, FILE* f, const char* note)
{
    unsigned long len = HeaderValue(object);
    unsigned long type = TypeOf(object);
    
    fprintf(f, "\t.4byte\t0x%lx << 8 + %ld\t# %s\n", len, type, note);
}

    
int
asm_ni(lispobj* ptr, lispobj object, FILE* f)
{
    asm_label(ptr, object, f);
    fprintf(f, "\t.4byte\t0x%lx\t# NOT IMPLEMENTED\n",
            object);
    return 1;
}

/*
 * Handles all objects that consists of only of lispobjs
 */
int
asm_boxed(lispobj* ptr, lispobj object, FILE* f) 
{
    int len = 1 + HeaderValue(object);
    len = CEILING(len, 2);
    int k;

    asm_label(ptr, object, f);

    asm_header_word(ptr, object, f, "");
    
    for (k = 1; k < len; ++k) {
        asm_lispobj(ptr + k, ptr[k], f);
    }

    return len;
}

int
asm_immediate(lispobj* ptr, lispobj object, FILE* f)
{
    asm_label(ptr, object, f);
    
    fprintf(f, "\t.4byte\t0x%lx\t# immediate\n", object);

    return 1;
}

int
asm_list_pointer(lispobj* ptr, lispobj object, FILE* f)
{
    asm_label(ptr, object, f);

    asm_lispobj(ptr, object, f);

    return 1;
}

int
asm_function_pointer(lispobj* ptr, lispobj object, FILE* f)
{
#if 0
    printf("function pointer 0x%lx\n", object);
#endif

    asm_label(ptr, object, f);
    asm_lispobj(ptr, object, f);
    return 1;
}


int
asm_other_pointer(lispobj* ptr, lispobj object, FILE* f)
{
    asm_label(ptr, object, f);
    asm_lispobj(ptr, object, f);
    return 1;
}

int
asm_fdefn(lispobj* ptr, lispobj object, FILE* f)
{
    asm_label(ptr, object, f);
    
    asm_header_word(ptr, object, f, "fdefn");
    asm_lispobj(ptr + 1, ptr[1], f);
    asm_lispobj(ptr + 2, ptr[2], f);

    fprintf(f, "\t.4byte\tL%lx\t# raw_addr\n", (unsigned long) ptr[3]);

    return 4;
}

int
asm_instance_pointer(lispobj* ptr, lispobj object, FILE* f)
{
    asm_label(ptr, object, f);
    asm_lispobj(ptr, object, f);
    return 1;
}

int
asm_simple_vector(lispobj* ptr, lispobj object, FILE* f)
{
    int k;
    int len = ptr[1] >> 2;
    len = CEILING(len, 2);
    lispobj* data = ptr + 2;
    
    asm_label(ptr, object, f);
    asm_header_word(ptr, object, f, "simple vector");
    asm_lispobj(ptr + 1, ptr[1], f);

    for (k = 0; k < len; ++k) {
        asm_lispobj(data + k,  data[k], f);
    }
    
    return len + 2;
}

int
asm_closure_header(lispobj* ptr, lispobj object, FILE* f)
{
    return asm_boxed(ptr, object, f);
}

int
asm_complex_vector(lispobj* ptr, lispobj object, FILE* f)
{
    return asm_ni(ptr, object, f);
}

int
asm_code_header(lispobj* ptr, lispobj object, FILE* f)
{
    struct code *code;
    int nheader_words;
    int ncode_words;
    int nwords;
    int k;

    code = (struct code *) ptr;
    ncode_words = fixnum_value(code->code_size);
    nheader_words = HeaderValue(object);
    nwords = ncode_words + nheader_words;
    nwords = CEILING(nwords, 2);

#if 0
    fprintf(stderr, "nwords = %d nheader_words %d\n",
            nwords, nheader_words);
#endif

    asm_label(ptr, object, f);
    asm_header_word(ptr, object, f, "code header");
    
    for (k = 0; k < nheader_words - 1; ++k) {
        asm_lispobj(ptr + k + 1, ptr[k + 1], f);
    }

    fprintf(f, "# Code bytes?\n");
    
    for (; k < nwords; ++k) {
        fprintf(f, "\t.4byte\t0x%lx\n", ptr[k + 1]);
    }
    
    return nwords;
}

int
asm_simple_string(lispobj* where, lispobj object, FILE* f)
{
    struct vector* vector;
    int length;
    int nwords;
    int k;
    int nchars;
    uint16_t* s;
    
    /*
     * NOTE: Strings contain one more byte of data than the length
     * slot indicates.
     */

    vector = (struct vector *) where;
    length = fixnum_value(vector->length) + 1;
#ifndef UNICODE
#ifdef __x86_64
    nwords = CEILING(NWORDS(length, 8) + 2, 2);
#else
    nwords = CEILING(NWORDS(length, 4) + 2, 2);
#endif
#else
    /*
     * Strings are just like arrays with 16-bit elements, and contain
     * one more element than the slot length indicates.
     */
    nwords = CEILING(NWORDS(length, 2) + 2, 2);
    nchars = 2 * nwords;
#endif

    asm_label(where, object, f);
    asm_header_word(where, object, f, "simple string");
    asm_lispobj(where + 1, where[1], f);
    
    s = (uint16_t*) vector->data;
    
    for (k = 0; k < nchars; ++k) {
        fprintf(f, "\t.2byte\t0x%x\n", s[k]);
    }

    return nwords;
}

int
asm_single_float(lispobj* ptr, lispobj object, FILE* f)
{
    struct single_float* obj = (struct single_float*) ptr;
    
    asm_label(ptr, object, f);
    asm_header_word(ptr, object, f, "single float");
    fprintf(f, "\t.float\t%.15g\n", obj->value);

    return 2;
}

int
asm_double_float(lispobj* ptr, lispobj object, FILE* f)
{
    struct double_float* obj = (struct double_float*) ptr;
    
    asm_label(ptr, object, f);
    asm_header_word(ptr, object, f, "double float");
    asm_lispobj(&obj->filler, obj->filler, f);
    fprintf(f, "\t.double\t%.15g\n", obj->value);

    return 4;
}

int
asm_vector_unsigned_byte_8(lispobj* ptr, lispobj object, FILE* f)
{
    struct vector *vector;
    int length, nwords;
    unsigned long* data;
    int k;
    
    vector = (struct vector *) ptr;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = CEILING(NWORDS(length, 8) + 2, 2);
#else
    nwords = CEILING(NWORDS(length, 4) + 2, 2);
#endif
    asm_label(ptr, object, f);
    asm_header_word(ptr, object, f, "vector unsigned_byte 8");
    asm_lispobj(ptr + 1, ptr[1], f);
    
    data = vector->data;

    /* Minus 2 for the header and length words */
    for (k = 0; k < nwords - 2; ++k) {
        fprintf(f, "\t.4byte\t0x%lx\n", data[k]);
    }
    
    return nwords;
}

int
asm_vector_unsigned_byte_32(lispobj* ptr, lispobj object, FILE* f)
{
    struct vector *vector;
    int length, nwords;
    unsigned long* data;
    int k;
    
    vector = (struct vector *) ptr;
    length = fixnum_value(vector->length);
#ifdef __x86_64
    nwords = CEILING(NWORDS(length, 2) + 2, 2);
#else
    nwords = CEILING(length + 2, 2);
#endif

    asm_label(ptr, object, f);
    asm_header_word(ptr, object, f, "vector unsigned_byte 32");
    asm_lispobj(ptr + 1, ptr[1], f);
    
    data = vector->data;

    /* Minus 2 for the header and length words */
    for (k = 0; k < nwords - 2; ++k) {
        fprintf(f, "\t.4byte\t0x%lx\n", data[k]);
    }
    
    return nwords;
}

#if 0
int
asm_bignum(lispobj* ptr, lispobj object, FILE* f)
{
    int len = HeaderValue(object);
    
    asm_label(ptr, object, f);
    
    asm_lispobj(ptr, object, f);
    ++ptr;
    
    for (k = 0; k < len; ++k) {
        fprintf(f, "\t.4byte\t%d\n", ptr[k]);
    }

    return len;
}

int
asm_catch_block(lispobj* ptr, lispobj object, FILE* f)
{
    return asm_boxed(ptr, object, f);
}

int
asm_catch_block(lispobj* ptr, lispobj object, FILE* f)
{
    return asm_boxed(ptr, object, f);
}

int
asm_closure(lispobj* ptr, lispobj object, FILE* f)
{
    return asm_boxed(ptr, object, f);
}

int
asm_code(lispobj* ptr, lispobj object, FILE* f)
{
    return asm_boxed(ptr, object, f);
}

int
asm_complex(lispobj* ptr, lispobj object, FILE* f)
{
    return asm_boxed(ptr, object, f);
}

int
asm_complex_double_double_float(lispobj* ptr, lispobj object, FILE* f)
{
    asm_label(ptr, object, f);
    asm_lispobj(ptr, object, f);
    asm_lispobj(ptr + 1, ptr[1], f);

    double* d = (double*) (ptr + 2);
    
    for (k = 0; k < 4; ++k) {
        fprintf(f, "\t.double\t%.15lg\n", d[k]);
    }

    return HeaderValue(object);
}

int
asm_complex_double_float(lispobj* ptr, lispobj object, FILE* f)
{
    asm_label(ptr, object, f);
    asm_lispobj(ptr, object, f);
    asm_lispobj(ptr + 1, ptr[1], f);

    double* d = ptr + 2;
    fprintf(f, "\t.double\t%.15lg, %.15lg\n", d[0], d[1]);

    return HeaderValue(object);
}

int
asm_complex_double_float(lispobj* ptr, lispobj object, FILE* f)
{
    asm_label(ptr, object, f);
    asm_lispobj(ptr, object, f);

    double* d = ptr + 2;
    fprintf(f, "\t.double\t%.15lg, %.15lg\n", d[0], d[1]);

    return HeaderValue(object);
}

#endif

void
init_asmtab(void)
{
    int k = 0;

    for (k = 0; k < 256; ++k) {
        asmtab[k] = asm_ni;
    }

    for (k = 0; k < 32; ++k) {
        asmtab[type_EvenFixnum | (k << 3)] = asm_immediate;
        asmtab[type_FunctionPointer | (k << 3)] = asm_function_pointer;
        asmtab[type_OtherImmediate0 | (k << 3)] = asm_ni;
        asmtab[type_ListPointer | (k << 3)] = asm_list_pointer;
        asmtab[type_OddFixnum | (k << 3)] = asm_immediate;
        asmtab[type_InstancePointer | (k << 3)] = asm_instance_pointer;
        asmtab[type_OtherImmediate1 | (k << 3) ] = asm_ni;
        asmtab[type_OtherPointer | (k << 3)] = asm_other_pointer;
    }
    
    asmtab[type_Ratio] = asm_boxed;
    asmtab[type_SingleFloat] = asm_single_float;
    asmtab[type_DoubleFloat] = asm_double_float;
    asmtab[type_Complex] = asm_boxed;
    asmtab[type_SimpleArray] = asm_boxed;
    asmtab[type_SimpleString] = asm_simple_string;
    asmtab[type_SimpleVector] = asm_simple_vector;
    asmtab[type_SimpleArrayUnsignedByte8] = asm_vector_unsigned_byte_8;
    asmtab[type_SimpleArrayUnsignedByte32] = asm_vector_unsigned_byte_32;
    asmtab[type_ComplexString] = asm_boxed;
    asmtab[type_ComplexVector] = asm_boxed;
    asmtab[type_CodeHeader] = asm_code_header;
    asmtab[type_ClosureHeader] = asm_closure_header;
    asmtab[type_FuncallableInstanceHeader] = asm_closure_header;
    /* Just use asm_boxed or have a special version for a value cell? */
    asmtab[type_ValueCellHeader] = asm_boxed;
    asmtab[type_SymbolHeader] = asm_boxed;
    asmtab[type_BaseChar] = asm_immediate;
    asmtab[type_InstanceHeader] = asm_boxed;
    asmtab[type_Fdefn] = asm_fdefn;
}
    
void
write_asm_object(const char *dir, int id, os_vm_address_t start, os_vm_address_t end)
{
    char asm_file[PATH_MAX];
    FILE* f;
    
    snprintf(asm_file, PATH_MAX, "%s/space-%d.s", dir, id);
    f = fopen(asm_file, "w");

    lispobj* ptr = (lispobj*) start;
    lispobj* end_ptr = (lispobj*) end;
    
    /*
     * If the id is the static space, we need special handling for
     * beginning which has NIL in a funny way to make NIL a symbol and
     * list.
     */
    if (id == STATIC_SPACE_ID) {
        int k;
        
        /* Output the first word */
        asm_header_word(ptr, *ptr, f, "");
        /* Header word for NIL */
        asm_header_word(ptr + 1, ptr[1], f, "NIL header");
        /* Label for NIL */
        asm_label(ptr + 2, ptr[2], f);
        ptr += 2;

        /* The 5 other words for the NIL symbol */
        for (k = 0; k < 5; ++k) {
            asm_lispobj(ptr, *ptr, f);
            ++ptr;
        }
        /* Bump pointer one more time to get double-word alignment */
        ++ptr;
        asm_align(f);
    }
    
    while (ptr < end_ptr) {
        lispobj object = *ptr;
        int object_length;
        
        object_length = asmtab[TypeOf(object)](ptr, object, f);
        ptr += object_length;
    }

    fclose(f);
}

