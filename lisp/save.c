
#include <stdio.h>
#include <signal.h>
#include <sys/file.h>

#include "lisp.h"
#include "os.h"
#include "internals.h"
#include "core.h"
#include "globals.h"
#include "save.h"
#include "lispregs.h"
#include "validate.h"

extern int version;

static long write_bytes(FILE *file, char *addr, long bytes)
{
    long count, here, data;

    bytes = (bytes+CORE_PAGESIZE-1)&~(CORE_PAGESIZE-1);

    fflush(file);
    here = ftell(file);
    fseek(file, 0, 2);
    data = (ftell(file)+CORE_PAGESIZE-1)&~(CORE_PAGESIZE-1);
    fseek(file, data, 0);

    while (bytes > 0) {
        count = fwrite(addr, 1, bytes, file);
        if (count > 0) {
            bytes -= count;
            addr += count;
        }
        else {
            perror("Error writing to save file");
            bytes = 0;
        }
    }
    fflush(file);
    fseek(file, here, 0);
    return data/CORE_PAGESIZE - 1;
}

static void output_space(FILE *file, int id, lispobj *addr, lispobj *end)
{
    int words, bytes, data;
    static char *names[] = {NULL, "Dynamic", "Static", "Read-Only"};

    putw(id, file);
    words = end - addr;
    putw(words, file);

    bytes = words * sizeof(lispobj);

    printf("Writing %d bytes from the %s space at 0x%08X.\n",
           bytes, names[id], (unsigned long)addr);

    data = write_bytes(file, (char *)addr, bytes);

    putw(data, file);
    putw((os_vm_address_t)((long)addr / CORE_PAGESIZE), file);
    putw((bytes + CORE_PAGESIZE - 1) / CORE_PAGESIZE, file);
}

boolean save(char *filename, lispobj init_function)
{
    FILE *file;

    /* Open the file: */
    unlink(filename);
    file = fopen(filename, "w");
    if (file == NULL) {
        perror(filename);
        return TRUE;
    }
    printf("[Undoing binding stack... ");
    fflush(stdout);
    unbind_to_here((lispobj *)BINDING_STACK_START);
    SetSymbolValue(CURRENT_CATCH_BLOCK, 0);
    SetSymbolValue(CURRENT_UNWIND_PROTECT_BLOCK, 0);
    SetSymbolValue(EVAL_STACK_TOP, 0);
    printf("done]\n");

    printf("[Saving current lisp image into %s:\n", filename);

    putw(CORE_MAGIC, file);

    putw(CORE_VERSION, file);
    putw(3, file);
    putw(version, file);

    putw(CORE_NDIRECTORY, file);
    putw((5*3)+2, file);

    output_space(file, READ_ONLY_SPACE_ID, read_only_space,
		 (lispobj *)SymbolValue(READ_ONLY_SPACE_FREE_POINTER));
    output_space(file, STATIC_SPACE_ID, static_space,
		 (lispobj *)SymbolValue(STATIC_SPACE_FREE_POINTER));
#ifdef reg_ALLOC
    output_space(file, DYNAMIC_SPACE_ID, current_dynamic_space,
		 current_dynamic_space_free_pointer);
#else
    output_space(file, DYNAMIC_SPACE_ID, current_dynamic_space,
		 (lispobj *)SymbolValue(ALLOCATION_POINTER));
#endif

    putw(CORE_INITIAL_FUNCTION, file);
    putw(3, file);
    putw(init_function, file);

    putw(CORE_END, file);
    fclose(file);

    printf("done.]\n");

    exit(0);
}
