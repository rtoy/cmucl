
#include <stdio.h>
#include <signal.h>
#include <sys/file.h>

#include "lisp.h"
#include "os.h"
#include "internals.h"
#include "core.h"
#include "globals.h"
#include "lispregs.h"
#include "interrupt.h"
#include "save.h"
#include "validate.h"

extern int version;

extern void restore_state(char *stack_ptr, int result);
extern int save_state(int fn(char *stack_ptr, FILE *file), FILE *file);

static long write_bytes(FILE *file, char *addr, long bytes)
{
    long count, here, data;

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

static long write_stack(FILE *file, char *name, char *start, char *end)
{
    long len;

    start = (char *)os_trunc_to_page((os_vm_address_t)start);
    end = (char *)os_round_up_to_page((os_vm_address_t)end);

    len = end-start;

    printf("Writing %d bytes from the %s stack at 0x%08X.\n", len, name,
	   (unsigned long)start);

    return write_bytes(file, start, len);
}

static int do_save(char *stack_ptr, FILE *file)
{
    struct machine_state ms;

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

    putw(CORE_MACHINE_STATE, file);
    putw(2 + sizeof(ms)/sizeof(long), file);

    ms.csp = current_control_stack_pointer;
    ms.cfp = current_control_frame_pointer;
    ms.control_stack_page = write_stack(file, "Control",
       (char *)control_stack,
       (char *)current_control_stack_pointer);

#ifdef reg_BSP
    ms.bsp = current_binding_stack_pointer;
    ms.binding_stack_page = write_stack(file, "Binding",
       (char *)binding_stack,
       (char *)current_binding_stack_pointer);
#else
    ms.binding_stack_page = write_stack(file, "Binding",
       (char *)binding_stack,
       (char *)SymbolValue(BINDING_STACK_POINTER));
#endif

    ms.nsp = stack_ptr;
#ifdef NUMBER_STACK_GROWS_UP
    ms.number_stack_page = write_stack(file, "Number",
	(char *)NUMBER_STACK_START,
	stack_ptr);
#else
    ms.number_stack_page = write_stack(file, "Number", stack_ptr,
       (char *)(NUMBER_STACK_START+NUMBER_STACK_SIZE));
#endif

    fwrite((void *)&ms, sizeof(ms), 1, file);

    putw(CORE_END, file);
    fclose(file);

    printf("done.]\n");

    return 1;
}

int save(char *filename)
{
    FILE *file;

    /* Open the file: */
    unlink(filename);
    file = fopen(filename, "w");
    if (file == NULL) {
        perror(filename);
        return TRUE;
    }
    printf("[Saving current lisp image into %s:\n", filename);

    return save_state(do_save, file);
}

static os_vm_address_t map_stack(int fd, os_vm_address_t start,
				 os_vm_address_t end,
				 long data_page,
				 char *name)
{
    os_vm_size_t len;

    start = os_trunc_to_page(start);
    len = os_round_up_to_page(end) - start;

    if (len != 0) {
#ifdef PRINTNOISE
        printf("Mapping %d bytes onto the %s stack at 0x%08x.\n", len, name, start);
#endif
        start=os_map(fd, (1+data_page)*CORE_PAGESIZE, start, len);
    }

    return start;
}

static char *restored_nsp;

void load(int fd, struct machine_state *ms)
{
    current_control_stack_pointer = ms->csp;
    current_control_frame_pointer = ms->cfp;
#ifdef reg_BSP
    current_binding_stack_pointer = ms->bsp;
#endif

    map_stack(fd,
              (os_vm_address_t)control_stack,
              (os_vm_address_t)current_control_stack_pointer,
              ms->control_stack_page,
              "Control");

    map_stack(fd,
              (os_vm_address_t)binding_stack,
#ifdef reg_BSP
              (os_vm_address_t)current_binding_stack_pointer,
#else
	      (os_vm_address_t)SymbolValue(BINDING_STACK_POINTER),
#endif
              ms->binding_stack_page,
              "Binding");

    map_stack(fd,
#ifdef NUMBER_STACK_GROWS_UP
		NUMBER_STACK_START,
		(os_vm_address_t)ms->nsp,
#else
		(os_vm_address_t)ms->nsp,
		NUMBER_STACK_START+NUMBER_STACK_SIZE,
#endif
		ms->number_stack_page,
		"Number");

    restored_nsp = ms->nsp;
}

void restore(void)
{
    restore_state(restored_nsp, 0);

    fprintf(stderr, "Hm, tried to restore, but nothing happened.\n");
}
