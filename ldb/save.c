#include <stdio.h>
#include <signal.h>
#include <sys/file.h>
#include <mach.h>

#include "lisp.h"
#include "ldb.h"
#include "core.h"
#include "globals.h"
#include "lispregs.h"

#define STACK_SIZE (8*1024)

extern int version;

static FILE *save_file;

/* State that is restored when save returns, and therefore must be setup by either save or restore. */

static boolean return_value;
static int oldmask;
static struct sigvec oldsv;
static struct sigstack oldss;



static long write_bytes(addr, bytes)
char *addr;
long bytes;
{
    long count, here, data;

    fflush(save_file);
    here = ftell(save_file);
    fseek(save_file, 0, 2);
    data = (ftell(save_file)+CORE_PAGESIZE-1)&~(CORE_PAGESIZE-1);
    fseek(save_file, data, 0);

    while (bytes > 0) {
        count = fwrite(addr, 1, bytes, save_file);
        if (count > 0) {
            bytes -= count;
            addr += count;
        }
        else {
            perror("Error writing to save file");
            bytes = 0;
        }
    }
    fflush(save_file);
    fseek(save_file, here, 0);
    return data/CORE_PAGESIZE - 1;
}

static void output_space(id, addr, end)
int id;
lispobj *addr, *end;
{
    int words, data;
    static char *names[] = {NULL, "Dynamic", "Static", "Read-Only"};

    putw(id, save_file);
    words = end - addr;
    putw(words, save_file);

    printf("Writing %d words from the %s space at 0x%08x.\n", words, names[id], addr);

    data = write_bytes((char *)addr, words*sizeof(lispobj));

    putw(data, save_file);
    putw((vm_address_t)addr / CORE_PAGESIZE, save_file);
    putw((words*sizeof(lispobj) + CORE_PAGESIZE - 1) / CORE_PAGESIZE, save_file);
}

static long write_stack(name, start, end)
char *name, *start, *end;
{
    long data, len;

    start = (char *)trunc_page((vm_address_t)start);
    end = (char *)round_page((vm_address_t)end);

    len = end-start;

    printf("Writing %d bytes from the %s stack at 0x%08x.\n", len, name, start);

    return write_bytes(start, len);
}

static void output_machine_state(context)
struct sigcontext *context;
{
    int data, stacklen;
    char *stack;
    struct machine_state ms;

    putw(CORE_MACHINE_STATE, save_file);
    putw(2 + sizeof(ms)/sizeof(long), save_file);

    ms.csp = current_control_stack_pointer;
    ms.fp = current_control_frame_pointer;
    ms.bsp = current_binding_stack_pointer;
    ms.flags = current_flags_register;
    ms.sigcontext_page = write_bytes((char *)context, sizeof(struct sigcontext));
    ms.control_stack_page = write_stack("Control",
       (char *)control_stack,
       (char *)current_control_stack_pointer);
    ms.binding_stack_page = write_stack("Binding",
       (char *)binding_stack,
       (char *)current_binding_stack_pointer);
    ms.number_stack_page = write_stack("Number",
       (char *)(context->sc_regs[NSP]),
       (char *)0x80000000);

    fwrite(&ms, sizeof(ms), 1, save_file);
}

static save_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    if (context->sc_onstack) {
        printf("Cannot save while executing on a signal stack!\n");
        return;
    }

    putw(CORE_MAGIC, save_file);

    putw(CORE_VERSION, save_file);
    putw(3, save_file);
    putw(version, save_file);

    putw(CORE_NDIRECTORY, save_file);
    putw((5*3)+2, save_file);

    output_space(READ_ONLY_SPACE_ID, read_only_space, (lispobj *)SymbolValue(READ_ONLY_SPACE_FREE_POINTER));
    output_space(STATIC_SPACE_ID, static_space, (lispobj *)SymbolValue(STATIC_SPACE_FREE_POINTER));
    output_space(DYNAMIC_SPACE_ID, current_dynamic_space, current_dynamic_space_free_pointer);

    output_machine_state(context);

    putw(CORE_END, save_file);
    fclose(save_file);

    printf("done.]\n");
}

extern lispobj save(filename)
char *filename;
{
    int oldmask;
    struct sigvec oldsv, newsv;
    struct sigstack oldss, newss;
    static char signal_stack[STACK_SIZE];

    /* Set the return value. */
    return_value = TRUE;

    /* Open the file: */
    unlink(filename);
    save_file = fopen(filename, "w");
    if (save_file == NULL) {
        perror(filename);
        return;
    }
    printf("[Saving current lisp image into %s:\n", filename);

    oldmask = sigblock(sigmask(SIGUSR1));

    /* Install the save handler */
    newsv.sv_handler = save_handler;
    newsv.sv_mask = 0;
    newsv.sv_flags = SV_ONSTACK;
    sigvec(SIGUSR1, &newsv, &oldsv);
    
    /* Set up the signal stack. */
    newss.ss_sp = signal_stack + STACK_SIZE;
    newss.ss_onstack = 0;
    sigstack(&newss, &oldss);

    /* Deliver the signal and wait for it to be delivered. */
    kill(getpid(), SIGUSR1);
    sigpause(oldmask);

    /* Restore the state of the world. */
    sigvec(SIGUSR1, &oldsv, NULL);
    sigstack(&oldss, NULL);
    sigsetmask(oldmask);

    return return_value;
}


/* State that must be preserved between load and restore. */

static struct sigcontext context;
static vm_address_t number_stack;


static vm_address_t map_stack(fd, start, end, data_page, name)
int fd;
vm_address_t start, end;
long data_page;
char *name;
{
    vm_size_t len;
    kern_return_t res;

    start = trunc_page(start);
    len = round_page(end) - start;

    if (len != 0) {
        if (start == NULL) {
            res = vm_allocate(task_self(), &start, len, TRUE);
            if (res != KERN_SUCCESS) {
                fprintf(stderr, "Could not allocate stageing area for the %s stack: %s.\n", name, mach_error_string(res));
            }
        }

        printf("Mapping %d bytes onto the %s stack at 0x%08x.\n", len, name, start);
        os_map(fd, (1+data_page)*CORE_PAGESIZE, start, len);
    }

    return start;
}

extern void load(fd, ms)
int fd;
struct machine_state *ms;
{
    current_control_stack_pointer = ms->csp;
    current_control_frame_pointer = ms->fp;
    current_binding_stack_pointer = ms->bsp;
    current_flags_register = ms->flags;

    lseek(fd, CORE_PAGESIZE * (1+ms->sigcontext_page), L_SET);
    read(fd, &context, sizeof(context));

    map_stack(fd, (vm_address_t)control_stack, (vm_address_t)current_control_stack_pointer, ms->control_stack_page, "Control");
    map_stack(fd, (vm_address_t)binding_stack, (vm_address_t)current_binding_stack_pointer, ms->binding_stack_page, "Binding");
    number_stack = map_stack(fd, (vm_address_t)NULL, (vm_address_t)0x80000000 - (vm_address_t)context.sc_regs[NSP], ms->number_stack_page, "Number");
}

static restore_handler(signal, code, old_context)
int signal, code;
struct sigcontext *old_context;
{
    vm_size_t len;
    vm_address_t nsp;
    kern_return_t res;
    vm_machine_attribute_val_t flush;


    /* We have to move the number stack into place. */
    nsp = trunc_page((vm_address_t)context.sc_regs[NSP]);
    len = (vm_address_t)0x80000000 - nsp;
    printf("Moving the number stack from 0x%08x to 0x%08x.\n", number_stack, nsp);

#if 0
    res = vm_copy(task_self(), number_stack, len, nsp);

    if (res != KERN_SUCCESS) {
        mach_error("Could not move the number stack into place: ", res);
    }
#else
    bcopy(number_stack, nsp, len);
#endif

    vm_deallocate(task_self(), number_stack, len);

    /* Flush the old number stack from the cache. */
    flush = MATTR_VAL_CACHE_FLUSH;

    res = vm_machine_attribute(task_self(), nsp, len,
                                MATTR_CACHE, &flush);
    if (res != KERN_SUCCESS)
        mach_error("Could not flush the number stack from the cache: ", res);

    sigreturn(&context);

    /* We should not get here. */
}


extern void restore()
{
    struct sigvec newsv;
    struct sigstack newss;
    static char signal_stack[STACK_SIZE];

    /* Set the return value. */
    return_value = 0;

    /* Block the signal. */
    oldmask = sigblock(sigmask(SIGUSR1));

    /* Install the handler */
    newsv.sv_handler = restore_handler;
    newsv.sv_mask = 0;
    newsv.sv_flags = SV_ONSTACK;
    sigvec(SIGUSR1, &newsv, &oldsv);
    
    /* Set up the signal stack. */
    newss.ss_sp = signal_stack + STACK_SIZE;
    newss.ss_onstack = 0;
    sigstack(&newss, &oldss);

    /* Deliver the signal and wait for it to be delivered. */
    kill(getpid(), SIGUSR1);
    sigpause(oldmask);

    /* We should never get here. */
    sigvec(SIGUSR1, &oldsv, NULL);
    sigstack(&oldss, NULL);
    sigsetmask(oldmask);

    fprintf(stderr, "Hm, tried to restore, but nothing happened.\n");
}
