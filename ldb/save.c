#include <stdio.h>
#include <signal.h>
#include <sys/file.h>

#include "ldb.h"
#include "os.h"
#include "lisp.h"
#include "core.h"
#include "globals.h"
#include "lispregs.h"
#include "interrupt.h"

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
    int words, bytes, data;
    static char *names[] = {NULL, "Dynamic", "Static", "Read-Only"};

    putw(id, save_file);
    words = end - addr;
    putw(words, save_file);

    bytes = words * sizeof(lispobj);

    printf("Writing %d bytes from the %s space at 0x%08x.\n",
           bytes, names[id], addr);

    data = write_bytes((char *)addr, bytes);

    putw(data, save_file);
    putw((os_vm_address_t)((long)addr / CORE_PAGESIZE), save_file);
    putw((bytes + CORE_PAGESIZE - 1) / CORE_PAGESIZE, save_file);
}

static long write_stack(name, start, end)
char *name, *start, *end;
{
    long len;

    start = (char *)os_trunc_to_page((os_vm_address_t)start);
    end = (char *)os_round_up_to_page((os_vm_address_t)end);

    len = end-start;

    printf("Writing %d bytes from the %s stack at 0x%08x.\n", len, name, start);

    return write_bytes(start, len);
}

static void output_machine_state(context)
struct sigcontext *context;
{
    struct machine_state ms;

    putw(CORE_MACHINE_STATE, save_file);
    putw(2 + sizeof(ms)/sizeof(long), save_file);

    ms.csp = current_control_stack_pointer;
    ms.fp = current_control_frame_pointer;
#ifndef ibmrt
    ms.bsp = current_binding_stack_pointer;
#endif
    ms.number_stack_start = number_stack_start;
    ms.sigcontext_page = write_bytes((char *)context, sizeof(struct sigcontext));
    ms.control_stack_page = write_stack("Control",
       (char *)control_stack,
       (char *)current_control_stack_pointer);
#ifndef ibmrt
    ms.binding_stack_page = write_stack("Binding",
       (char *)binding_stack,
       (char *)current_binding_stack_pointer);
#else
    ms.binding_stack_page = write_stack("Binding",
       (char *)binding_stack,
       (char *)SymbolValue(BINDING_STACK_POINTER));
#endif
    ms.number_stack_page = write_stack("Number",
       (char *)(context->sc_regs[NSP]),
       number_stack_start);

    fwrite(&ms, sizeof(ms), 1, save_file);
}

static SIGHDLRTYPE save_handler(signal, code, context)
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
#ifndef ibmrt
    output_space(DYNAMIC_SPACE_ID, current_dynamic_space, current_dynamic_space_free_pointer);
#else
    output_space(DYNAMIC_SPACE_ID, current_dynamic_space, (lispobj *)SymbolValue(ALLOCATION_POINTER));
#endif


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

    oldmask = sigblock(sigmask(SIGTERM));

    /* Install the save handler */
    newsv.sv_handler = save_handler;
    newsv.sv_mask = 0;
    newsv.sv_flags = SV_ONSTACK;
    sigvec(SIGTERM, &newsv, &oldsv);
    
    /* Set up the signal stack. */
    newss.ss_sp = (char *)(((unsigned)signal_stack + STACK_SIZE) & ~7);
    newss.ss_onstack = 0;
    sigstack(&newss, &oldss);

    /* Deliver the signal and wait for it to be delivered. */
    kill(getpid(), SIGTERM);

    sigpause(oldmask);

    /* Restore the state of the world. */
    sigvec(SIGTERM, &oldsv, NULL);
    sigstack(&oldss, NULL);
    sigsetmask(oldmask);

    return return_value;
}


/* State that must be preserved between load and restore. */

static struct sigcontext context;
static os_vm_address_t number_stack;


static os_vm_address_t map_stack(fd, start, end, data_page, name)
int fd;
os_vm_address_t start, end;
long data_page;
char *name;
{
    os_vm_size_t len;

    start = os_trunc_to_page(start);
    len = os_round_up_to_page(end) - start;

    if (len != 0) {
        if (start == NULL) {
            start = os_validate(start, len);
	    if(start==NULL)
                fprintf(stderr, "Could not allocate stageing area for the %s stack.\n", name);
        }

#ifdef PRINTNOISE
        printf("Mapping %d bytes onto the %s stack at 0x%08x.\n", len, name, start);
#endif
        start=os_map(fd, (1+data_page)*CORE_PAGESIZE, start, len);
    }

    return start;
}

extern void load(fd, ms)
int fd;
struct machine_state *ms;
{
    current_control_stack_pointer = ms->csp;
    current_control_frame_pointer = ms->fp;
#ifndef ibmrt
    current_binding_stack_pointer = ms->bsp;
#endif

    if (ms->number_stack_start > number_stack_start) {
        fprintf(stderr, "Can't allocate number stack --- probably this is a Sparc 10.\n");
        fprintf(stderr, "If not, your environment is real big...\n");
        exit(1);
    }
    else
        number_stack_start = ms->number_stack_start;

    lseek(fd, CORE_PAGESIZE * (1+ms->sigcontext_page), L_SET);
    read(fd, &context, sizeof(context));

    map_stack(fd,
              (os_vm_address_t)control_stack,
              (os_vm_address_t)current_control_stack_pointer,
              ms->control_stack_page,
              "Control");

    map_stack(fd,
              (os_vm_address_t)binding_stack,
#ifndef ibmrt
              (os_vm_address_t)current_binding_stack_pointer,
#else
	      (os_vm_address_t)SymbolValue(BINDING_STACK_POINTER),
#endif
              ms->binding_stack_page,
              "Binding");

    number_stack = map_stack(fd,
                             (os_vm_address_t)NULL,
                             os_round_up_to_page((os_vm_address_t)number_stack_start) - (os_vm_address_t)context.sc_regs[NSP],
                             ms->number_stack_page,
                             "Number");
}

static SIGHDLRTYPE restore_handler(signal, code, old_context)
int signal, code;
struct sigcontext *old_context;
{
    os_vm_size_t len;
    os_vm_address_t nsp;

    /* We have to move the number stack into place. */
    nsp = os_trunc_to_page((os_vm_address_t)context.sc_regs[NSP]);
    len = (os_vm_address_t)number_stack_start - nsp;
#ifdef PRINTNOISE
    printf("Moving the number stack from 0x%08x to 0x%08x.\n", number_stack, nsp);
#endif
    bcopy(number_stack, nsp, len);

    os_invalidate(number_stack, os_round_up_to_page(len));

#ifndef ibmrt
    sigreturn(&context);

    /* We should not get here. */
#else
    bcopy(&context, old_context, sizeof(context));
#endif
}


extern void restore()
{
    struct sigvec newsv;
    struct sigstack newss;
    static char signal_stack[STACK_SIZE];

    /* Set the return value. */
    return_value = 0;

    /* Block the signal. */
    oldmask = sigblock(sigmask(SIGTERM));

    /* Install the handler */
    newsv.sv_handler = restore_handler;
    newsv.sv_mask = 0;
    newsv.sv_flags = SV_ONSTACK;
    sigvec(SIGTERM, &newsv, &oldsv);
    
    /* Set up the signal stack. */
    newss.ss_sp = (char *)(((unsigned)signal_stack + STACK_SIZE) & ~7);
    newss.ss_onstack = 0;
    sigstack(&newss, &oldss);

    /* Deliver the signal and wait for it to be delivered. */
    kill(getpid(), SIGTERM);
    sigpause(oldmask);

    /* We should never get here. */
    sigvec(SIGTERM, &oldsv, NULL);
    sigstack(&oldss, NULL);
    sigsetmask(oldmask);

    fprintf(stderr, "Hm, tried to restore, but nothing happened.\n");
}
