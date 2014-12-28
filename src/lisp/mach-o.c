/*
 *
 * This code was written by Raymond Toy as part of CMU Common Lisp and
 * has been placed in the public domain.
 *
 * Mach-O support for generating executable images on Mac OS X (aka
 * Darwin).  This only knows enough of the Mach-O format to be able to
 * write out very simple object files for the Lisp spaces and to read
 * just enough of a Mach-O executable to find the segments containing
 * the Lisp spaces.
 *
 * For details of the file format see " Mac OS X ABI Mach-O File
 * Format Reference",
 * http://developer.apple.com/mac/library/documentation/DeveloperTools/Conceptual/MachORuntime/Reference/reference.html
 *
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "os.h"
#include "core.h"
#include "internals.h"
#include "globals.h"
#include "validate.h"

#include "elf.h"

typedef struct mach_header MachO_hdr;

/* Uncomment to enable debugging prints */
/* #define DEBUG_MACH_O */

/*
 * Names of the Lisp image sections. These names must be the same as
 * the corresponding names found in the linker script.
 */

static char *section_names[] = {"CORDYN", "CORSTA", "CORRO"};

/*
 * Starting addresses of the various spaces.  Must be in the same
 * order as section_names
 */
static os_vm_address_t section_addr[] =
{
    (os_vm_address_t) DYNAMIC_0_SPACE_START,
    (os_vm_address_t) STATIC_SPACE_START,
    (os_vm_address_t) READ_ONLY_SPACE_START
};

/* Note: write errors are not fatal. */
static int
ewrite(int fd, const void *buf, size_t nbytes, const char *func)
{
    if (write(fd, buf, nbytes) < nbytes) {
	perror(func);
	return -1;	/* Simple way to indicate error. */
    }
    return 0;
}

/*
  Read errors are fatal, because these reads have to succeed for lisp to
  get off the ground.
 */
static void
eread(int d, void *buf, size_t nbytes, const char *func)
{
    int res = read(d, buf, nbytes);

    if (res == -1) {
	perror(func);
	exit(-1);
    }

    if (res < nbytes) {
	fprintf(stderr, "Short read in %s!\n", func);
	exit(-1);
    }
}

static void
elseek(int d, off_t o, int whence, const char *func)
{
    if (lseek(d, o, whence) == -1) {
	perror(func);
	exit(-1);
    }
}

/*
 * Create a file for the specified Lisp space in the specified
 * directory.
 */
static int
create_mach_o_file (const char *dir, int id)
{
    char outfilename[FILENAME_MAX + 1];
    int out;

    /* Note: the space id will be either 1, 2 or 3.  Subtract one to index
       the name array. */
    snprintf(outfilename, FILENAME_MAX, "%s/%s.o", dir, section_names[id - 1]);
    out = open(outfilename, O_WRONLY | O_CREAT | O_TRUNC, 0666);

    if(!out) {
	perror("create_mach_o_file: can't open file");
	fprintf(stderr, "%s\n", outfilename);
    }

    return out;
}

/*
 * Write the Mach-O header.  We only handle what we need for our
 * purposes.
 */
static int
write_mach_o_header(int fd)
{
    MachO_hdr eh;

    /* Ident array. */
    eh.magic = MH_MAGIC;
#if defined(__ppc__)
    eh.cputype = CPU_TYPE_POWERPC;
    eh.cpusubtype = CPU_SUBTYPE_POWERPC_ALL;
#else
    eh.cputype = CPU_TYPE_I386;
    /*
     * Support any kind x86.  Should we be more specific?  We need at
     * least a pentium to run x87.  For SSE2 we need at least a
     * Pentium 4 chip.  So if the core is SSE2, should we set this to
     * Pentium 4?
     */
    eh.cpusubtype = CPU_SUBTYPE_I386_ALL;
#endif

    eh.filetype = MH_OBJECT;

    /* We only have 1 load command in our object */
    eh.ncmds = 1;
    /* Size of 1 segment command plus size of 1 section */
    eh.sizeofcmds = sizeof(struct segment_command) + sizeof(struct section);
    eh.flags = MH_NOUNDEFS | MH_NOMULTIDEFS;

    return ewrite(fd, &eh, sizeof(MachO_hdr), __func__);
}

/*
 * Write one segment (load) command to the object file in fd.  The
 * name of the segment is in name.  We also need to specify the
 * starting VM address (start) and the VM size (length) of the section
 * of memory that we want this to map to.
 */
static int
write_load_command(int fd, char* name, int length, os_vm_address_t start)
{
    struct segment_command lc;

    lc.cmd = LC_SEGMENT;
    /* Size is 1 segment command + 1 section command */
    lc.cmdsize = sizeof(lc) + sizeof(struct section);
    /*
     * Set the segment name.  This is very important because
     * map_core_sections looks for segment command whose segment name
     * matches a Lisp section name.
     */
    strncpy(lc.segname, name, sizeof(lc.segname));
    lc.vmaddr = (uint32_t) start;
    /*
     * The size is very important.  map_core_sections uses this to
     * determine how much to map.
     */
    lc.vmsize = length;
    /*
     * Offset where the data is.  It's the header, the segment
     * command, and one section.
     */
    lc.fileoff = lc.cmdsize + sizeof(struct mach_header);
    lc.filesize = length;
    lc.maxprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
    lc.initprot = lc.maxprot;
    /* There's only one section for this segment command. */
    lc.nsects = 1;
    lc.flags = 0;

    return ewrite(fd, &lc, sizeof(lc), __func__);
}

/*
 * Write the section info to the object file, fd.  Again, we need the
 * object name (object_name) which is used as both the section name
 * and the segment name.  The starting VM address (start) and the VM
 * length (length) is needed for the section.
 */
static int
write_section(int fd, int length, os_vm_address_t start, char* object_name)
{
    struct section sc;

    /*
     * sectname and segname are the same for our purposes.  However,
     * map_core_sections never looks here; it looks for the segment
     * name from the segment commands.
     */
    strncpy(sc.sectname, object_name, sizeof(sc.sectname));
    strncpy(sc.segname, object_name, sizeof(sc.segname));
    sc.addr = (uint32_t) start;
    sc.size = length;
    /*
     * Offset of the data.  We have one header, one segment and one
     * section
     */
    sc.offset = sizeof(struct mach_header) + sizeof(struct segment_command) +
        sizeof(struct section);
    sc.align = 12;              /* Align on 2^12 = 4096 boundary */
    sc.reloff = 0;
    sc.nreloc = 0;
    sc.flags = 0;
    sc.reserved1 = 0;
    sc.reserved2 = 0;

    return ewrite(fd, &sc, sizeof(sc), __func__);
}

/*
 * Write the actual data for the specific Lisp space to the object
 * file.  The data is read from memory starting at real_addr and
 * consists of length bytes.
 */

static int
write_section_data(int fd, long length, os_vm_address_t real_addr)
{
    return ewrite(fd, (void *)real_addr, length, __func__);
}

/*
 * Write out an object file containing the data for our Lisp space.
 * The object file is written to the directory dir.  The selected
 * space is specified by id, and the starting address of the space is
 * start, and goes to the address end.
 */
int
write_space_object(const char *dir, int id, os_vm_address_t start, os_vm_address_t end)
{
    int out = create_mach_o_file(dir, id);
    int ret = 0;
    /* The length should be a multiple of the page size. */
    size_t length = end - start + (os_vm_page_size -
				   ((end - start) % os_vm_page_size));
    static char *names[] = { "Dynamic", "Static", "Read-Only" };

    if (!(1 <= id) && (id <= 3)) {
	fprintf(stderr, "Invalid space id in %s: %d\n", __func__, id);
	fprintf(stderr, "Executable not built.\n");
	ret = -1;
    }

    /* Make id be 0-based to match array. */
    id--;

    printf("\t %s: %d bytes...\n", names[id], (end - start));
    fflush(stdout);

    if ((write_mach_o_header(out) == -1)
        || (write_load_command(out, section_names[id], length, start) == -1)
        || (write_section(out, length, start, section_names[id]) == -1)
        || (write_section_data(out, length, start) == -1)) {
	fprintf(stderr, "Executable not built.\n");
	ret = -1;
    }

    close(out);
    return ret;
}

/*
 * Link everything together to create the executable.
 */
int
obj_run_linker(long init_func_address, char *file)
{
    lispobj libstring = SymbolValue(CMUCL_LIB);     /* Get library: */
    struct vector *vec = (struct vector *)PTR(libstring);
    char *paths;
    char command[FILENAME_MAX + 1];
    char command_line[FILENAME_MAX + FILENAME_MAX + 10];
    char *strptr;
    struct stat st;
    int ret;
    extern int debug_lisp_search;
#ifndef UNICODE
    paths = strdup((char *)vec->data);
#else
    /*
     * What should we do here with 16-bit characters?  For now we just
     * take the low 8-bits.
     */
    paths = malloc(vec->length);
    {
        int k;
        unsigned short *data;
        data = (unsigned short*) vec->data;
        
        for (k = 0; k < vec->length; ++k) {
            paths[k] = data[k] & 0xff;
        }
    }
#endif
    strptr = strtok(paths, ":");

    if (debug_lisp_search) {
        printf("Searching for %s script\n", LINKER_SCRIPT);
    }

    while(strptr != NULL) {
        
	sprintf(command, "%s/%s", strptr, LINKER_SCRIPT);

        if (debug_lisp_search) {
            printf("  %s\n", command);
        }
        
	if (stat(command, &st) == 0) {
	    free(paths);
	    printf("\t[%s: linking %s... \n", command, file);
	    fflush(stdout);
            sprintf(command_line, "%s %s 0x%lx '%s' 0x%lx 0x%lx 0x%lx", command,
                    C_COMPILER, init_func_address, file,
                    (unsigned long) READ_ONLY_SPACE_START,
                    (unsigned long) STATIC_SPACE_START,
                    (unsigned long) DYNAMIC_0_SPACE_START);
	    ret = system(command_line);
	    if (ret == -1) {
		perror("Can't run link script");
	    } else {
		printf("\tdone]\n");
		fflush(stdout);
	    }
	    return ret;
	}
	strptr = strtok(NULL, ":");
    }

    fprintf(stderr,
	    "Can't find %s script in CMUCL library directory list.\n", LINKER_SCRIPT);
    free(paths);
    return -1;
}


/*
 * Read the Mach-O header from a file descriptor and stuff it into a
 * structure.  Make sure it is really a Mach-O header etc.
 */
static void
read_mach_o_header(int fd, MachO_hdr *ehp)
{
    eread(fd, ehp, sizeof(MachO_hdr), __func__);

    if (ehp->magic != MH_MAGIC) {
	fprintf(stderr,
		"Bad Mach-O magic number --- not a Mach-O file. Exiting in %s.\n",
		__func__);
	exit(-1);
    }
}


/*
 * Map the built-in lisp core sections.
 *
 * NOTE!  We need to do this without using malloc because the memory
 * layout is not set until some time after this is done.
 */
void
map_core_sections(const char *exec_name)
{
    MachO_hdr eh;
    int exec_fd;
    int sections_remaining = 3;
    int i;
    int j;
    extern int image_dynamic_space_size;
    extern int image_static_space_size;
    extern int image_read_only_space_size;

    if (!(exec_fd = open(exec_name, O_RDONLY))) {
	perror("Can't open executable!");
	exit(-1);
    }

    read_mach_o_header(exec_fd, &eh);

    for (i = 0; i < eh.ncmds && sections_remaining > 0; i++) {
        struct load_command lc;
        struct segment_command sc;
        
        /*
         * Read the load command to see what kind of command it is and
         * how big it is.
         */
        
        eread(exec_fd, &lc, sizeof(lc), __func__);
#ifdef DEBUG_MACH_O
        fprintf(stderr, "Load %d:  cmd = %d, cmdsize = %d\n", i, lc.cmd, lc.cmdsize);
#endif

        if (lc.cmd == LC_SEGMENT) {
          /*
           * Got a segment command, so read the rest of the command so
           * we can see if it's the segment for one of our Lisp
           * spaces.
           */
#ifdef DEBUG_MACH_O
            fprintf(stderr, "Reading next %ld bytes for SEGMENT\n", sizeof(sc) - sizeof(lc));
#endif

            /*
             * This will read in the rest of the slots of a
             * segment_command, starting from segname.  The initial
             * slots of the segment_command were read above for the
             * load_command. Alternatively, we could read in each slot
             * individually, but that's error prone, especially if a
             * new slot is ever added.
             */
            eread(exec_fd, &sc.segname, sizeof(sc) - sizeof(lc), __func__);

#ifdef DEBUG_MACH_O
            fprintf(stderr, "LC_SEGMENT: name = %s\n", sc.segname);
#endif

            /* See if the segment name matches any of our section names */
            for (j = 0; j < 3; ++j) {
                if (strncmp(sc.segname, section_names[j], sizeof(sc.segname)) == 0) {
		    os_vm_address_t addr;

                    /* Found a core segment.  Map it! */
#ifdef DEBUG_MACH_O
                    fprintf(stderr, "Matched!\n");
                    fprintf(stderr, " Fileoff = %u\n", sc.fileoff);
                    fprintf(stderr, " vmaddr  = 0x%x\n", sc.vmaddr);
                    fprintf(stderr, " vmsize  = 0x%x\n", sc.vmsize);
#endif
                    /*
                     * We don't care what address the segment has.  We
                     * will map it where it wants to go.
                     */
                    
		    addr = section_addr[j];
                    
                    if ((os_vm_address_t) os_map(exec_fd, sc.fileoff,
                                                 (os_vm_address_t) addr,
                                                 sc.vmsize)
                        == (os_vm_address_t) -1) {
			fprintf(stderr, "%s: Can't map section %s\n", __func__, section_names[j]);
			exit(-1);
                    }
                    switch (j) {
                      case 0:
                          /* Dynamic space */
                          image_dynamic_space_size = sc.vmsize;
                          break;
                      case 1:
                          /* Static space */
                          image_static_space_size = sc.vmsize;
                          break;
                      case 2:
                          /* Read only */
                          image_read_only_space_size = sc.vmsize;
                          break;
                      default:
                          /* Shouldn't happen! */
                          abort();
                    }
                    --sections_remaining;
                    break;
                }
            }
#ifdef DEBUG_MACH_O
            fprintf(stderr, "Skipping %ld remainder bytes left in command\n",
                    lc.cmdsize - sizeof(sc));
#endif
            elseek(exec_fd, lc.cmdsize - sizeof(sc), SEEK_CUR, __func__);
        } else {
            /* Not a segment command, so seek to the next command */
#ifdef DEBUG_MACH_O
            fprintf(stderr, "Seeking by %ld bytes\n", lc.cmdsize - sizeof(lc));
#endif
            elseek(exec_fd, lc.cmdsize - sizeof(lc), SEEK_CUR, __func__);
        }
    }

    close(exec_fd);

    if (sections_remaining != 0) {
	fprintf(stderr, "Couldn't map all core sections!	Exiting!\n");
	exit(-1);
    }
}
