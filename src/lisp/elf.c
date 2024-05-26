/*

 This code was written by Fred Gilham, and has been placed in the public
 domain.  It is provided "as-is" and without warranty of any kind.

 Modified to add elf write code for writing out executables using
 (save-lisp).  FMG 27-Aug-2003

 Above changes put into main CVS branch. 05-Jul-2007.

 $Id: elf.c,v 1.32 2010/12/23 03:20:27 rtoy Exp $
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
#include "elf.h"
#include "validate.h"

static char elf_magic_string[] = {ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3};

/* Elf data structures. */
static Elf_Ehdr eh;
static Elf_Shdr sh;

/* Names of the Lisp image ELF sections. These names must be the same as
   the corresponding names found in the linker script.  */

static char *section_names[] = {"CORDYN", "CORSTA", "CORRO"};

/*
 * Starting address of the three ELF sections/spaces.  These must be
 * in the same order as section_names above!
 *
 * XXX: Should we combine these into a structure?  Should we do this
 * for all architectures?
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
elseek(int d, off_t o, const char *func)
{
    if (lseek(d, o, SEEK_SET) == -1) {
	perror(func);
	exit(-1);
    }
}


static int
create_elf_file (const char *dir, int id)
{
    char outfilename[FILENAME_MAX + 1];
    int out;

    /* Note: the space id will be either 1, 2 or 3.  Subtract one to index
       the name array. */
    snprintf(outfilename, FILENAME_MAX, "%s/%s.o", dir, section_names[id - 1]);
    out = open(outfilename, O_WRONLY | O_CREAT | O_TRUNC, 0666);

    if(!out) {
	perror("write_elf_object: can't open file");
	fprintf(stderr, "%s\n", outfilename);
    }

    return out;
}


static int
write_elf_header(int fd)
{
    extern Elf_Ehdr eh;

    /* Ident array. */
    eh.e_ident[EI_MAG0]		= ELFMAG0;
    eh.e_ident[EI_MAG1]		= ELFMAG1;
    eh.e_ident[EI_MAG2]		= ELFMAG2;
    eh.e_ident[EI_MAG3]		= ELFMAG3;

    eh.e_ident[EI_CLASS]	= ELFCLASS32;
#if defined(sparc) && defined(SOLARIS)
    eh.e_ident[EI_DATA]		= ELFDATA2MSB;
#else
    eh.e_ident[EI_DATA]		= ELFDATA2LSB;
#endif
    eh.e_ident[EI_VERSION]	= EV_CURRENT;
#ifdef SOLARIS
    eh.e_ident[EI_OSABI]	= ELFOSABI_SOLARIS;
#elif defined(__FREEBSD__)
    eh.e_ident[EI_OSABI]	= ELFOSABI_FREEBSD;
#elif defined(__NetBSD__)
    eh.e_ident[EI_OSABI]	= ELFOSABI_NETBSD;
#elif defined(__linux__)
    eh.e_ident[EI_OSABI]	= ELFOSABI_LINUX;
#else
    /* Default to NONE */
    eh.e_ident[EI_OSABI]	= ELFOSABI_NONE;
#endif

#ifdef SOLARIS
    eh.e_type		= ET_REL;	/* ???? */
#else
    eh.e_type		= ET_NONE;	/* ???? */
#endif
#if defined(sparc) && defined(SOLARIS)
    /*
     * We only support 32-bit code right now, and our binaries are
     * v8plus binaries.
     */
    eh.e_machine	= EM_SPARC32PLUS;
#else
    eh.e_machine	= EM_386;
#endif
    eh.e_version	= EV_CURRENT;
    eh.e_entry		= 0;
    eh.e_phoff		= 0;
    eh.e_shoff		= sizeof(Elf_Ehdr);
    eh.e_flags		= 0;
    eh.e_ehsize		= sizeof(Elf_Ehdr);
    eh.e_phentsize	= 0;
    eh.e_phnum		= 0;
    eh.e_shentsize	= sizeof(Elf_Shdr);
    eh.e_shnum		= 3;		/* Number of lisp spaces. */
    eh.e_shstrndx	= 2;

    return ewrite(fd, &eh, sizeof(Elf_Ehdr), __func__);
}


static int
write_zero_section_header(int fd)
{
    /* Section index 0. */
    sh.sh_name		= 0;
    sh.sh_type		= SHT_NULL;
    sh.sh_flags		= 0;
    sh.sh_addr		= 0;
    sh.sh_offset	= 0;
    sh.sh_size		= 0;
    sh.sh_link		= SHN_UNDEF;
    sh.sh_info		= 0;
    sh.sh_addralign	= 0;
    sh.sh_entsize	= 0;

    return ewrite(fd, &sh, eh.e_shentsize, __func__);
}


static char *string_table_name = ".shstrtab";
static char *object_name;

static int
write_object_section_header(int fd, long length, os_vm_address_t addr)
{
    extern Elf_Shdr sh;
    Elf_Word flags = SHF_ALLOC | SHF_EXECINSTR | SHF_WRITE;

    sh.sh_name		= strlen(string_table_name) + 2;
    sh.sh_type		= SHT_PROGBITS;
    sh.sh_flags		= flags;
    sh.sh_addr		= (Elf_Addr)addr;

    /* The object section offset is figured out as follows: elf-header +
       three section headers + string table.  The string table is the
       initial null byte followed by the name strings for the string
       table section and the object section. */
    sh.sh_offset	= sizeof(Elf_Ehdr) + 3 * sizeof(Elf_Shdr) + 1 +
	(strlen(string_table_name) + 1) + (strlen(object_name) + 1);
    sh.sh_size		= length;
    sh.sh_link		= SHN_UNDEF;
    sh.sh_info		= 0;
    sh.sh_addralign	= os_vm_page_size;	/* Must be page aligned. */
    sh.sh_entsize	= 0;

    return ewrite(fd, &sh, eh.e_shentsize, __func__);
}


static int
write_string_section_header(int fd)
{
    /* Section index 0. */

    extern Elf_Shdr sh;

    sh.sh_name		= 1;
    sh.sh_type		= SHT_STRTAB;
    sh.sh_flags		= 0;
    sh.sh_addr		= 0;
    sh.sh_offset	= sizeof(Elf_Ehdr) + 3 * sizeof(Elf_Shdr);
    /* The size of this section is the lengths of the strings plus the
       nulls. */
    sh.sh_size		= strlen(string_table_name) + strlen(object_name) + 3;
    sh.sh_link		= SHN_UNDEF;
    sh.sh_info		= 0;
    sh.sh_addralign	= 0;
    sh.sh_entsize	= 0;

    return ewrite(fd, &sh, eh.e_shentsize, __func__);
}


static int
write_string_section(int fd)
{
    char *buffer = malloc(1 + strlen(string_table_name) + 1 +
			  strlen(object_name) + 1);
    int ret;

    if(buffer == NULL) {
	perror("Out of memory in write_string_section()");
	ret = -1;

    } else {

	*buffer = '\0';
	memcpy(buffer + 1, string_table_name, strlen(string_table_name) + 1);
	memcpy(buffer + 1 + strlen(string_table_name) + 1,
	       object_name,
	       strlen(object_name) + 1);
	ret = ewrite(fd, buffer,
		     1 + strlen(string_table_name) + 1 + strlen(object_name) + 1,
		     __func__);
	free(buffer);
    }

    return ret;
}


static int
write_object_section(int fd, long length, os_vm_address_t real_addr)
{
    return ewrite(fd, (void *)real_addr, length, __func__);
}


int
write_space_object(const char *dir, int id, os_vm_address_t start, os_vm_address_t end)
{
    int out = create_elf_file(dir, id);
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

    object_name = section_names[id];

    printf("\t %s: %d bytes...\n", names[id], (end - start));
    fflush(stdout);

    if ((write_elf_header(out) == -1) ||
	(write_zero_section_header(out) == -1) ||
	(write_object_section_header(out, length, start) == -1) ||
	(write_string_section_header(out) == -1) ||
	(write_string_section(out) == -1) ||
	(write_object_section(out, length, start) == -1)) {
	fprintf(stderr, "Executable not built.\n");
	ret = -1;
    }

    close(out);
    return ret;
}

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
    if (paths == NULL) {
	perror("strdup");
	return -1;
    }
#else
    /*
     * What should we do here with 16-bit characters?  For now we just
     * take the low 8-bits.
     */
    paths = malloc(vec->length);
    if (paths == NULL) {
	perror("malloc");
	return -1;
    } else {
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
        printf("Searching for linker.sh script\n");
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
#if defined(__linux__) || defined(__FreeBSD__) || defined(SOLARIS) || defined(__NetBSD__)
            sprintf(command_line, "%s %s 0x%lx '%s' 0x%lx 0x%lx 0x%lx", command,
                    C_COMPILER, init_func_address, file,
                    (unsigned long) READ_ONLY_SPACE_START,
                    (unsigned long) STATIC_SPACE_START,
                    (unsigned long) DYNAMIC_0_SPACE_START);
#else
            extern int main();
	    sprintf(command_line, "%s %s 0x%lx 0x%lx %s", command, C_COMPILER,
                    init_func_address, (unsigned long) &main, file);
#endif
	    ret = system(command_line);
	    if(ret == -1) {
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


/* Read the ELF header from a file descriptor and stuff it into a
	 structure.	 Make sure it is really an elf header etc. */
static void
read_elf_header(int fd, Elf_Ehdr *ehp)
{
    eread(fd, ehp, sizeof(Elf_Ehdr), __func__);

    if (strncmp((const char *) ehp->e_ident, elf_magic_string, 4)) {
	fprintf(stderr,
		"Bad ELF magic number --- not an elf file.	Exiting in %s.\n",
		__func__);
	exit(-1);
    }
}


static void
read_section_header_entry(int fd, Elf_Shdr *shp)
{
    eread(fd, shp, eh.e_shentsize, __func__);
}


/*
  Map the built-in lisp core sections.

  NOTE!  We need to do this without using malloc because the memory layout
  is not set until some time after this is done.
*/
void
map_core_sections(const char *exec_name)
{
    int exec_fd;
    Elf_Shdr sh;		/* A section header entry. */
    Elf_Shdr strsecent;
    char nambuf[10];
    int soff;
    int strsecoff;		/* File offset to string table section. */
    int sections_remaining = 3;
    int i, j;
    extern int
	image_dynamic_space_size,
	image_static_space_size,
	image_read_only_space_size;

    if ((exec_fd = open(exec_name, O_RDONLY)) == -1) {
	perror("Can't open executable!");
	exit(-1);
    }

    read_elf_header(exec_fd, &eh);

    /* Find the section name string section.	Save its file offset. */
    soff = eh.e_shoff + eh.e_shstrndx * eh.e_shentsize;
    elseek(exec_fd, soff, __func__);
    read_section_header_entry(exec_fd, &strsecent);
    strsecoff = strsecent.sh_offset;

    for (i = 0; i < eh.e_shnum && sections_remaining > 0; i++) {

	/* Read an entry from the section header table. */
	elseek(exec_fd, eh.e_shoff + i * eh.e_shentsize, __func__);
	read_section_header_entry(exec_fd, &sh);

	/* Read the name from the string section. */
	elseek(exec_fd, strsecoff + sh.sh_name, __func__);
	eread(exec_fd, nambuf, 6, __func__);

	if (sh.sh_type == SHT_PROGBITS) {
	    /* See if this section is one of the lisp core sections. */
	    for (j = 0; j < 3; j++) {
		if (!strncmp(nambuf, section_names[j], 6)) {
		    os_vm_address_t addr;
		    /*
                     * Found a core section. Map it!
                     *
                     * Although the segment may contain the correct
                     * address for the start of the segment, we don't
                     * care.  We infer the address from the segment
                     * name.  (The names better be unique!!!!)  This
                     * approach allows for a possibly simpler linking
                     * operation because we don't have to figure out
                     * how to get the linker to give segments the
                     * correct address.
		     */
		    addr = section_addr[j];
		    if ((os_vm_address_t) os_map(exec_fd, sh.sh_offset,
						 addr, sh.sh_size)
			== (os_vm_address_t) -1) {
			fprintf(stderr, "%s: Can't map section %s\n", __func__, section_names[j]);
			exit(-1);
		    }
		    switch(j) {
                      case 0: /* Dynamic space. */
                          /* Dynamic space variables are set in lisp.c. */
                          image_dynamic_space_size = sh.sh_size;
                          break;
                      case 1: /* Static space. */
                          image_static_space_size = sh.sh_size;
                          break;
                      case 2: /* Read-only space. */
                          image_read_only_space_size = sh.sh_size;
                          break;
                      default:
                          /* Should never get here. */
                          abort();
                          break;
		    }

		    sections_remaining--;
		    /* Found a core section, don't check the other core section names. */
		    break;
		}
	    }
	}
    }

    close(exec_fd);

    if (sections_remaining != 0) {
	fprintf(stderr, "Couldn't map all core sections!	Exiting!\n");
	exit(-1);
    }
}
