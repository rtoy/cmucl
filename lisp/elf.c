/*

 This code was written by Fred Gilham, and has been placed in the public
 domain.  It is provided "as-is" and without warranty of any kind.

 Modified to add elf write code for writing out executables using
 (save-lisp).  FMG 27-Aug-2003

 Above changes put into main CVS branch. 05-Jul-2007.

 $Id: elf.c,v 1.10 2007/07/19 00:12:06 fgilham Exp $
*/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "os.h"
#include "core.h"
#include "internals.h"
#include "globals.h"
#include "elf.h"

static char elf_magic_string[] = {ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3};

/* Elf data structures. */
static Elf_Ehdr eh;
static Elf_Shdr sh;

/* Names of the Lisp image ELF sections. These names must be the same as
   the corresponding names found in the linker script.  */

static char *section_names[] = {"CORDYN", "CORSTA", "CORRO"};

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
    char outfilename[MAXPATHLEN + 1];
    int out;

    /* Note: the space id will be either 1, 2 or 3.  Subtract one to index
       the name array. */
    snprintf(outfilename, MAXPATHLEN, "%s/%s.o", dir, section_names[id - 1]);
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
    eh.e_ident[EI_DATA]		= ELFDATA2LSB;
    eh.e_ident[EI_VERSION]	= EV_CURRENT;
    eh.e_ident[EI_OSABI]	= ELFOSABI_FREEBSD;

    eh.e_type		= ET_NONE;	/* ???? */
    eh.e_machine	= EM_386;	/*XXXXX what about sparc? */
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

    return ewrite(fd, &eh, sizeof(Elf_Ehdr), "write_elf_header");
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

    return ewrite(fd, &sh, eh.e_shentsize, "write_zero_section_header");
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

    return ewrite(fd, &sh, eh.e_shentsize, "write_object_section_header");
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

    return ewrite(fd, &sh, eh.e_shentsize, "write_string_section_header");
}


static int
write_string_section(int fd)
{
    char *buffer = malloc(1 + strlen(string_table_name) + 1 +
			  strlen(object_name) + 1);
    int ret;

    if(buffer == NULL) {

	write(0, "Out of memory in write_string_section()\n", 40);
	ret = -1;

    } else {

	*buffer = '\0';
	memcpy(buffer + 1, string_table_name, strlen(string_table_name) + 1);
	memcpy(buffer + 1 + strlen(string_table_name) + 1,
	       object_name,
	       strlen(object_name) + 1);
	ret = ewrite(fd, buffer,
		     1 + strlen(string_table_name) + 1 + strlen(object_name) + 1,
		     "write_string_section");
	free(buffer);
    }

    return ret;
}


static int
write_object_section(int fd, long length, os_vm_address_t real_addr)
{
    return ewrite(fd, (void *)real_addr, length, "write_object_section");
}


int
write_elf_object(const char *dir, int id, os_vm_address_t start, os_vm_address_t end)
{
    int out = create_elf_file(dir, id);
    int ret = 0;
    /* The length should be a multiple of the page size. */
    size_t length = end - start + (os_vm_page_size -
				   ((end - start) % os_vm_page_size));

    if(id < 1 || id > 3) {
	fprintf(stderr, "Invalid space id in %s: %d\n", "write_elf_object", id);
	fprintf(stderr, "Executable not built.\n");
	ret = -1;
    }

    /* Make id be 0-based to match array. */
    id--;

    object_name = section_names[id];

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

void
elf_cleanup(const char *dirname)
{
    char filename[MAXPATHLEN];
    int i;

    /* Get rid of lisp space files. */
    for(i = 0; i < 3; i++) {
	/* Delete core space .o files. */
	sprintf(filename, "%s/%s.o", dirname, section_names[i]);
	unlink(filename);
    }
}

int
elf_run_linker(long init_func_address, char *file)
{

    lispobj libstring = SymbolValue(CMUCL_LIB);     /* Get library: */
    struct vector *vec = (struct vector *)PTR(libstring);
    char *paths = strdup((char *)vec->data);
    char command[MAXPATHLEN + 1];
    char command_line[MAXPATHLEN + MAXPATHLEN + 10];
    char *strptr;
    struct stat st;
    int ret;

    strptr = strtok(paths, ":");

    while(strptr != NULL) {

	sprintf(command, "%s/%s", strptr, LINKER_SCRIPT);

	if(stat(command, &st) == 0) {
	    free(paths);
	    printf("\t[%s: linking %s... \n", command, file);
	    fflush(stdout);
	    sprintf(command_line, "%s 0x%lx %s", command, init_func_address, file);
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
	    "Can't find linkit command in CMUCL library directory list.\n");
    free(paths);
    return -1;
}


/* Read the ELF header from a file descriptor and stuff it into a
	 structure.	 Make sure it is really an elf header etc. */
static void
read_elf_header(int fd, Elf_Ehdr *ehp)
{
    eread(fd, ehp, sizeof(Elf_Ehdr), "read_elf_header");

    if (strncmp(ehp->e_ident, elf_magic_string, 4)) {
	fprintf(stderr,
		"Bad ELF magic number --- not an elf file.	Exiting in %s.\n",
		__FUNCTION__);
	exit(-1);
    }
}


static void
read_section_header_entry(int fd, Elf_Shdr *shp)
{
    eread(fd, shp, eh.e_shentsize, "read_section_header_entry");
}


/*
  Map the built-in lisp core sections.

  NOTE!  We need to do this without using malloc because the memory layout
  is not set until some time after this is done.
*/
void
map_core_sections(char *exec_name)
{
    int exec_fd;
    Elf_Shdr sh;		/* A section header entry. */
    Elf_Shdr strsecent;
    char nambuf[10];
    int soff;
    int strsecoff;		/* File offset to string table section. */
    int sections_remaining = 3;
    int i, j;
    lispobj *free_pointer;
    os_vm_address_t addr;
    extern int
	image_dynamic_space_size,
	image_static_space_size,
	image_read_only_space_size;

    if (!(exec_fd = open(exec_name, O_RDONLY))) {
	perror("Can't open executable!");
	exit(-1);
    }

    read_elf_header(exec_fd, &eh);

    /* Find the section name string section.	Save its file offset. */
    soff = eh.e_shoff + eh.e_shstrndx * eh.e_shentsize;
    elseek(exec_fd, soff, "map_core_sections");
    read_section_header_entry(exec_fd, &strsecent);
    strsecoff = strsecent.sh_offset;

    for (i = 0; i < eh.e_shnum && sections_remaining > 0; i++) {

	/* Read an entry from the section header table. */
	elseek(exec_fd, eh.e_shoff + i * eh.e_shentsize, "map_core_sections");
	read_section_header_entry(exec_fd, &sh);

	/* Read the name from the string section. */
	elseek(exec_fd, strsecoff + sh.sh_name, "map_core_sections");
	eread(exec_fd, nambuf, 6, "map_core_sections");

	if (sh.sh_type == SHT_PROGBITS) {
	    /* See if this section is one of the lisp core sections. */
	    for (j = 0; j < 3; j++) {
		if (!strncmp(nambuf, section_names[j], 6)) {
		    /* Found a core section. Map it! */
		  if ((addr = (os_vm_address_t) os_map(exec_fd, sh.sh_offset,
						       (os_vm_address_t) sh.sh_addr, sh.sh_size))
		      == (os_vm_address_t) -1) {
			fprintf(stderr, "Can't map section %s\n", section_names[j]);
			exit(-1);
		    }

		    free_pointer = (lispobj *) addr + sh.sh_size;

		    switch(j) {
		    case 0: /* Dynamic space. */
			current_dynamic_space = (lispobj *)addr;
#if defined(ibmrt) || defined(i386) || defined(__x86_64)
			SetSymbolValue(ALLOCATION_POINTER, (lispobj) free_pointer);
#else
			current_dynamic_space_free_pointer = free_pointer;
#endif
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

