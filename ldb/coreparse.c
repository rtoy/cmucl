/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/coreparse.c,v 1.4 1990/03/29 02:58:03 ch Exp $ */
#include <stdio.h>
#include <mach.h>
#include <sys/types.h>
#include <sys/file.h>
#include "lisp.h"
#include "globals.h"

extern int version;

#define CORE_PAGESIZE (4*1024)
#define CORE_MAGIC (('C' << 24) | ('O' << 16) | ('R' << 8) | 'E')
#define CORE_END 3840
#define CORE_NDIRECTORY 3861
#define CORE_VALIDATE 3845
#define CORE_VERSION 3860

#define DYNAMIC_SPACE_ID (1)
#define STATIC_SPACE_ID (2)
#define READ_ONLY_SPACE_ID (3)

struct ndir_entry {
	long identifier;
	long nwords;
	long data_page;
	long address;
	long page_count;
};

static void process_directory(fd, ptr, count)
int fd, count;
long *ptr;
{
	long id, offset, len;
	lispobj *free_pointer;
	vm_address_t addr;
	struct ndir_entry *entry;

	entry = (struct ndir_entry *) ptr;

	while (count-- > 0) {
		id = entry->identifier;
		offset = CORE_PAGESIZE * (1 + entry->data_page);
		addr = (vm_address_t) CORE_PAGESIZE * entry->address;
		free_pointer = (lispobj *) addr + entry->nwords;
		len = CORE_PAGESIZE * entry->page_count;

		if (len != 0) {
			printf("Mapping %d bytes at 0x%x.\n", len, addr);
			os_map(fd, offset, addr, len);
		}

#if 0
		printf("Space ID = %d, free pointer = 0x%08x.\n", id, free_pointer);
#endif

		switch (id) {
		case DYNAMIC_SPACE_ID:
			if (current_dynamic_space != (lispobj *) addr)
				printf("Strange ... dynamic space lossage.\n");
			current_dynamic_space_free_pointer = free_pointer;
			break;
		case STATIC_SPACE_ID:
			static_space = (lispobj *) addr;
			break;
		case READ_ONLY_SPACE_ID:
			/* Don't care about read only space */
			break;
		default:
			printf("Strange space ID: %d; ignored.\n", id);
			break;
		}
		entry++;
	}
}

void load_core_file(file)
char *file;
{
    int fd = open(file, O_RDONLY), count;
    long header[CORE_PAGESIZE / sizeof(long)], val, len, *ptr;

    if (fd < 0) {
	    fprintf(stderr, "Could not open file \"%s\".\n", file);
	    perror("open");
	    exit(1);
    }

    count = read(fd, header, CORE_PAGESIZE);
    if (count < 0) {
        perror("read");
        exit(1);
    }
    if (count < CORE_PAGESIZE) {
        fprintf(stderr, "Premature EOF.\n");
        exit(1);
    }   

    ptr = header;
    val = *ptr++;

    if (val != CORE_MAGIC) {
        fprintf(stderr, "Invalid magic number: 0x%x should have been 0x%x.\n",
		val, CORE_MAGIC); 
        exit(1);
    }

    while (val != CORE_END) {
        val = *ptr++;
        len = *ptr++;

        switch (val) {
            case CORE_END:
                break;

            case CORE_VERSION:
                if (*ptr != version) {
                    fprintf(stderr, "WARNING: ldb version (%d) different from core version (%d).\nYou may lose big.\n", version, *ptr);
                }
                break;

            case CORE_VALIDATE:
		fprintf(stderr, "Validation no longer supported; ignored.\n");
                break;

            case CORE_NDIRECTORY:
                process_directory(fd, ptr,
				  (len-2) / (sizeof(struct ndir_entry) / sizeof(long)));
                break;

            default:
                printf("Unknown core file entry: %d; skipping.\n", val);
                break;
        }

        ptr += len - 2;
    }
}
