/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/coreparse.c,v 1.1 1990/02/24 19:37:14 wlott Exp $ */
#include <stdio.h>
#include <mach.h>
#include <sys/types.h>
#include <sys/file.h>

extern int version;

#define CORE_PAGESIZE (4*1024)
#define CORE_MAGIC (('C' << 24) | ('O' << 16) | ('R' << 8) | 'E')
#define CORE_END 3840
#define CORE_DIRECTORY 3841
#define CORE_VALIDATE 3845
#define CORE_VERSION 3860

static void process_validate(ptr, count)
long *ptr;
{
    long addr, len;

    while (count-- > 0) {
        addr = *ptr++ * CORE_PAGESIZE;
        len = *ptr++ * CORE_PAGESIZE;
        printf("validating %d bytes at 0x%x\n", len, addr);
        os_validate(addr, len);
    }
}

static void process_directory(fd, ptr, count)
long *ptr;
int count;
{
    long offset, addr, len;

    while (count-- > 0) {
        offset = CORE_PAGESIZE * (1 + *ptr++);
        addr = CORE_PAGESIZE * *ptr++;
        len = CORE_PAGESIZE * *ptr++;

        if (len != 0) {
            printf("mapping %d bytes at 0x%x\n", len, addr);
            os_map(fd, offset, addr, len);
        }
    }
}

void load_core_file(file)
char *file;
{
    int fd = open(file, O_RDONLY), count;
    long header[CORE_PAGESIZE / sizeof(long)], val, len, *ptr;

    if (fd < 0) {
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
        fprintf(stderr, "Invalid magic number: 0x%x should have been 0x%x\n", val, CORE_MAGIC);
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
                    fprintf(stderr, "*** Warning, ldb version (%d) different from core version (%d)\nYou will probably lose big.\n", version, *ptr);
                }
                break;

            case CORE_VALIDATE:
                process_validate(ptr, (len-2)/2);
                break;

            case CORE_DIRECTORY:
                process_directory(fd, ptr, (len-2)/3);
                break;

            default:
                printf("Unknown header entry: %d. Skipping.\n", val);
                break;
        }

        ptr += len - 2;
    }
}
