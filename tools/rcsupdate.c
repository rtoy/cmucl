/* rcsupdate: utility to update a tree of RCS files.

$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/Attic/rcsupdate.c,v 1.2 1991/05/17 09:12:55 wlott Exp $

*/
#include <sys/types.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <stdio.h>
#include <strings.h>
#include <errno.h>

static int quiet = 0;

extern int errno;

#define MAXRCSFILES 1000

static char lastcache[MAXPATHLEN] = "";
static struct cache_entry {
    char *file;
    long modtime, rcstime;
} cache[MAXRCSFILES], *last_entry;
static int dirty = 0; 

void write_cache()
{
    FILE *file;
    char bak[MAXPATHLEN];
    struct cache_entry *entry;

    if (dirty) {
        strcpy(bak, lastcache);
        strcat(bak, ".BAK");
        rename(lastcache, bak);
        file = fopen(lastcache, "w");
        for (entry = cache; entry != last_entry; entry++) {
            fprintf(file, "%s %ld %ld\n", entry->file, entry->modtime,
                    entry->rcstime);
            free(entry->file);
        }
        fclose(file);
    }
    lastcache[0] = '\0';
    last_entry = cache;
    dirty = 0;
}

void read_cache(cachefile)
char *cachefile;
{
    FILE *file;
    char line[MAXPATHLEN+80], name[MAXPATHLEN];
    long modtime, rcstime;

    write_cache();

    if ((file = fopen(cachefile, "r")) != NULL) {
        while (fgets(line, sizeof(line), file) != NULL) {
            sscanf(line, "%s %ld %ld", name, &modtime, &rcstime);
            last_entry->file = (char *)malloc(strlen(name)+1);
            strcpy(last_entry->file, name);
            last_entry->modtime = modtime;
            last_entry->rcstime = rcstime;
            last_entry++;
        }
        fclose(file);
    }
    strcpy(lastcache, cachefile);
}

long last_ci_time(localfile, rcsfile)
char *localfile, *rcsfile;
{
    char cachefile[MAXPATHLEN], *ptr, *name, buf[MAXPATHLEN];
    FILE *stream;
    struct cache_entry *entry;
    int reload_entry;
    struct stat statbuf;
    long modtime;

    if (stat(rcsfile, &statbuf) < 0)
        return 0;

    strcpy(cachefile, localfile);
    ptr = rindex(cachefile, '/');
    if (ptr == NULL)
        ptr = cachefile;
    else
        ptr++;
    strcpy(ptr, ".rcsci_time_cache");

    if (strcmp(cachefile, lastcache) != 0)
        read_cache(cachefile);

    name = rindex(rcsfile, '/');
    if (name == NULL)
        name = rcsfile;
    else
        name++;

    reload_entry = 0;
    for (entry = cache; entry != last_entry; entry++)
        if (strcmp(name, entry->file) == 0)
            break;
    if (entry == last_entry) {
        reload_entry = 1;
        entry->file = (char *)malloc(strlen(name)+1);
        strcpy(entry->file, name);
        last_entry++;
    }
    else {
        if (entry->rcstime != statbuf.st_mtime)
            reload_entry = 1;
    }
    if (reload_entry) {
        fflush(stdout);

        sprintf(buf, "rcstime %s", rcsfile);
        stream = popen(buf, "r");
        if (stream == NULL) {
            perror("oops");
            return;
        }

        fgets(buf, sizeof(buf), stream);
        pclose(stream);

        modtime = atoi(buf);
        if (modtime == 0) {
            printf("bogus: ``%s''\n", buf);
            return 0;
        }

        entry->rcstime = statbuf.st_mtime;
        entry->modtime = modtime;
        dirty = 1;
    }

    return entry->modtime;
}

void update(localfile, rcsfile)
char *localfile, *rcsfile;
{
    struct stat statbuf;
    long localtime, rcstime;
    char buf[MAXPATHLEN+40];
    
    if (stat(localfile, &statbuf) < 0) {
        switch (errno) {
          case ENOENT:
	    if (!quiet)
		printf("local: <doesn't exist>\trcs: ");
            localtime = 0;
            break;

          default:
	    if (quiet)
		perror(localfile);
	    else
		perror("oops");
            return;
        }
    }
    else {
        localtime = statbuf.st_mtime;
	if (!quiet)
	    printf("local: %ld\trcs: ", localtime);
    }
    
    rcstime = last_ci_time(localfile, rcsfile);
    if (rcstime == 0) {
	if (quiet)
	    perror(rcsfile);
	else
	    perror("oops");
        return;
    }

    if (!quiet)
	printf("%ld\t", rcstime);

    if (localtime < rcstime) {
        if (!quiet)
	    printf("out of date.\n");
        sprintf(buf, "rcsco %s %s", localfile, rcsfile);
        system(buf);
    }
    else
	if (!quiet)
	    printf("up to date.\n");
    fflush(stdout);
}

void update_rcs_files(dirname)
     char *dirname;
{
    char localfile[MAXPATHLEN], rcsfile[MAXPATHLEN];
    char *localptr, *rcsptr;
    DIR *rcsdir;
    struct direct *entry;

    strcpy(rcsfile, dirname);
    strcat(rcsfile, "/RCS");

    rcsdir = opendir(rcsfile);
    if (rcsdir == NULL)
        return;

    rcsptr = rcsfile + strlen(rcsfile);
    *rcsptr++ = '/';

    strcpy(localfile, dirname);
    localptr = localfile + strlen(localfile);
    *localptr++ = '/';

    while ((entry = readdir(rcsdir)) != NULL) {
        if (strcmp(entry->d_name + entry->d_namlen - 2, ",v") == 0) {
            strcpy(rcsptr, entry->d_name);
            strcpy(localptr, entry->d_name);
            localptr[entry->d_namlen - 2] = '\0';
	    if (!quiet)
		printf("%s:\t", localfile);
            update(localfile, rcsfile);
        }
    }

    closedir(rcsdir);
}

void update_directory(dirname)
     char *dirname;
{
    DIR *dir;
    struct stat buf;
    struct direct *entry;
    char subdir[MAXPATHLEN], *subptr;

    printf("Updating %s:\n", dirname);

    update_rcs_files(dirname);

    dir = opendir(dirname);
    if (dir == NULL) {
        fprintf(stderr, "couldn't open ``%s''?\n", dirname);
        return;
    }

    strcpy(subdir, dirname);
    subptr = subdir + strlen(dirname);
    *subptr++ = '/';

    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, "RCS") != 0
            && strcmp(entry->d_name, "..") != 0
            && strcmp(entry->d_name, ".") != 0) {
            strcpy(subptr, entry->d_name);
            if (stat(subdir, &buf) != -1 && (buf.st_mode & S_IFMT) == S_IFDIR)
                update_directory(subdir);
        }
    }

    closedir(dir);
}

main(argc, argv)
int argc;
char *argv[];
{
    char localfile[MAXPATHLEN], rcsfile[MAXPATHLEN], *ptr;
    int len;
    struct stat buf;
    int did_anything = 0;

    while (*++argv != NULL) {
	if (strcmp(*argv, "-q") == 0)
	    quiet = 1;
	else if (stat(*argv, &buf)!=-1 && (buf.st_mode&S_IFMT)==S_IFDIR) {
	    update_directory(*argv);
	    did_anything = 1;
	}
	else {
	    len = strlen(*argv);

	    /* Determine the two names. */
	    strcpy(rcsfile, *argv);
	    strcpy(localfile, *argv);
	    if (strcmp(",v", localfile + len-2) == 0) {
		localfile[len-2] = '\0';
		ptr = rindex(localfile, '/');
		if (ptr == NULL)
		    ptr = localfile;
		else
		    ptr -= 3;
		if (strncmp(ptr, "RCS/", 4) == 0)
		    strcpy(ptr, ptr+4);
	    } else {
		ptr = rindex(rcsfile, '/');
		if (ptr == NULL)
		    ptr = rcsfile;
		else
		    ptr++;
		strcpy(ptr, "RCS/");
		strcat(ptr, localfile + (ptr - rcsfile));
		strcat(ptr, ",v");
	    }
                
	    /* Display the file we are working on: */
	    if (!quiet) {
		ptr = rindex(localfile, '/');
		if (ptr == NULL)
		    ptr = localfile;
		printf("%s:\t", ptr);
		fflush(stdout);
	    }
                
	    /* Update it. */
	    update(localfile, rcsfile);
	    did_anything = 1;
	}
    }
    if (!did_anything)
	update_directory(".");

    write_cache();

    exit(0);
}
