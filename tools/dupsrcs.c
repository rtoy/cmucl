#include <stdio.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <sys/param.h>

/* Duplicates a source tree. */

/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/Attic/dupsrcs.c,v 1.2 1991/11/07 22:55:36 wlott Exp $ */

void duptree(srcdir, dstdir)
char *srcdir, *dstdir;
{
    DIR *dir;
    struct direct *entry;
    char srcpath[MAXPATHLEN], dstpath[MAXPATHLEN];
    struct stat buf;

    printf("Duplicating %s\n  into %s\n", srcdir, dstdir);

    /* Make sure the dstdir is there. */
    if (mkdir(dstdir) == 0)
	printf("Creating %s\n", dstdir);

    dir = opendir(srcdir);
    if (dir == NULL) {
	perror(srcdir);
	return;
    }

    while ((entry = readdir(dir)) != NULL) {
	if (strncmp(entry->d_name, "RCS", 3) == 0)
	    continue;
	if (entry->d_name[0] == '.')
	    continue;
	sprintf(srcpath, "%s/%s", srcdir, entry->d_name);
	sprintf(dstpath, "%s/%s", dstdir, entry->d_name);
	if (stat(srcpath, &buf) < 0) {
	    perror(srcpath);
	    continue;
	}
	if ((buf.st_mode & S_IFMT) == S_IFDIR)
	    duptree(srcpath, dstpath);
	else
	    if (symlink(srcpath, dstpath) == 0)
		printf("Linked %s\n", dstpath);
	    else
		if (errno != EEXIST)
		    perror(dstpath);
    }

    closedir(dir);
}

main(argc, argv)
int argc;
char *argv[];
{
    char *subdir;
    char srcdir[MAXPATHLEN], dstdir[MAXPATHLEN];

    if (argc > 2) {
	fprintf(stderr, "usage: dupsrcs [ subdir ]\n");
	exit(1);
    }

    if (argc == 2)
	subdir = argv[1];
    else
	subdir = "alpha";

    getwd(dstdir);

    sprintf(srcdir, "/afs/cs/project/clisp/src/%s", subdir);
    if (chdir(srcdir) < 0) {
	perror(srcdir);
	exit(1);
    }
    getwd(srcdir);

    duptree(srcdir, dstdir);
}
