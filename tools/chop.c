#include <stdio.h>

#define BUFFERSIZE (1<<21)

char buffer[BUFFERSIZE];

void chop(infile)
char *infile;
{
    FILE *in, *out;
    char outfile[1024];
    int count, bytes;

    count = 0;

    in = fopen(infile, "r");
    if (in == NULL) {
	perror(infile);
	return;
    }

    while ((bytes = fread(buffer, 1, BUFFERSIZE, in)) != 0) {
	sprintf(outfile, "%s.%d", infile, count++);
	out = fopen(outfile, "w+");
	if (out == NULL) {
	    perror(outfile);
	    fclose(in);
	    return;
	}
	fwrite(buffer, 1, bytes, out);
	fclose(out);
    }

    fclose(in);
}

main(argc, argv)
int argc;
char *argv[];
{
    if (argc < 2) {
	fprintf(stderr, "usage: chop file...\n");
	exit(1);
    }

    while (*++argv != NULL)
	chop(*argv);
}
