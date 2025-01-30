#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Err {
    char *name;
    int num;
#if 0
    char *descr;
#endif    
} err[256];

int nerr = 0;

regex_t reg;

void match(const char *line) {
    regmatch_t match[3];
    regoff_t len;

    if (regexec(&reg, line, 3, match, 0) == REG_NOMATCH)
	return;

    len = match[1].rm_eo - match[1].rm_so;
    err[nerr].name = malloc(len + 1);
    sprintf(err[nerr].name, "%.*s", len, line + match[1].rm_so);

    err[nerr].num = atoi(line + match[2].rm_so);

#if 0
    err[nerr].descr = strerror(err[nerr].num);
#endif    

    nerr++;
}

int cmp(const void *a, const void *b) {
    return ((struct Err *)a)->num - ((struct Err *)b)->num;
}

int main(int argc, char **argv)
{
    int i;
    FILE *fp;
    char line[1024];

    regcomp(&reg, "^#define[ \t]*(E[A-Z0-9]+)[ \t]*([0-9]+)", REG_EXTENDED);

    for (i = 1; i < argc; i++) {
	if ((fp = fopen(argv[i], "r")) == NULL) {
	    perror("fopen");
	    exit(1);
	}
	while (fgets(line, sizeof(line), fp) != NULL)
	    match(line);
	fclose(fp);
    }

    qsort(err, nerr, sizeof(*err), cmp);

    puts("(in-package \"UNIX\")\n");
    for (i = 0; i < nerr; i++) {
#if 0
	printf("(def-unix-error %s %d \"%s\")\n", err[i].name, err[i].num, err[i].descr);
#else	
	printf("(def-unix-error %s %d)\n", err[i].name, err[i].num);
#endif	
    }

    return 0;
}
