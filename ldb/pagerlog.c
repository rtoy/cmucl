/*
 * pager log facility
 */

#include <stdio.h>
#include <varargs.h>

#define PAGER_LOGFILE "./pager.log"

static initialized = 0;
static FILE *logfile;

static void
initialize_pagerlog()
{
	logfile = fopen(PAGER_LOGFILE, "w+");
	if (logfile == (FILE *) NULL) {
		fprintf(stderr, "pagerlog: cannot open %s\n", PAGER_LOGFILE);
		exit(1);
	}
	setlinebuf(logfile);
	initialized = 1;	
}

/*VARARGS1*/
pagerlog(fmt, va_alist)
char *fmt;
va_dcl
{
	va_list pvar;
	
	if (!initialized)
		initialize_pagerlog();

	va_start(pvar);
	vfprintf(logfile, fmt, pvar);
	va_end(pvar);
}
