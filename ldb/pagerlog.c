/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/pagerlog.c,v 1.2 1991/02/19 12:27:50 ch Exp $
 * 
 * pager log facility
 *
 */

#include <stdio.h>
#include <varargs.h>

#define PAGERLOG_ENVVAR "PAGERLOG"
#define PAGERLOG_DEFAULT "./pager.log"

extern char *getenv();

static initialized = 0;
static FILE *logfile;

static void
initialize_pagerlog()
{
	char *log;

	log = getenv(PAGERLOG_ENVVAR);
	if (log == NULL)
		log = PAGERLOG_DEFAULT;

	logfile = fopen(log, "w+");
	if (logfile == (FILE *) NULL) {
		fprintf(stderr, "pagerlog: cannot open \"%s\"\n", log);
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
