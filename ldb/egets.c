/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/egets.c,v 1.1 1990/02/24 19:37:15 wlott Exp $ */
/********************************************************************
*                                                                   *
*                                                                   *
*     Copyright (C) 1987, Carnegie Mellon University                *
*                                                                   *
*                                                                   *
********************************************************************/

/* egets.c --	replacement for C library's gets(3s)
 *		allows emacs style command editing if stdin is a tty
 *						Derek Beatty
 * HISTORY
 *	20 Nov 87 Beatty: added code for more-mode.
 *		More-mode is handled by this file so gets can reset
 *		its line count.
 */
#include <stdio.h>
#include <ctype.h>
#include <sgtty.h>
#include <signal.h>
#include <strings.h>
#include <setjmp.h>

int getpid();		/* this should have been declared somewhere */

/*LINTLIBRARY*/

#define TRUE (1)
#define FALSE (0)

typedef unsigned char uchar;

#define HISTBUF		(2000)
#define LINEBUF		(132)

#define CONTROL(x)	((x)&0x1F)
#define META(x)		((x)|0x80)
#define BELL		CONTROL('g')
#define ESC		(0x1B)
#define RUBOUT		(0x7F)
#define ISCONTROL(x)	((x)<0x20 || (x)>0x7E)		/* includes meta */
#define NOTPREFIX(c)	((c)!=ESC)

/** Global Variables for user settings */
int ScrollPauseSwitch;
int ScreenLengthLimit;
int EditSwitch = TRUE;

/** Shared by MORE and GETS **/
static int CurrentLine = 1;
static int Initialized = FALSE;/* true iff initialize has been called */

/** VARIABLES FOR GETS **/
static struct sgttyb OttyFlags, NttyFlags;
static struct ltchars OltChars, NltChars;
  
#ifdef SIGTSTP
static int (*TstpHandler)();
#endif SIGTSTP
  
static int Active = TRUE;	/* true iff egets is working */
static uchar History[HISTBUF];	/* history buffer */
static uchar
	*FirstHistory,		/* ptr to earliest line in history buffer */
	*YankedHistory;		/* ptr to history last yanked */
static uchar KillBuf[LINEBUF+1];	/* kill buffer */
static uchar Line[LINEBUF+1];	/* line being entered, plus null */
static uchar
	*Cursor,		/* ptr to character cursor is on */
	*Bol,			/* ptr to first char of line */
	*Eol;			/* ptr to null that terminates line */

static uchar tLine[LINEBUF+1];	/* line displayed on terminal */
static uchar *tCursor, *tBol, *tEol;

static uchar Meta = 0;	/* meta bit; 0x80 if meta key was last */
static int Bleep = FALSE;   /* TRUE iff bell needed at next screen update */

static jmp_buf out_of_here;

static void saveTtyModes(), restoreTtyModes(), initialize(), showLine(),
	killToEol(), yankNext(), yankPrev(),	yankKillBuf(), 
	transpose(), openSpace(), closeSpace();

/* saveTtyModes -- save tty state and put tty into cbreak mode */
static void 
saveTtyModes()
{
    (void) ioctl(fileno(stdin), TIOCGETP, (char*) &OttyFlags);
    NttyFlags = OttyFlags;
    NttyFlags.sg_flags |= CBREAK;
    NttyFlags.sg_flags &= ~XTABS;
    NttyFlags.sg_flags &= ~ECHO;
    (void) ioctl(fileno(stdin), TIOCSETN, (char*) &NttyFlags);
    (void) ioctl(fileno(stdin), TIOCGLTC, (char*) &OltChars);
    NltChars = OltChars;
    NltChars.t_dsuspc = NltChars.t_suspc;
    (void) ioctl(fileno(stdin), TIOCSLTC, (char*) &NltChars);
}

/* restoreTtyModes -- restore tty state */
static void 
restoreTtyModes()
{
    (void) ioctl(fileno(stdin), TIOCSLTC, (char*) &OltChars);
    (void) ioctl(fileno(stdin), TIOCSETN, (char*) &OttyFlags);
}

/* isaCRT -- return nonzero iff fd is a tty with CRT bits set */
static int 
isaCRT( fd)
int fd;
{
    int             ltbits;

    (void) ioctl(fd, TIOCLGET, (char*) &ltbits);
    return (ltbits & (LCRTERA | LCRTKIL));
}

/* tstp -- handler for SIGTSTP: restore tty and suspend */
#ifdef SIGTSTP
int 
tstp()
{
    if (TstpHandler == SIG_IGN)
	return;
    restoreTtyModes();
    if (TstpHandler == SIG_DFL)
	(void) kill(getpid(), SIGSTOP);
    else
	(*TstpHandler) ();
    saveTtyModes();
    tBol = tEol = tCursor = tLine;
    tLine[0] = '\0';
    showLine();
}
#endif

/* initialize -- init data structures used by gets and more. called once. */
static void 
initialize()
{
    int             i;

    /* initialization for gets */
    Initialized = TRUE;
    YankedHistory = FirstHistory = History;
    for (i = 0; i < HISTBUF; i++)
	History[i] = '\0';
    KillBuf[0] = '\0';

    /* initialization for more */
    ScrollPauseSwitch= isaCRT(fileno(stdin)) && isatty(fileno(stdout));

    ScreenLengthLimit= -1;
#ifdef TIOCGWINSZ /* 4.3bsd window size */
    {
	struct winsize wss;
	ioctl(fileno(stdin), TIOCGWINSZ, (char*) &wss);
	ScreenLengthLimit= wss.ws_row-1;
    }
#endif
#ifdef TIOCGSIZE /* sun window size */
    {
	struct ttysize tss;
	ioctl(fileno(stdin), TIOCGSIZE, (char*) &tss);
	ScreenLengthLimit= tss.ts_lines-1;
    }
#endif
    if (ScreenLengthLimit==-1)
	ScreenLengthLimit = 24;
}


/* killToEol -- delete characters on and following cursor into kill buffer */
static void 
killToEol()
{
    int             n = strlen( (char*) Cursor);

    (void) strcpy((char*) KillBuf, (char*) Cursor);
    closeSpace(n);
}

/* transpose -- transpose two characters preceding cursor */
static void 
transpose()
{
    if (Cursor <= Bol + 1)
	Bleep = TRUE;
    else {
	uchar           c = *(Cursor - 1);
	*(Cursor - 1) = *(Cursor - 2);
	*(Cursor - 2) = c;
    }
}

/* numInWord -- return number of chars through word before or after cursor */
static int
numInWord( before)
  int before;		/* true iff want count in word before cursor */
{
    uchar          *tempCursor = Cursor;
    int             n;

    if (before) {
	if (tempCursor > Bol)
	    tempCursor--;
	else
	    Bleep = TRUE;
	while (tempCursor > Bol && !isalnum(*tempCursor))
	    tempCursor--;
	while (tempCursor > Bol && isalnum(*tempCursor))
	    tempCursor--;
	n = Cursor - tempCursor;
	if (n != 0 && !isalnum(*tempCursor))
	    n--;
	return (n);
    } else {
	if (tempCursor >= Eol)
	    Bleep = TRUE;
	while (tempCursor < Eol && !isalnum(*tempCursor))
	    tempCursor++;
	while (tempCursor < Eol && isalnum(*tempCursor))
	    tempCursor++;
	return (tempCursor - Cursor);
    }
}

/* yankHistory -- yank line at YankedHistory into Line */
static void 
yankHistory()
{
    uchar          *tempHistory;

    YankedHistory++;
    if (YankedHistory >= History + HISTBUF)
	YankedHistory = History;
    tempHistory = YankedHistory;
    Bol = Cursor = Line;
    while (*tempHistory) {
	*Cursor++ = *tempHistory++;
	if (tempHistory >= History + HISTBUF)
	    tempHistory = History;
    }
    *Cursor = '\0';
    Eol = Cursor;
}

/* yankNext -- yank the next line from the history buffer */
static void 
yankNext()
{
    while (*YankedHistory) {
	YankedHistory++;
	if (YankedHistory >= History + HISTBUF)
	    YankedHistory = History;
    }
    yankHistory();
}

/* yankPrev -- yank the previous line from the history buffer */
static void 
yankPrev()
{
    YankedHistory--;
    if (YankedHistory < History)
	YankedHistory = History + HISTBUF - 1;
    YankedHistory--;
    if (YankedHistory < History)
	YankedHistory = History + HISTBUF - 1;
    while (*YankedHistory) {
	YankedHistory--;
	if (YankedHistory < History)
	    YankedHistory = History + HISTBUF - 1;
    }
    yankHistory();
}

/* addHistory -- add the current line to the history buffer */
static void 
addHistory()
{
    uchar          *tempCursor = Bol;

    while (*tempCursor) {
	*FirstHistory++ = *tempCursor++;
	if (FirstHistory >= History + HISTBUF)
	    FirstHistory = History;
    }
    *FirstHistory++ = '\0';
    if (FirstHistory >= History + HISTBUF)
	FirstHistory = History;
    YankedHistory = FirstHistory;
}

/* yankKillBuf -- insert kill buffer in current line.  Bleep if trouble */
static void 
yankKillBuf()
{
    uchar          *sourceCursor = KillBuf;
    uchar          *destCursor = Cursor;
    int             n = strlen((char*) KillBuf);

    if (Eol + n > Line + LINEBUF)
	Bleep = TRUE;
    else {
	openSpace(n);
	while (*sourceCursor)
	    *destCursor++ = *sourceCursor++;
	Cursor += n;
    }
}

/* redraw -- force showLine to redraw current line on screen */
static void 
redraw()
{
    tCursor = tBol = tEol = tLine;
    tLine[0] = '\0';
    printf("\n");
    (void) fflush(stdout);
}

/* showLine -- update screen and tLine to reflect Line */
static void 
showLine()
{
    uchar          *Dif, *tDif;


    /* find first place the lines differ */
    Dif = Bol;
    tDif = tBol;
    while (*Dif && *tDif && *Dif == *tDif) {
	Dif++;
	tDif++;
    }
    if (*Dif || *tDif) {
	/* lines differ: move tCursor to the point of difference */
	while (tCursor > tDif) {
	    printf("\b");
	    tCursor--;
	}
	while (tCursor < tDif)
	    printf("%c", *tCursor++);
	/* write remainder of Line */
	while (*Dif)
	    printf("%c", *tCursor++ = *Dif++);
	/* add blanks to bring tLine up to length of Line */
	while (tCursor < tEol) {
	    printf(" ");
	    *tCursor++ = '\0';
	}
	/* adjust tEol */
	tEol = tBol + (Eol - Bol);
    }
    /* move tCursor to proper place */
    while (tCursor - tBol > Cursor - Bol) {
	printf("\b");
	tCursor--;
    }
    while (tCursor - tBol < Cursor - Bol)
	printf("%c", *tCursor++);
    /* cleanup */
    if (Bleep) {
	printf("%c", BELL);
	Bleep = FALSE;
    }
    (void) fflush(stdout);
}

/* openSpace -- open space for n characters at cursor.  Beep if trouble. */
static void 
openSpace( n)
  int n;
{
    uchar          *sourceCursor, *destCursor;

    if (n == 0)
	return;
    destCursor = Eol + n;
    if (destCursor > Line + LINEBUF + 1)
	Bleep = TRUE;
    else {
	sourceCursor = Eol;
	Eol = destCursor;
	while (sourceCursor >= Cursor)
	    *destCursor-- = *sourceCursor--;
    }
}

/* closeSpace -- close space for n characters at cursor.  Bleep if trouble. */
static void 
closeSpace( n)
  int n;
{
    uchar          *sourceCursor, *destCursor;

    if (n == 0)
	return;
    if (n > Eol - Cursor) {
	Bleep = TRUE;
	n = Eol - Cursor;
    }
    sourceCursor = Cursor + n;
    destCursor = Cursor;
    while (sourceCursor <= Eol)
	*destCursor++ = *sourceCursor++;
    Eol = destCursor - 1;
}

/* ogets -- get a line without command line editing */
static char *
ogets(s, n)
char *s;
int n;				/* total length of string */
{
	int c, i;
	char *t;

	t = s; i = n;

	if (n <= 0)
		return NULL;

	while ((--i > 0) && ((c = getchar()) != '\n') && (c != EOF))
		*t++ = c;
	if ((n != 1) && (c == EOF) && (s == t))
		return NULL;
	*t = '\0';

	return(s);
}

static sigint_handler()
{
    longjmp(out_of_here, 1);
}


/* egets -- get a line from stdin with command line editing */
char * 
egets()
{
    uchar           c= '\0';
    int            done, returnEOF;
    long int        buffered= 0L;
    int             n;
    int             arg = 0;
    int            gettingArg = FALSE, gettingDigitArg = FALSE;
    int            stillGettingArg = FALSE;
    struct sigvec sv, old_int_sv;

    if (!EditSwitch
	|| !Active
	|| (!Initialized && (!isatty(fileno(stdin)) ))
	|| !isaCRT(fileno(stdin))) {
	Active = FALSE;
	return (ogets(Line, LINEBUF + 1));
    }
    if (!Initialized)
	initialize();
    fflush(stdout);
    CurrentLine=1;		/* Reset line count for more-mode. */
    Line[0] = '\0';
    tLine[0] = '\0';
    Cursor = Bol = Eol = Line;
    tCursor = tBol = tEol = tLine;
#ifdef SIGTSTP
    TstpHandler = signal(SIGTSTP, tstp);
#endif
    saveTtyModes();
    if (setjmp(out_of_here))
        returnEOF = TRUE;
    else {
        sv.sv_handler = sigint_handler;
        sv.sv_mask = 0;
        sv.sv_flags = 0;
        sigvec(SIGINT, &sv, &old_int_sv);

        returnEOF = done = FALSE;
        while (!done) {
            /* lint complains that c may be used before set */
            if (!gettingArg && arg > 0 && NOTPREFIX(c))
                arg--;
            else {
                if (read(fileno(stdin), (char*) &c, 1) == 0)
                    c = CONTROL('m');
                else {
                    c |= Meta;
                    Meta = 0;
                }
            }
            if (ISCONTROL(c)) {
                switch (c) {
                    case CONTROL('a'):
                        Cursor = Bol;
                        break;
                    case CONTROL('b'):
                        if (Cursor > Bol)
                            Cursor--;
                        else
                            Bleep = TRUE;
                        break;
                    case CONTROL('d'):
                        if (Cursor < Eol)
                            closeSpace(1);
                        else
                            Bleep = TRUE;
                        break;
                    case CONTROL('e'):
                        Cursor = Eol;
                        break;
                    case CONTROL('f'):
                        if (Cursor < Eol)
                            Cursor++;
                        else
                            Bleep = TRUE;
                        break;
                    case CONTROL('h'):
                    case RUBOUT:
                        if (Cursor > Bol) {
                            Cursor--;
                            closeSpace(1);
                        } else
                            Bleep = TRUE;
                        break;
                    case CONTROL('j'):
                        done = TRUE;
                        break;
                    case CONTROL('k'):
                        killToEol();
                        break;
                    case CONTROL('m'):
                        done = TRUE;
                        break;
                    case CONTROL('n'):
                        yankNext();
                        break;
                    case CONTROL('p'):
                        yankPrev();
                        break;
                    case CONTROL('r'):
                        redraw();
                        break;
                    case CONTROL('t'):
                        transpose();
                        break;
                    case CONTROL('u'):
                        if (gettingArg)
                            arg *= 4;
                        else
                            arg = 4;
                        stillGettingArg = TRUE;
                        gettingDigitArg = FALSE;
                        break;
                    case CONTROL('y'):
                        yankKillBuf();
                        break;
                    case ESC:
                        Meta = 0x80;
                        break;
                    case META('f'):
                        Cursor += numInWord(0);
                        break;
                    case META('b'):
                        Cursor -= numInWord(1);
                        break;
                    case META('d'):
                        closeSpace(numInWord(0));
                        break;
                    case CONTROL('w'):
                    case META('h'):
                        n = numInWord(1);
                        Cursor -= n;
                        closeSpace(n);
                        break;
                    case META(CONTROL('d')):
                        if (Bol == Eol)
                            done = returnEOF = TRUE;
                        else
                            Bleep = TRUE;
                        break;
                    default:
                        Bleep = TRUE;
                        break;
                }
            } else if (!gettingArg || !isdigit(c)) {
                if (Eol + 1 > Line + LINEBUF)
                    Bleep = TRUE;
                else {
                    openSpace(1);
                    *Cursor++ = c;
                }
            } else {
                /* getting argument digits */
                if (gettingDigitArg)
                    arg = 10 * arg + c - '0';
                else
                    arg = c - '0';
                stillGettingArg = TRUE;
                gettingDigitArg = TRUE;
            }
            if (gettingArg && !stillGettingArg)
                arg--;
            gettingArg = stillGettingArg;
            stillGettingArg = FALSE;
            if (Bleep)
                arg = 0;
            (void) ioctl(fileno(stdin), FIONREAD, (char*) &buffered);
            if ((buffered == 0L && arg == 0) || done)
                showLine();
        }
        addHistory();
    }

    if (!returnEOF) printf("\n");
    (void) fflush(stdout);

    restoreTtyModes();
#ifdef SIGTSTP
    (void) signal(SIGTSTP, TstpHandler);
#endif
    sigvec(SIGINT, &old_int_sv, NULL);
    return (returnEOF ? NULL : (char *) Line);
}
