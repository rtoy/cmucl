# $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/GNUmakefile,v 1.10 1994/10/29 05:11:34 ram Exp $

all: lisp.nm

CC = gcc
DEPEND_FLAGS =

include Config

SRCS = lisp.c coreparse.c alloc.c monitor.c print.c interr.c \
	vars.c parse.c interrupt.c search.c validate.c gc.c globals.c \
	dynbind.c breakpoint.c regnames.c backtrace.c save.c purify.c \
	socket.c runprog.c time.c undefineds.c \
	${ARCH_SRC} ${ASSEM_SRC} ${OS_SRC}


OBJS = $(patsubst %.c,%.o,$(patsubst %.S,%.o,$(SRCS)))

### Don't look in RCS for the files, because we might not want the latest.
%: RCS/%,v

lisp.nm: lisp
	echo 'Map file for lisp version ' `cat version` > ,lisp.nm
	$(NM) lisp >> ,lisp.nm
	mv ,lisp.nm lisp.nm

lisp: version.c ${OBJS} version
	echo '1 + ' `cat version` | bc > ,version
	mv ,version version
	$(CC) ${CFLAGS} -DVERSION=`cat version` -c $<
	$(CC) -g ${OS_LINK_FLAGS} -o ,lisp \
		${OBJS} version.o \
		${OS_LIBS} -lm
	mv -f ,lisp lisp

version:
	echo 0 > version

### Socket.c needs to be compiled with UNIXCONN defined.
socket.o: socket.c
	$(COMPILE.c) -DUNIXCONN $<

internals.h:
	@echo "You must run genesis to create internals.h!"
	@false

clean:
	rm -f Depends *.o lisp lisp.nm core

depend: ${SRCS}
	$(CC) -MM -E ${DEPEND_FLAGS} ${CFLAGS} ${CPPFLAGS} $? > ,depends
	mv ,depends Depends

include Depends
