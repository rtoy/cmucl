# $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/GNUmakefile,v 1.1 1992/07/28 20:13:58 wlott Exp $

all: lisp.nm

CC = gcc

include Config

SRCS = lisp.c coreparse.c alloc.c monitor.c print.c interr.c os-common.c \
	vars.c parse.c interrupt.c search.c validate.c gc.c globals.c \
	dynbind.c breakpoint.c regnames.c backtrace.c save.c purify.c \
	socket.c ${ARCH_SRC} ${ASSEM_SRC} ${OS_SRC}

OBJS = $(patsubst %.c,%.o,$(patsubst %.S,%.o,$(SRCS)))

### Don't look in RCS for the files, because we might not want the latest.
%: RCS/%,v

lisp.nm: lisp
	echo -n 'Map file for lisp version ' > ,lisp.nm
	cat version >> ,lisp.nm
	$(NM) lisp >> ,lisp.nm
	mv ,lisp.nm lisp.nm

lisp: ${OBJS} version undefineds
	echo -n '1 + ' | cat - version | bc > ,version
	mv ,version version
	$(CC) ${CFLAGS} -DVERSION=`cat version` -c version.c
	$(CC) $(CFLAGS) ${OS_LINK_FLAGS} `cat undefineds` -o ,lisp \
		${OBJS} version.o \
		${OS_LIBS} -lm
	mv -f ,lisp lisp

version:
	echo 0 > version

undefineds: undefineds.src
	${CPP} undefineds.src | \
	sed -e '/^#/d' -e '/^[ 	]*$$/d' -e 's/.*/-Xlinker -u -Xlinker ${UNDEFSYMPATTERN}/' | \
	sort -u > ,undefineds
	mv ,undefineds undefineds

### Socket.c needs to be compiled with UNIXCONN defined.
socket.o: socket.c
	$(COMPILE.c) -DUNIXCONN socket.c

internals.h:
	@echo "You must run genesis to create internals.h!"
	@false

clean:
	rm -f Depends lisp.h undefineds *.o lisp lisp.nm

depend:
	$(CC) -MM ${CFLAGS} ${CPPFLAGS} ${SRCS} > ,depends
	mv ,depends Depends

include Depends
