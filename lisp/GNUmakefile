# $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/GNUmakefile,v 1.36 2010/07/29 01:51:12 rtoy Exp $

all: lisp.nm

-include internals.inc
include Config

SRCS = lisp.c coreparse.c alloc.c monitor.c print.c interr.c \
	vars.c parse.c interrupt.c search.c validate.c globals.c \
	dynbind.c breakpoint.c regnames.c backtrace.c save.c purify.c \
	runprog.c time.c exec-init.c \
	${ARCH_SRC} ${ASSEM_SRC} ${OS_SRC} ${GC_SRC}

OBJS = $(patsubst %.c,%.o,$(patsubst %.S,%.o,$(patsubst %.s,%.o,$(SRCS))))

### Don't look in RCS for the files, because we might not want the latest.
%: RCS/%,v

lisp.nm: lisp lisp.a
	echo 'Map file for lisp version ' `cat version` > ,lisp.nm
	$(NM) lisp | grep -v " [F] " >> ,lisp.nm
	mv ,lisp.nm lisp.nm

version.o : version.c version
	echo '1 + ' `cat version` | bc > ,version
	mv ,version version
	$(CC) ${CFLAGS} $(CPPFLAGS) -DVERSION=`cat version` -c $<

lisp: ${OBJS} version.o
	$(CC) -g ${OS_LINK_FLAGS} -o ,lisp \
		${OBJS} version.o \
		${OS_LIBS} -lm
	mv -f ,lisp lisp

# Create a library out of all the object files so we can build an
# executable.  However, for Solaris, we need to remove exec-init.o
# because the linker will define symbols with the correct addresses.
# Also, we need to add EXEC_FINAL on Solaris to define
# builtin_image_flag with the correct value.
lisp.a:	version.o ${OBJS} ${EXEC_FINAL_OBJ}
	ar crs lisp.a ${OBJS} version.o
ifdef FEATURE_SOLARIS
	ar d lisp.a exec-init.o
	ar r lisp.a ${EXEC_FINAL_OBJ}
endif

version:
	echo 0 > version

internals.h internals.inc:
	@echo "You must run genesis to create internals.h!"
	@false

clean:
	rm -f Depends *.o lisp lisp.nm core
	echo 'Map file for lisp version 0' > lisp.nm

depend: Depends

Depends: ${SRCS}
	$(DEPEND) ${DEPEND_FLAGS} ${CFLAGS} ${CPPFLAGS} $^ > ,depends
	mv ,depends Depends

-include Depends

# Find all directories in ../i18n/locale.  These are the locales we
# currently support.

# This would be a nice way to do it so we don't have to keep track of
# the directories, but Solaris' find doesn't grok -depth 1
#LOCALES=$(patsubst ../i18n/locale/%, %, $(shell find ../i18n/locale -type d -depth 1))
LOCALES=en@piglatin ko

# Convert locale names to the appropriate path where we want the mo files to go.
LOCALE_DIRS = $(patsubst %, i18n/locale/%/LC_MESSAGES, $(LOCALES))

translations: 
	for pot in ../../src/i18n/locale/*.pot; do \
	  for po in $(LOCALE_DIRS); do \
            d=`dirname $$pot`; \
	    f=`basename $$pot .pot`; \
	    touch ../../src/$$po/$$f.po; \
	    echo ; \
	    echo '***' Processing $$f.pot:  $$po; \
	    msgmerge -v ../../src/$$po/$$f.po $$pot -o ../../src/$$po/$$f.po; \
	    msgfmt -v  ../../src/$$po/$$f.po -o ../$$po/$$f.mo; \
	  done; done

.PHONY : translations

