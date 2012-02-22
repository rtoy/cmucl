# $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/GNUmakefile,v 1.39 2010/10/14 17:47:12 rtoy Exp $

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
# executable.  However, we need to remove exec-init.o from the library
# and replace it with exec-final.o because exec-final.o sets the
# builtin_image_flag to 1 so that the runtime knows the runtime
# contains the core sections.
lisp.a:	version.o ${OBJS} ${EXEC_FINAL_OBJ}
	ar crs lisp.a ${OBJS} version.o
ifneq (${EXEC_FINAL_OBJ},)
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

# When processing the translations, we merge the po file from the src
# directory with the appropriate pot file.  The result is placed in
# the build directory.  Then we compare the original po file with the
# newly generated one (but sorting both before comparing).  If they
# are different we print out a note so that we know to look into it
# and decide if the po file in the src directory needs to be updated.
#
# The sorting is done so that differences in order of the lines is not
# important.  In particular, msgmerge on OSX wants to put the
# Language: line after the Content-Transfer-Encoding line.  But on
# FreeBSD, it moves the line to after the Language-Team line.  This
# makes things messy.  Hence, sort and diff.
translations: 
	for pot in ../../src/i18n/locale/*.pot; do \
	  for po in $(LOCALE_DIRS); do \
            d=`dirname $$pot`; \
	    f=`basename $$pot .pot`; \
	    echo ; \
	    echo '***' Processing $$f.pot:  $$po; \
	    msgmerge -v ../../src/$$po/$$f.po $$pot -o ../$$po/$$f.po; \
	    msgfmt -v  ../$$po/$$f.po -o ../$$po/$$f.mo; \
	    sort < ../$$po/$$f.po > /tmp/$$$$-updated-$$f.po; \
	    sort < ../../src/$$po/$$f.po > /tmp/$$$$-orig-$$f.po; \
	    diff -uwB /tmp/$$$$-orig-$$f.po /tmp/$$$$-updated-$$f.po || echo "Warning:  $$po/$$f.po changed"; \
	    rm -f /tmp/$$$$-updated-$$f.po /tmp/$$$$-orig-$$f.po; \
	  done; done

.PHONY : translations

# Like translations, but we don't compute the diff.  We just overwrite
# the po files in the repository so that we can tell if the
# translations have changed and need to be updated.  (This is what the
# old translations used to do.)
translations-update: 
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

