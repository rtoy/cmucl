# Makefile for generating the real Makefile.
# $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/Makefile.boot,v 1.2 1991/05/24 19:32:40 wlott Exp $
#

Makefile: Makefile.orig
	/usr/cs/lib/cpp < Makefile.orig > Makefile.NEW
	mv Makefile.NEW Makefile
