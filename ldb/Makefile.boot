# Makefile for generating the real Makefile.
# $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/Makefile.boot,v 1.1 1990/10/21 14:40:35 wlott Exp $
#

Makefile: Makefile.orig
	/lib/cpp < Makefile.orig > Makefile.NEW
	mv Makefile.NEW Makefile
