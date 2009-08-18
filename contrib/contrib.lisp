;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/contrib/contrib.lisp,v 1.1 2009/08/18 13:12:42 rtoy Exp $")

;;; Define the various contrib modules.  We use defmodule for now
;;; because it's easy for the contribs.

(in-package "EXT")

(defmodule "contrib-demos"
    "modules:demos/demos" "modules:demos/menu")

(defmodule "contrib-follow-mouse"
    "modules:follow-mouse/follow-mouse")

(defmodule "contrib-games-feebs"
    "modules:games/feebs/feebs")

(defmodule "contrib-hist"
    "modules:hist/hist")

(defmodule "contrib-psgraph"
    "modules:psgraph/psgraph")
