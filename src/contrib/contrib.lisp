;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/contrib/contrib.lisp $")

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

;; This actually consists of several files, but compile-ops will
;; compile and load them all, so this module appears to have just the
;; one file.
(defmodule "contrib-ops"
    "modules:ops/compile-ops")

(defmodule "contrib-embedded-c"
    "modules:embedded-c/embedded-c")

;; Sprof really needs to be compiled, so make this module compile sprof. 
(defmodule "contrib-sprof"
    "modules:sprof/compile-sprof")

;; packed sse2 needs to be compiled to work.
(defmodule "contrib-packed-sse2"
    "modules:packed-sse2/compile-packed-sse2")

;; CPU performance counters, for x86 only.  By Eric Marsden.
#+x86
(defmodule "contrib-cpc"
    "modules:cpc/cpc")

;; Interface to SSL streams, by Eric Marsden.
(defmodule "contrib-ssl"
    "modules:ssl-cmucl/ssl-cmucl")

;; Tcp forwarder program which redirects TCP connections to another
;; port on another machine.  Uses Cmucl's SERVE.  By Eric Marsden.
(defmodule "contrib-tcp-forwarder"
    "modules:tcp-forwarder/tcp-forwarder")

;; Adapation of Chris Double's XML-RPC client for Cmucl (and Clisp).
;; By Eric Marsden.
(defmodule "contrib-xml-rpc"
    "modules:xml-rpc/xmlrpc")

(provide "cmu-contribs")
