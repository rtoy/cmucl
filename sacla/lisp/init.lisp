;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: init.lisp,v 1.7 2004/09/02 06:59:43 yuji Exp $
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;;  * Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; I mainly use clisp, but other common lisp implementations should work
;; with a minor tweak.
;; Set *sacla-dir* to the dir where the sacla tree sits,
;; like (setq *sacla-dir* /path/to/sacla).
;; Load this file first (load "init"), then enter (in-package "TB").
;; Load and test a lisp file like the following,
;; (ld "reader")
;; (test "must-reader").
;;

(defvar *sacla-dir* "/usr/local/src/lisp/sacla")
(defvar *sacla-lisp-dir* (concatenate 'string *sacla-dir* "/lisp"))
(defvar *sacla-lisp-tests-dir* (concatenate 'string *sacla-dir* "/lisp/tests"))
;;"/cygdrive/c/src/lisp/sacla/lisp"
;;(setq *print-circle* t)

#+clisp (ext:cd *sacla-lisp-dir*)
#+cmu (unix:unix-chdir *sacla-lisp-dir*)

(load (concatenate 'string *sacla-lisp-dir* "/testbed"))

(in-package "TB")
(setq tb::*testbed-compile* t)
(tb::ld "share")

(push :sacla *features*)
