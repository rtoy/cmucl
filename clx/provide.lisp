;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes; Package: USER;  -*-

;;;; Module definition for CLX

;;; This file is a Common Lisp Module description, but you will have to edit
;;; it to meet the needs of your site.

;;; Ideally, this file (or a file that loads this file) should be
;;; located in the system directory that REQUIRE searches.  Thus a user
;;; would say
;;;			(require :clx)
;;; to load CLX.  If there is no such registry, then the user must
;;; put in a site specific
;;;			(require :clx <pathname-of-this-file>)
;;;

(in-package :user)

(provide :clx)

;;; Load the defsystem file from the source directory.  You may
;;; want to include an explicit extension (such as ".l" or ".lisp").
;;;
(load "/src/local/clx/defsystem.l")

;;; The binary files for a particular lisp implementation and architecture.
;;;
(let ((lisp
	(or #+lucid "lucid"
            #+excl  "franz"
	    #+akcl  "akcl"
	    #+kcl   "kcl"
            #+ibcl  "ibcl"
	    (error "Can't figure out what lisp vendor this lisp is from.")))
      (computer
	(or #+(or sun3 (and sun (or mc68000 mc68020))) "sun3"
	    #+(or sun4 sparc) "sparc"
	    #+(and hp (or mc68000 mc68020)) "hp9000-300"
	    #+vax "vax"
	    (error "Can't figure out what computer vendor this computer is from."))))
  (xlib:load-clx (format nil "/src/local/clx/~A.~A/" lisp computer)))
