;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/final-sigma.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; This is a composing format that attempts to detect sigma in
;; word-final position and change it from "σ" (U+03C3) to "ς" (U+03C2).

(define-composing-external-format :final-sigma (:size 1 :documentation
"FINAL-SIGMA is a composing external format that attempts to detect
sigma in word-final position and changes it from U+03C3 to U+03C2.")
  (input (state input unput tmp1 tmp2 tmp3 tmp4)
    `(multiple-value-bind (,tmp1 ,tmp2) ,input
       (when (= ,tmp1 #x03C3)
	 (multiple-value-bind (,tmp3 ,tmp4) ,input
	   (when (or (not ,tmp3) (and (< ,tmp3 #x0370) (/= ,tmp3 #x0027)))
	     (setq ,tmp1 #x03C2))
	   (when ,tmp3
	     (,unput ,tmp4))))
       (values ,tmp1 ,tmp2)))
  (output (code state output)
    `(,output ,code)))
