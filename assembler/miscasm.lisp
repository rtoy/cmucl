;;; -*- Mode: Lisp; Package: Compiler -*-

;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************

(in-package 'compiler)

(export '(asm asm-files) (find-package 'compiler))

(defun asm (f)
  (assemble-file (concatenate 'simple-string "nmiscops:" f ".romp")))

(defun asm-files ()
  (asm "abs")
  (asm "allocation")
  (asm "arith")
  (asm "array")
  (asm "byte")
  (asm "call")
  (asm "nlx")
  (asm "compare")
  (asm "divide")
  (asm "gc")
  (asm "gcd")
  (asm "irrat")
  (asm "list")
  (asm "logic")
  (asm "minus")
  (asm "misc")
  (asm "multiply")
  (asm "negate")
  (asm "plus")
  (asm "print")
  (asm "predicate")
  (asm "save")
  (asm "stack")
  (asm "symbol")
  (asm "system")
  (asm "truncate"))
