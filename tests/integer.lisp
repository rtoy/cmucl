;; Tests of integer functions

(defpackage :integer-tests
  (:use :cl :lisp-unit))

(in-package "INTEGER-TESTS")

;; Simple functions for testing INTEGER-LENGTH for numbers of type
;; (unsigned-byte 32) and (signed-byte 32).
(defun integer-length-u32 (n)
  (declare (type (unsigned-byte 32) n))
  (integer-length n))

(defun integer-length-s32 (n)
  (declare (type (signed-byte 32) n))
  (integer-length n))

(define-test integer-length.unsigned-byte-32
    (:tag :issues)
  (assert-equal 0 (integer-length-u32 0))
  (assert-equal 1 (integer-length-u32 1))
  (assert-equal 31 (integer-length-u32 #x70000000))
  (assert-equal 31 (integer-length-u32 #x7fffffff))
  (assert-equal 32 (integer-length-u32 #xffffffff)))

(define-test integer-length.signed-byte-32
    (:tag :issues)
  (assert-equal 0 (integer-length-s32 0))
  (assert-equal 1 (integer-length-s32 1))
  (assert-equal 31 (integer-length-s32 #x70000000))
  (assert-equal 31 (integer-length-s32 #x7fffffff))
  (assert-equal 0 (integer-length-s32 -1))
  (assert-equal 1 (integer-length-s32 -2))
  (assert-equal 31 (integer-length-s32 #x-70000000))
  (assert-equal 31 (integer-length-s32 #x-7fffffff))
  (assert-equal 31 (integer-length-s32 #x-80000000)))

           
