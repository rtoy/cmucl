;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: array.lisp,v 1.4 2004/02/20 07:12:10 yuji Exp $
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


(defun aref (array &rest subscripts)
  "Access an element of ARRAY specified by SUBSCRIPTS."
  (row-major-aref array (apply #'array-row-major-index array subscripts)))

(defsetf aref (array &rest subscripts) (value)
  "Set VALUE onto the element of ARRAY specified by SUBSCRIPTS."
  `(setf (row-major-aref ,array (array-row-major-index ,array ,@subscripts))
	 ,value))

(defun array-dimension (array axis-number)
  "Return AXIS-NUMBER dimension of ARRAY."
  (nth axis-number (array-dimensions array)))

(defun array-in-bounds-p (array &rest subscripts)
  "Return true if SUBSCRIPTS are all in bounds for ARRAY, otherwise false."
  (and (not (some #'minusp subscripts))
       (every #'< subscripts (array-dimensions array))))

(defun array-rank (array)
  "Return the number of dimensions of ARRAY."
  (length (array-dimensions array)))

(defun array-row-major-index (array &rest subscripts)
  "Compute the row-major index of the element of ARRAY specified by SUBSCRIPTS."
  (assert (apply #'array-in-bounds-p array subscripts))
  (apply #'+ (maplist #'(lambda (x y)
			  (* (car x) (apply #'* (cdr y))))
		      subscripts
		      (array-dimensions array))))

(defun array-total-size (array)
  "Return the total number of elements in ARRAY."
  (apply #'* (array-dimensions array)))


(defun vector (&rest objects)
  "Create a fresh simple general vector whose elements are OBJECTS."
  (make-array (length objects)
	      :element-type t
	      :initial-contents objects))

(defun vector-pop (vector)
  "Decrease the fill pointer of VECTOR by one and return the top element."
  (check-type vector vector)
  (assert (and (array-has-fill-pointer-p vector)
	       (plusp (fill-pointer vector))))
  (aref vector (setf (fill-pointer vector) (1- (fill-pointer vector)))))

(defun vector-push (new-element vector)
  "Try to store NEW-ELEMENT in VECTOR's element designated by the fill pointer."
  (let ((fill-pointer (fill-pointer vector)))
    (when (< fill-pointer (array-dimension vector 0))
      (setf (aref vector fill-pointer) new-element)
      (setf (fill-pointer vector) (1+ fill-pointer))
      fill-pointer)))

(defun vector-push-extend (new-element vector &optional
				       (extension (1+ (length vector))))
  "Do the same thing as vector-push but extend VECTOR when space is lacking."
  (when (>= (fill-pointer vector) (array-dimension vector 0))
    (assert (adjustable-array-p vector))
    (adjust-array vector (+ (fill-pointer vector) extension)))
  (vector-push new-element vector))

(defun vectorp (object)
  "Return true if OBJECT is of type vector; otherwise, return false."
  (and (arrayp object)
       (eql (array-rank object) 1)))


(defun bit-andc1 (bit-array1 bit-array2 &optional opt-arg)
  "And complement of BIT-ARRAY1 with BIT-ARRAY2."
  (bit-and (bit-not bit-array1 opt-arg) bit-array2 opt-arg))

(defun bit-andc2 (bit-array1 bit-array2 &optional opt-arg)
  "And BIT-ARRAY1 with complement of BIT-ARRAY2."
  (bit-and bit-array1 (bit-not bit-array2) opt-arg))

(defun bit-eqv (bit-array1 bit-array2 &optional opt-arg)
  "Exclusive nor (equivalence) between BIT-ARRAY1 and BIT-ARRAY2."
  (bit-not (bit-xor bit-array1 bit-array2 opt-arg) opt-arg))

(defun bit-nand (bit-array1 bit-array2 &optional opt-arg)
  "Complement of BIT-ARRAY1 and BIT-ARRAY2."
  (bit-not (bit-and bit-array1 bit-array2 opt-arg) opt-arg))

(defun bit-nor (bit-array1 bit-array2 &optional opt-arg)
  "Complement of BIT-ARRAY1 or BIT-ARRAY2."
  (bit-not (bit-ior bit-array1 bit-array2 opt-arg) opt-arg))

(defun bit-orc1 (bit-array1 bit-array2 &optional opt-arg)
  "Or complement of BIT-ARRAY1 with BIT-ARRAY2."
  (bit-ior (bit-not bit-array1 opt-arg) bit-array2 opt-arg))

(defun bit-orc2 (bit-array1 bit-array2 &optional opt-arg)
  "Or BIT-ARRAY1 with complement of BIT-ARRAY2."
  (bit-ior bit-array1 (bit-not bit-array2) opt-arg))


(defun bit-vector-p (object)
  "Return true if OBJECT is of type bit-vector; otherwise, return false."
  (and (vectorp object)
       (eq (array-element-type object) 'bit)))

(defun simple-bit-vector-p (object)
  "Return true if OBJECT is of type simple-bit-vector; otherwise, return false."
  (and (bit-vector-p object)
       (typep object 'simple-array)))
