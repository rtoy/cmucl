;;; -*- Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/stream-vector-io.lisp,v 1.3.6.2 2009/04/19 04:09:56 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Stream I/O for vectors
;;; Written by Lynn Quam
;;;

(in-package "EXT")

(export '(read-vector write-vector))

;;; READ-VECTOR WRITE-VECTOR 

(declaim (start-block read-vector write-vector))

;;;  ENDIAN SWAPPING

;;; Not sure that this is really need, but for completeness ...
(defun swap-endians-456 (vector start end endian)
  (declare (optimize (speed 3)(safety 0)))
  (declare (type kernel:simple-stream-buffer vector))
  (declare (fixnum start end endian))
  (loop for i fixnum from (* 8 start) below (* 8 end) by 8
	for b0 fixnum = (bref vector i)
	for b1 fixnum = (bref vector (+ i 1))
	for b2 fixnum = (bref vector (+ i 2))
	for b3 fixnum = (bref vector (+ i 3))
	for b4 fixnum = (bref vector (+ i 4))
	for b5 fixnum = (bref vector (+ i 5))
	for b6 fixnum = (bref vector (+ i 6))
	for b7 fixnum = (bref vector (+ i 7))
	do (setf (bref vector (logxor i endian)) b0)
	   (setf (bref vector (logxor (+ i 1) endian)) b1)
	   (setf (bref vector (logxor (+ i 2) endian)) b2)
	   (setf (bref vector (logxor (+ i 3) endian)) b3)
	   (setf (bref vector (logxor (+ i 4) endian)) b4)
	   (setf (bref vector (logxor (+ i 5) endian)) b5)
	   (setf (bref vector (logxor (+ i 6) endian)) b6)
	   (setf (bref vector (logxor (+ i 7) endian)) b7)
	))
;(disassemble 'endian-swap-vector)
(defun endian-swap-vector (vector start end endian-swap)
  (declare (optimize (speed 3)(safety 0)))
  (declare (type simple-array vector))
  (declare (fixnum start end endian-swap ))
  (unless (eql endian-swap 0)
    (when (or (>= endian-swap (vector-elt-width vector))
	      (< endian-swap 0))
      (error "endian-swap ~a is illegal for element-type of vector ~a"
	     endian-swap vector))
    (lisp::with-array-data ((data vector) (offset-start start)
			    (offset-end end))
      ;;(declare (type (kernel:simple-unboxed-array (*)) data))
      (macrolet ((swap8 (i j) 
		   `(rotatef (bref data ,i)  (bref data ,j))))
	(case endian-swap
	  (1 (loop for i fixnum from (* 2 start) below (* 2 end) by 2
		do (swap8 i (+ i 1))))
	  (3 (loop for i fixnum from (* 4 start) below (* 4 end) by 4
		do (swap8 i (+ i 3))
		(swap8 (+ i 1) (+ i 2))))
	  (7 (loop for i fixnum from (* 8 start) below (* 8 end) by 8
		do (swap8 i       (+ i 7))
		(swap8 (+ i 1) (+ i 6))
		(swap8 (+ i 2) (+ i 5))
		(swap8 (+ i 3) (+ i 4))))
	  (2 (loop with sap = (sys:vector-sap vector)
		for i fixnum from (* 2 start) below (* 2 end) by 2
		do (rotatef (sys:sap-ref-16 sap i) (sys:sap-ref-16
						    sap (+ i 1)))))
	  ;; Not sure that swap-endians-456
	  ((4 5 6) (swap-endians-456 data offset-start offset-end
				     endian-swap))
	  ;;otherwise, do nothing ???
	  )))))

(deftype simple-numeric-vector ()
  `(or (simple-array (unsigned-byte 8) (*))
       (simple-array (signed-byte 8) (*))
       (simple-array (unsigned-byte 16) (*))
       (simple-array (signed-byte 16) (*))
       (simple-array (unsigned-byte 32) (*))
       (simple-array (signed-byte 32) (*))
       (simple-array (unsigned-byte *) (*))
       (simple-array (signed-byte *) (*))
       (simple-array single-float (*))	; not previously supported by read-sequence
       (simple-array double-float (*))	; not previously supported by read-sequence
       ))

;;; New versions of READ-VECTOR and WRITE-VECTOR that deal with octet positions
;;; rather than element-positions, for compatibility with Allegro.

;;; WARNING: START and END must be a multiple of octets-per-element.
;;; (Should we enforce this constraint?)
;;; WARNING: Element-types
;;; smaller than 8-bits are not supported.

;;; READ-VECTOR --
(defun read-vector (vector stream &key (start 0) end (endian-swap :byte-8))
  "Read from Stream into Vector.  The Start and End indices of Vector
  is in octets, and must be an multiple of the octets per element of
  the vector element.  The keyword argument :Endian-Swap specifies any
  endian swapping to be done. "
  (declare (type vector vector)
	   (type stream stream)
	   (type unsigned-byte start)	; a list does not have a limit
	   (type (or null unsigned-byte) end)
	   (values unsigned-byte))
  ;;(declare (optimize (speed 3)(safety 0)))
  ;; START and END are octet offsets, not vector indices! [Except for strings]
  ;; Return value is index of next octet to be read into (i.e., start+count)

  (unless (typep vector '(or string simple-numeric-vector))
    (error "Wrong vector type ~a for read-vector on stream ~a." (type-of vector) stream))
  (let* ((octets-per-element (vector-elt-width vector))
	 ;; read-sequence is parameterized by element position.
	 (next-index (read-sequence vector stream
				    :start (floor (or start 0) octets-per-element)
				    :end (and end (floor end octets-per-element)))))
    (declare (fixnum octets-per-element next-index))
    (endian-swap-vector vector (or start 0) next-index
			(endian-swap-value vector endian-swap))
    (* next-index octets-per-element)))

;;; WRITE VECTOR --
;;; returns the next octet-position in vector.
(defun write-vector (vector stream &key (start 0) (end nil) (endian-swap :byte-8))
  "Write Vector to Stream.  The Start and End indices of Vector is in
  octets, and must be an multiple of the octets per element of the
  vector element.  The keyword argument :Endian-Swap specifies any
  endian swapping to be done. "
  (declare (type vector vector)
	   (type stream stream)
	   (type unsigned-byte start)	; a list does not have a limit
	   (type (or null unsigned-byte) end)
	   (values unsigned-byte))

  (let* ((octets-per-element (vector-elt-width vector))
	 (end (or end (* octets-per-element (length vector))))
	 (swap-mask (endian-swap-value vector endian-swap))
	 (next-index end))
    (declare (type fixnum swap-mask end octets-per-element next-index))
    (cond ((= swap-mask 0)
	   (write-sequence vector stream
			   :start (floor start octets-per-element)
			   :end (and end (floor end octets-per-element))))

	  (t
	   ;; In a multiprocessing situation, WITHOUT-INTERRUPTS might be required here
	   ;; otherwise the vector could be seen by another process in the modified state.
	   (unless (typep vector '(or string simple-numeric-vector))
	     (error "Wrong vector type ~a for write-vector on stream ~a." (type-of vector)
		    stream))
	   (let ((start-idx (floor start octets-per-element))
		 (end-idx (floor end octets-per-element)))
	     (endian-swap-vector vector start-idx end-idx swap-mask)
	     (unwind-protect
		  (write-sequence vector stream
				  :start start-idx
				  :end end-idx)
	       (endian-swap-vector vector start-idx end-idx swap-mask)))
	   vector))
    (* next-index octets-per-element)))


(declaim (end-block)) ; READ-VECTOR WRITE-VECTOR block
