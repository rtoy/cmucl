;;; -*- Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/stream-vector-io.lisp,v 1.3.6.9 2009/06/04 12:50:02 rtoy Exp $")
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
    (when (>= endian-swap (vector-elt-width vector))
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
	  (-1
	   ;; Swap nibbles
	   ;; NOTE:  start and end are in terms of elements (4 bits)  but we want octets in this loop.
	   (let ((start-octet (truncate start 2))
		 (end-octet (truncate end 2)))
	     (loop for i fixnum from start-octet below end-octet
		   do
		   (let ((x (bref data i)))
		     (setf (bref data i) (logior (ash (logand x #x0f) 4)
						 (ash (logand x #xf0) -4)))))))
	  (-2
	   ;; Swap pairs of bits.
	   (let ((start-octet (truncate start 4))
		 (end-octet (truncate end 4)))
	     (loop for i fixnum from start-octet below end-octet
		   do
		   (let ((x (bref data i)))
		     (declare (type (unsigned-byte 8) x))
		     (setf x (logior (ash (logand x #x33) 2)
				     (ash (logand x #xcc) -2)))
		     (setf x (logior (ash (logand x #x0f) 4)
				     (ash (logand x #xf0) -4)))
		     (setf (bref data i) x)))))
	  (-8
	   ;; Swap bits
	   (let ((start-octet (truncate start 8))
		 (end-octet (truncate end 8)))
	     (loop for i fixnum from start-octet below end-octet
		   do
		   (let ((x (bref data i)))
		     (declare (type (unsigned-byte 8) x))
		     (setf x (logior (ash (logand x #x55) 1)
				     (ash (logand x #xaa) -1)))
		     (setf x (logior (ash (logand x #x33) 2)
				     (ash (logand x #xcc) -2)))
		     (setf x (logior (ash (logand x #x0f) 4)
				     (ash (logand x #xf0) -4)))
		     (setf (bref data i) x)))))
	  ;;otherwise, do nothing ???
	  )))))

(deftype simple-numeric-vector ()
  `(or (simple-array bit (*))
       (simple-array (unsigned-byte 2) (*))
       (simple-array (unsigned-byte 4) (*))
       (simple-array (unsigned-byte 8) (*))
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

;; Read from stream into vector.  Start and End are byte offsets into
;; the vector.
(defun read-vector* (vector stream start end)
  (labels ((get-n-bytes (stream data offset numbytes)
	       ;; Handle case of read-n-bytes reading short.
	       (let ((need numbytes))
		 (loop
		     (let ((n (read-n-bytes stream data offset need nil)))
		       (decf need n)
		       (cond ((or (zerop need) ; Complete
				  (zerop n)) ; EOF
			      (return (- numbytes need)))
			     (t (incf offset n)))))))
	     (read-n-x8-bytes (stream data offset-start offset-end byte-size)
	       (let* ((x8-mult (truncate byte-size 8))
		      (numbytes (* (- offset-end offset-start) x8-mult))
		      (bytes-read (get-n-bytes
				   stream
				   data
				   offset-start
				   numbytes)))
		 ;; A check should probably be made here in order to
		 ;; be sure that we actually read the right amount
		 ;; of bytes. (I.e. (truncate bytes-read x8-mult)
		 ;; should return a 0 second value.
		 (if (< bytes-read numbytes)
		     (+ offset-start (truncate bytes-read x8-mult))
		     offset-end))))
    (read-n-x8-bytes stream vector start end 8)))

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
	 (start-elt (truncate start octets-per-element))
	 (end-octet (or end (ceiling (* (length vector) octets-per-element))))
	 (end-elt  (if end
		       (truncate end octets-per-element)
		       (length vector)))
	 (next-index (read-vector* vector stream
				   start
				   end-octet)))
    (endian-swap-vector vector start-elt end-elt
			(endian-swap-value vector endian-swap))
    next-index))

;; Write vector into stream.  Start and End are byte offsets into the
;; vector.
(defun write-vector* (vector stream start end)
  (flet ((write-n-x8-bytes (stream data start end byte-size)
	   (let ((x8-mult (truncate byte-size 8)))
	     (system:output-raw-bytes stream data
				      (* x8-mult start)
				      (* x8-mult end)))))
    (write-n-x8-bytes stream vector start end 8)))

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
	 (start-elt (truncate start octets-per-element))
	 (end-octet (or end (ceiling (* (length vector) octets-per-element))))
	 (end-elt (if end
		      (truncate end octets-per-element)
		      (length vector)))
	 (swap-mask (endian-swap-value vector endian-swap))
	 (next-index end-octet))
    (declare (type fixnum swap-mask next-index))
    (cond ((= swap-mask 0)
	   (write-vector* vector stream start end-octet))
	  (t
	   ;; In a multiprocessing situation, WITHOUT-INTERRUPTS might be required here
	   ;; otherwise the vector could be seen by another process in the modified state.
	   (unless (typep vector '(or string simple-numeric-vector))
	     (error "Wrong vector type ~a for write-vector on stream ~a." (type-of vector)
		    stream))
	   (endian-swap-vector vector start-elt end-elt swap-mask)
	   (unwind-protect
		(write-vector* vector stream start end-octet)
	     (endian-swap-vector vector start-elt end-elt swap-mask))
	   vector))
    next-index))


(declaim (end-block)) ; READ-VECTOR WRITE-VECTOR block
