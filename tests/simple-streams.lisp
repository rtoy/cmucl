;;;; -*- lisp -*-

(require :simple-streams)

(defpackage simple-streams-tests
  (:use #:common-lisp #:stream #:lisp-unit))


(in-package #:simple-streams-tests)

(defparameter *dumb-string*
  "This file was created by simple-stream-tests.lisp. Nothing to see here, move along.")

(defparameter *test-path*
  (merge-pathnames (make-pathname :name :unspecific :type :unspecific
                                  :version :unspecific)
                   *load-truename*)
  "Directory for temporary test files.")

(defparameter *test-file*
  (merge-pathnames #p"test-data.tmp" *test-path*))

(defparameter *echo-server* "127.0.0.1")

(eval-when (:load-toplevel)
  (ensure-directories-exist *test-path* :verbose t))

;;; Non-destructive functional analog of REMF
(defun remove-key (key list)
  (loop for (current-key val . rest) on list by #'cddr
        until (eql current-key key)
        collect current-key into result
        collect val into result
        finally (return (nconc result rest))))

(defun create-test-file (&key (filename *test-file*) (content *dumb-string*))
  (with-open-file (s filename :direction :output
		     :external-format :latin-1
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (write-sequence content s)))

(defun remove-test-file (&key (filename *test-file*))
  (delete-file filename))

(defmacro with-test-file ((stream file &rest open-arguments
                                  &key (delete-afterwards t)
                                  initial-content
                                  &allow-other-keys)
                          &body body)
  (setq open-arguments (remove-key :delete-afterwards open-arguments))
  (setq open-arguments (remove-key :initial-content open-arguments))
  (if initial-content
      (let ((create-file-stream (gensym)))
        `(progn
           (with-open-file (,create-file-stream ,file :direction :output
						:external-format :latin-1
                                                :if-exists :supersede
                                                :if-does-not-exist :create)
             (write-sequence ,initial-content ,create-file-stream))
           (unwind-protect
                (with-open-file (,stream ,file ,@open-arguments
					 :external-format :latin-1)
                  (progn ,@body))
             ,(when delete-afterwards `(ignore-errors (delete-file ,file))))))
      `(unwind-protect
            (with-open-file (,stream ,file ,@open-arguments
				     :external-format :latin-1)
              (progn ,@body))
         ,(when delete-afterwards `(ignore-errors (delete-file ,file))))))

(define-test create-file-1
  ;; Create a file-simple-stream, write data.
  (assert-eql
   t
   (prog1
       (with-open-stream (s (make-instance 'file-simple-stream
					   :filename *test-file*
					   :direction :output
					   :external-format :latin-1
					   :if-exists :overwrite
					   :if-does-not-exist :create))
	 (string= (write-string *dumb-string* s) *dumb-string*))
     (delete-file *test-file*))))

(define-test create-file-2
  ;; Create a file-simple-stream via :class argument to open, write data.
  (assert-eql
   t
   (with-test-file (s *test-file* :class 'file-simple-stream
				  :direction :output :if-exists :overwrite
				  :if-does-not-exist :create)
     (string= (write-string *dumb-string* s) *dumb-string*))))

(define-test create-read-file-1
  ;; Via file-simple-stream objects, write and then re-read data.
  (assert-eql
   t
   (let ((result t))
     (with-test-file (s *test-file* :class 'file-simple-stream
				    :direction :output :if-exists :overwrite
				    :if-does-not-exist :create :delete-afterwards nil)
       (write-line *dumb-string* s)
       (setf result (and result (string= (write-string *dumb-string* s)
					 *dumb-string*))))

     (with-test-file (s *test-file* :class 'file-simple-stream
				    :direction :input :if-does-not-exist :error)
       ;; Check first line
       (multiple-value-bind (string missing-newline-p)
	   (read-line s)
	 (setf result (and result (string= string *dumb-string*)
			   (not missing-newline-p))))
       ;; Check second line
       (multiple-value-bind (string missing-newline-p)
	   (read-line s)
	 (setf result (and result (string= string *dumb-string*)
			   missing-newline-p))))
     result)))

(define-test create-read-mapped-file-1
  ;; Read data via a mapped-file-simple-stream object.
  (assert-eql
   t
   (let ((result t))
     (with-test-file (s *test-file* :class 'mapped-file-simple-stream
				    :direction :input :if-does-not-exist :error
				    :initial-content *dumb-string*)
       (setf result (and result (string= (read-line s) *dumb-string*))))
     result)))

#+(or)
(define-test write-read-inet
    ;; Open a socket-simple-stream to the echo service and test if we
    ;; get it echoed back.  Obviously fails if the echo service isn't
    ;; enabled.
  (assert-eql
   t
   (with-open-stream (s (make-instance 'socket-simple-stream
				       :remote-host *echo-server*
				       :remote-port 7
				       :direction :io))
     (string= (prog1
		  (write-line "Got it!" s)
		(finish-output s))
	      (read-line s)))))

(define-test write-read-large-sc-1
  ;; Do write and read with more data than the buffer will hold
  ;; (single-channel simple-stream)
  (assert-eql
   t
   (let* ((stream (make-instance 'file-simple-stream
				 :filename *test-file* :direction :output
				 :external-format :latin-1
				 :if-exists :overwrite
				 :if-does-not-exist :create))
	  (content (make-string (1+ (device-buffer-length stream))
				:initial-element #\x)))
     (with-open-stream (s stream)
       (write-string content s))
     (with-test-file (s *test-file* :class 'file-simple-stream
				    :direction :input :if-does-not-exist :error)
       (string= content (read-line s))))))

(define-test write-read-large-sc-2
  (assert-eql
   t
   (let* ((stream (make-instance 'file-simple-stream
				 :filename *test-file* :direction :output
				 :external-format :latin-1
				 :if-exists :overwrite
				 :if-does-not-exist :create))
	  (length (1+ (* 3 (device-buffer-length stream))))
	  (content (make-string length)))
     (dotimes (i (length content))
       (setf (aref content i) (code-char (random 256))))
     (with-open-stream (s stream)
       (write-string content s))
     (with-test-file (s *test-file* :class 'file-simple-stream
				    :direction :input :if-does-not-exist :error)
       (let ((seq (make-string length)))
	 #+nil (read-sequence seq s)
	 #-nil (dotimes (i length)
		 (setf (char seq i) (read-char s)))
	 (string= content seq))))))

(define-test write-read-large-sc-read-seq-2
  (assert-eql
   t
   (let* ((stream (make-instance 'file-simple-stream
				 :filename *test-file* :direction :output
				 :external-format :latin-1
				 :if-exists :overwrite
				 :if-does-not-exist :create))
	  (length (1+ (* 3 (device-buffer-length stream))))
	  (content (make-string length)))
     (dotimes (i (length content))
       (setf (aref content i) (code-char (random 256))))
     (with-open-stream (s stream)
       (write-string content s))
     (with-test-file (s *test-file* :class 'file-simple-stream
				    :direction :input :if-does-not-exist :error)
       (let ((seq (make-string length)))
	 (read-sequence seq s)
	 (string= content seq))))))

(define-test write-read-large-sc-3
  (assert-eql
   t
   (let* ((stream (make-instance 'file-simple-stream
				 :filename *test-file* :direction :output
				 :external-format :latin-1
				 :if-exists :overwrite
				 :if-does-not-exist :create))
	  (length (1+ (* 3 (device-buffer-length stream))))
	  (content (make-array length :element-type '(unsigned-byte 8))))
     (dotimes (i (length content))
       (setf (aref content i) (random 256)))
     (with-open-stream (s stream)
       (write-sequence content s))
     (with-test-file (s *test-file* :class 'file-simple-stream
				    :direction :input :if-does-not-exist :error)
       (let ((seq (make-array length :element-type '(unsigned-byte 8))))
	 #+nil (read-sequence seq s)
	 #-nil (dotimes (i length)
		 (setf (aref seq i) (read-byte s)))
	 (equalp content seq))))))

(define-test write-read-large-sc-read-seq-3
  (assert-eql
   t
   (let* ((stream (make-instance 'file-simple-stream
				 :filename *test-file* :direction :output
				 :external-format :latin-1
				 :if-exists :overwrite
				 :if-does-not-exist :create))
	  (length (1+ (* 3 (device-buffer-length stream))))
	  (content (make-array length :element-type '(unsigned-byte 8))))
     (dotimes (i (length content))
       (setf (aref content i) (random 256)))
     (with-open-stream (s stream)
       (write-sequence content s))
     (with-test-file (s *test-file* :class 'file-simple-stream
				    :direction :input :if-does-not-exist :error)
       (let ((seq (make-array length :element-type '(unsigned-byte 8))))
	 (read-sequence seq s)
	 (equalp content seq))))))

#+(or)
(define-test write-read-large-dc-1
  ;; Do write and read with more data than the buffer will hold
  ;; (dual-channel simple-stream; we only have socket streams atm)
  (assert-eql
   t
   (let* ((stream (make-instance 'socket-simple-stream
                                 :remote-host *echo-server*
                                 :remote-port 7
                                 :direction :io))
          (content (make-string (1+ (device-buffer-length stream))
                                :initial-element #\x)))
     (with-open-stream (s stream)
       (string= (prog1 (write-line content s) (finish-output s))
                (read-line s))))))


(define-test file-position-1
  ;; Test reading of file-position
  (assert-eql
   0
   (with-test-file (s *test-file* :class 'file-simple-stream :direction :input
				  :initial-content *dumb-string*)
     (file-position s))))

(define-test file-position-2
  ;; Test reading of file-position
  (assert-eql
   1
   (with-test-file (s *test-file* :class 'file-simple-stream :direction :input
				  :initial-content *dumb-string*)
     (read-byte s)
     (file-position s))))

(define-test file-position-3
  ;; Test reading of file-position in the presence of unsaved data
  (assert-eql
   1
   (with-test-file (s *test-file* :class 'file-simple-stream
				  :direction :output :if-exists :supersede
				  :if-does-not-exist :create)
     (write-byte 50 s)
     (file-position s))))

(define-test file-position-4
  ;; Test reading of file-position in the presence of unsaved data and
  ;; filled buffer
  (assert-eql
   2
   (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
				  :if-exists :overwrite :if-does-not-exist :create
				  :initial-content *dumb-string*)
     (read-byte s)			; fill buffer
     (write-byte 50 s)			; advance file-position
     (file-position s))))

(define-test file-position-5
  ;; Test file position when opening with :if-exists :append
  (assert-eql
   t
   (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
				  :if-exists :append :if-does-not-exist :create
				  :initial-content *dumb-string*)
     (= (file-length s) (file-position s)))))

(define-test write-read-unflushed-sc-1
  ;; Write something into a single-channel stream and read it back
  ;; without explicitly flushing the buffer in-between
  (assert-eql
   #\x
   (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
				  :if-does-not-exist :create :if-exists :supersede)
     (write-char #\x s)
     (file-position s :start)
     (read-char s))))

(define-test write-read-unflushed-sc-2
  ;; Write something into a single-channel stream, try to read back too much
  (assert-eql
   t
   (handler-case
       (with-test-file (s *test-file* :class 'file-simple-stream
				      :direction :io :if-does-not-exist :create
				      :if-exists :supersede)
	 (write-char #\x s)
	 (file-position s :start)
	 (read-char s)
	 (read-char s)
	 nil)
     (end-of-file () t))))

(define-test write-read-unflushed-sc-3
  ;; Test writing in a buffer filled with previous file contents
  (assert-eql
   t
   (let ((result t))
     (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
				    :if-exists :overwrite :if-does-not-exist :create
				    :initial-content *dumb-string*)
       (setq result (and result (char= (read-char s) (schar *dumb-string* 0))))
       (setq result (and result (= (file-position s) 1)))
       (let ((pos (file-position s)))
	 (write-char #\x s)
	 (file-position s pos)
	 (setq result (and result (char= (read-char s) #\x)))))
     result)))

(define-test write-read-unflushed-sc-4
  ;; Test flushing of buffers
  (assert-equal
   '("XooX" T)
   (progn
     (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
				    :if-exists :overwrite :if-does-not-exist :create
				    :initial-content "Foo"
				    :delete-afterwards nil)
       (read-char s)			; Fill the buffer.
       (file-position s :start)		; Change existing data.
       (write-char #\X s)
       (file-position s :end)		; Extend file.
       (write-char #\X s))
     (with-test-file (s *test-file* :class 'file-simple-stream
				    :direction :input :if-does-not-exist :error)
       (multiple-value-list (read-line s))))))

(define-test write-read-append-sc-1
  ;; Test writing in the middle of a stream opened in append mode
  (assert-equal
   '("XooX" T)
   (progn
     (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
				    :if-exists :append :if-does-not-exist :create
				    :initial-content "Foo"
				    :delete-afterwards nil)
       (file-position s :start)		; Jump to beginning.
       (write-char #\X s)
       (file-position s :end)		; Extend file.
       (write-char #\X s))
     (with-test-file (s *test-file* :class 'file-simple-stream
				    :direction :input :if-does-not-exist :error)
       (multiple-value-list (read-line s))))))

(define-test write-read-mixed-sc-1
  ;; Test read/write-sequence of types string and (unsigned-byte 8)
  (assert-eql
   t
   (let ((uvector (make-array '(10) :element-type '(unsigned-byte 8)
				    :initial-element 64))
	 (svector (make-array '(10) :element-type '(signed-byte 8)
				    :initial-element -1))
	 (result-uvector (make-array '(10) :element-type '(unsigned-byte 8)
					   :initial-element 0))
	 (result-svector (make-array '(10) :element-type '(signed-byte 8)
					   :initial-element 0))
	 (result-string (make-string (length *dumb-string*)
				     :initial-element #\Space)))
     (with-test-file (s *test-file* :class 'file-simple-stream :direction :io
				    :if-exists :overwrite :if-does-not-exist :create
				    :delete-afterwards nil)
       (write-sequence svector s)
       (write-sequence uvector s)
       (write-sequence *dumb-string* s))
     (with-test-file (s *test-file* :class 'file-simple-stream
				    :direction :input :if-does-not-exist :error
				    :delete-afterwards nil)
       (read-sequence result-svector s)
       (read-sequence result-uvector s)
       (read-sequence result-string s))
     (and (string= *dumb-string* result-string)
	  (equalp uvector result-uvector)
	  (equalp svector result-svector)))))

(define-test create-read-mapped-file-read-seq-1
  ;; Read data via a mapped-file-simple-stream object using
  ;; read-sequence.
  (assert-eql
   t
   (let ((result t))
     (with-test-file (s *test-file* :class 'mapped-file-simple-stream
				    :direction :input :if-does-not-exist :error
				    :initial-content *dumb-string*)
       (let ((seq (make-string (length *dumb-string*))))
	 (read-sequence seq s)
	 (setf result (and result (string= seq *dumb-string*)))))
     result)))

(define-test create-read-mapped-file-read-seq-2
  ;; Read data via a mapped-file-simple-stream object using
  ;; read-sequence.
  (assert-eql
   t
   (let ((result t))
     (with-test-file (s *test-file* :class 'mapped-file-simple-stream
				    :direction :input :if-does-not-exist :error
				    :initial-content *dumb-string*)
       (let ((seq (make-string (+ 10 (length *dumb-string*)))))
	 (read-sequence seq s)
	 (setf result (and result
			   (string= seq *dumb-string*
				    :end1 (length *dumb-string*))))))
     result)))


;;; From Lynn Quam, cmucl-imp, 2004-12-04: After doing a READ-VECTOR,
;;; FILE-POSITION returns the wrong value.

(lisp-unit:define-test file-position-6
  ;; Test the file-position is right.
  (assert-equal
   '(100 100 100 100)
   (with-open-file (st1 "/etc/passwd" :class 'stream:file-simple-stream)
     (with-open-file (st2 "/etc/passwd" :element-type '(unsigned-byte 8))
       (let* ((buf1 (make-array 100 :element-type '(unsigned-byte 8)))
	      (buf2 (make-array 100 :element-type '(unsigned-byte 8)))
	      (n1 (stream:read-vector buf1 st1))
	      (n2 (read-sequence buf2 st2)))
	 (list n1 (file-position st1) 
	       n2 (file-position st2)))))))

;;; From Madhu, cmucl-imp, 2006-12-16
(define-test file-position-7
  (assert-equal
   '(0 t)
   (with-open-file (st1 "/etc/passwd" :mapped t :class 'stream:file-simple-stream)
     (let* ((posn1 (file-position st1))
	    (line1 (read-line st1))
	    (posn2 (file-position st1)))
       (list posn1 (= posn2 (1+ (length line1))))))))

(define-test file-position-8
  (assert-equal
   '(0 1)
   (with-open-file (st1 "/etc/passwd" :mapped t :class 'stream:file-simple-stream)
     (let* ((posn1 (file-position st1))
	    (c1 (read-char st1))
	    (posn2 (file-position st1)))
       (list posn1 posn2)))))

;;; Some specific tests for full unicode support.
#+unicode
(define-test unicode-read-1
  ;; Tests if reading unicode surrogates works
  (assert-eql
   t
   (let ((string (map 'string #'code-char '(#xd800 #xdc00))))
     (with-open-file (s *test-file*
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf8)
       (write-string string s))
     (with-open-file (s *test-file* :class 'file-simple-stream
				    :direction :input
				    :if-does-not-exist :error
				    :external-format :utf8)
       (let ((seq (read-line s)))
	 (string= string seq))))))

#+unicode
(define-test unicode-read-large-1
  ;; Tests if reading unicode surrogates works
  (assert-eql
   t
   (let ((string (concatenate 'string
			      (map 'string #'code-char '(#xd800 #xdc00))
			      (make-string 5000 :initial-element #\X))))
     (with-open-file (s *test-file*
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf8)
       (write-string string s))
     (with-open-file (s *test-file* :class 'file-simple-stream
				    :direction :input
				    :if-does-not-exist :error
				    :external-format :utf8)
       (let ((seq (read-line s)))
	 (string= string seq))))))

#+unicode
(define-test unicode-write-1
  ;; Tests if writing unicode surrogates work
  (assert-eql
   t
   (let ((string (map 'string #'code-char '(#xd800 #xdc00))))
     (with-open-file (s *test-file*
			:class 'file-simple-stream
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf8)
       (write-string string s))
     (with-open-file (s *test-file*
			:direction :input
			:if-does-not-exist :error
			:external-format :utf8)
       (let ((seq (read-line s)))
	 (string= string seq))))))

#+unicode
(define-test unicode-write-large-1
  ;; Tests if writing unicode surrogates work
  (assert-eql
   t
   (let ((string (concatenate 'string
			      (map 'string #'code-char '(#xd800 #xdc00))
			      (make-string 5000 :initial-element #\X))))
     (with-open-file (s *test-file*
			:class 'file-simple-stream
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf8)
       (write-string string s))
     (with-open-file (s *test-file*
			:direction :input
			:if-does-not-exist :error
			:external-format :utf8)
       (let ((seq (read-line s)))
	 (string= string seq))))))
