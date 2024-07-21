(defpackage :unicode-tests
  (:use :cl :lisp-unit))

(in-package "UNICODE-TESTS")

(defvar *normalization-test* "target:i18n/tests/NormalizationTest.txt")

(defun parse-norm-line (line)
  (declare (string line))
  (cond ((or (char= (aref line 0) #\@)
	     (char= (aref line 0) #\#))
	 ;; Delete lines starting with # or @
	 nil)
	(t
	 ;; Split the line into columns.  There are 5 columns.  Column 6 is a comment.
	 (let* ((p1 (position #\; line))
		(p2 (position #\; line :start (1+ p1)))
		(p3 (position #\; line :start (1+ p2)))
		(p4 (position #\; line :start (1+ p3)))
		(p5 (position #\; line :start (1+ p4))))
	   (labels
	       ((convert (codes)
		  (let ((result nil))
		    (map nil #'(lambda (x)
				 (multiple-value-bind (hi lo)
				     (lisp::surrogates x)
				   (push hi result)
				   (when lo (push lo result))))
			 codes)
		    (coerce (nreverse result) 'simple-string)))
		(read-values (string start end)
		    (with-input-from-string (s string :start start :end end)
		      (let ((*read-base* 16))
			(convert
			 (loop for x = (read s nil nil) while x collect x))))))
	     (values (read-values line 0 p1)
		     (read-values line (1+ p1) p2)
		     (read-values line (1+ p2) p3)
		     (read-values line (1+ p3) p4)
		     (read-values line (1+ p4) p5)))))))

(define-test unicode.nfd
  "Test Unicode NFD normalization"
  (:tag :unicode)
  (with-open-file (testfile *normalization-test* :direction :input :external-format :iso8859-1)
    (loop for line = (read-line testfile nil nil) while line do
      (multiple-value-bind (c1 c2 c3 c4 c5)
	  (parse-norm-line line)
	(when c1
	  (let ((test1 (lisp::string-to-nfd c1))
		(test2 (lisp::string-to-nfd c2))
		(test3 (lisp::string-to-nfd c3))
		(test4 (lisp::string-to-nfd c4))
		(test5 (lisp::string-to-nfd c5)))
	    (assert-equalp (list c3 c3 c3 c5 c5)
			   (list test1 test2 test3 test4 test5))
	    ))))))

(define-test unicode.nfkd
  "Test Unicode NFKD normalization"
  (:tag :unicode)
  (with-open-file (testfile *normalization-test* :direction :input :external-format :utf-8)
      (loop for line = (read-line testfile nil nil) while line do
	   ;;(format t "line = ~S~%" line)
	   (multiple-value-bind (c1 c2 c3 c4 c5)
	       (parse-norm-line line)
	     (when c1
	       (let ((test1 (lisp::string-to-nfkd c1))
		     (test2 (lisp::string-to-nfkd c2))
		     (test3 (lisp::string-to-nfkd c3))
		     (test4 (lisp::string-to-nfkd c4))
		     (test5 (lisp::string-to-nfkd c5)))
		 (assert-equalp (list c5 c5 c5 c5 c5)
				(list test1 test2 test3 test4 test5))))))))

(define-test unicode.nfc
  "Test Unicode NFC normalization"
  (:tag :unicode)
  (with-open-file (testfile *normalization-test* :direction :input :external-format :utf-8)
      (loop for line = (read-line testfile nil nil) while line do
	   (multiple-value-bind (c1 c2 c3 c4 c5)
	       (parse-norm-line line)
	     (when c1
	       (let ((test1 (lisp::string-to-nfc c1))
		     (test2 (lisp::string-to-nfc c2))
		     (test3 (lisp::string-to-nfc c3))
		     (test4 (lisp::string-to-nfc c4))
		     (test5 (lisp::string-to-nfc c5)))
		 (assert-equalp (list c2 c2 c2 c4 c4)
				(list test1 test2 test3 test4 test5))))))))

(define-test unicode-nfkc
  "Test Unicode NFKC normalization"
  (:tag :unicode)
  (with-open-file (testfile *normalization-test* :direction :input :external-format :utf-8)
      (loop for line = (read-line testfile nil nil) while line do
	   (multiple-value-bind (c1 c2 c3 c4 c5)
	       (parse-norm-line line)
	     (when c1
	       (let ((test1 (lisp::string-to-nfkc c1))
		     (test2 (lisp::string-to-nfkc c2))
		     (test3 (lisp::string-to-nfkc c3))
		     (test4 (lisp::string-to-nfkc c4))
		     (test5 (lisp::string-to-nfkc c5)))
		 (assert-equalp (list c4 c4 c4 c4 c4)
				(list test1 test2 test3 test4 test5))))))))

(defvar *word-break-test* "target:i18n/tests/WordBreakTest.txt")

(defun parse-word-break-line (line)
  (let* ((eos (or (position #\# line)
		  (length line))))
    ;; See WordBreakTest.txt for the format of the file.  Basically
    ;; each test is on one line and consists of a marker and a
    ;; codepoint.  The marker indicates whether a break is allowed (a
    ;; division sign for a break, a multiplication sign for no break).
    ;; The test ends with a marker to indicate if a division is
    ;; allowed at the end of the test.  After this marker is a comment
    ;; for the test.
    (when (plusp eos)
      ;; For each non-empty line, read the codepints into a string and
      ;; the markers into a second array.  The second array contains
      ;; indices into the string where a break is allowed.
      (let ((*read-base* 16)
	    (count 0)
	    (breaks (make-array 10 :fill-pointer 0))
	    (string (make-array 10 :fill-pointer 0 :element-type 'character)))
	(with-input-from-string (s line :end eos)
	  (flet ((handle-break (c)
		   (when (eql #\division_sign (char (string c) 0))
		       (vector-push-extend count breaks))))
	    (handle-break (read s))
	    (incf count)
	    (loop 
		(let ((c (read s nil nil)))
		  (unless c
		    (return))
		  ;; Handle codepoints outside the BMP carefully.
		  (if (> c #xffff)
		      (let ((s (lisp::codepoints-string (list c))))
			;; Need to increment the count because of our
			;; UTF-16 encoding of strings.
			(incf count)
			(vector-push-extend (aref s 0) string)
			(vector-push-extend (aref s 1) string))
		      (vector-push-extend (code-char c) string))
		  (let ((c (read s)))
		    (handle-break c))
		  (incf count)))))
	(values string breaks)))))

(defun do-test (string breaks)
  (let ((posn -1)
	(count 0)
	(string (coerce string 'simple-string)))
    (loop
	(let ((end (unicode:string-next-word-break string posn)))
	  (when (= end posn)
	    (return))
	  (unless (= end (aref breaks count))
	    (format t "Test failed!~%")
	    (format t "  String = ~S~%" (map 'list #'char-name string))
	    (format t "  Breaks = ~S~%" breaks)
	    (format t "  Expected ~S but got ~S for break #~D~%" (aref breaks count) end count)
	    (finish-output)
	    (return-from do-test nil))
	  (setf posn end)
	  (incf count)))
    t))

(define-test unicode.word-break
  "Test Unicode word break algorithm"
  (:tag :unicode)
  (with-open-file (s *word-break-test* :direction :input :external-format :utf-8)
    (loop for line = (read-line s nil nil)
	  while line
	  do
	     (progn
	       (multiple-value-bind (s b)
		   (parse-word-break-line line)
		 (when s
		   (flet ((do-test (string)
			    (let ((posn -1)
				  (string (coerce string 'simple-string))
				  (computed-breaks (make-array 10 :fill-pointer 0)))
			      (loop
				(let ((end (unicode:string-next-word-break string posn)))
				  (when (= end posn)
				    (return))
				  (vector-push-extend end computed-breaks)
				  (setf posn end)))
			      computed-breaks)))
		     (assert-equalp b
				    (do-test s)))))))))

(define-test unicode.case-extend
  "Test that Unicode never produces an upper or lower case character
  outside the BMP for a character in the BMP"
  (:tag :unicode)
  ;; For each character code (that isn't a surrogate), find the
  ;; corresponding Unicode upper and lowe case character.  Verify that
  ;; this character is in the BMP.
  (loop for code from 0 below char-code-limit
        unless (lisp::surrogatep code)
          do
             (assert-true (< (lisp::unicode-upper code) char-code-limit)
                          code (lisp::unicode-upper code))
             (assert-true (< (lisp::unicode-lower code) char-code-limit)
                          code (lisp::unicode-upper code))))
             
