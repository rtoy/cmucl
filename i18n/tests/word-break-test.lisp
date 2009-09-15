;; Simple test routine to read and parse the word-break test file and
;; run the individual tests mentioned therein.

(in-package "CL-USER")
(defvar *word-break-test* "target:i18n/tests/WordBreakTest.txt")


(defun parse-line (line)
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
		  (vector-push-extend (code-char c) string)
		  (let ((c (read s)))
		    (handle-break c))
		  (incf count)))))
	(values string breaks)))))

(defun do-test (string breaks)
  (let ((posn -1)
	(count 0)
	(string (coerce string 'simple-string)))
    (loop
	(let ((end (lisp::string-next-word-break string posn)))
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
		     

(defun run-word-break-test (&optional (file *word-break-test*))
  (let ((count 0)
	(failed 0))
    (format t "Run WordBreakTest~%")
    (with-open-file (s file :direction :input :external-format :utf8)
      (loop for line = (read-line s nil nil)
	    while line
	    do
	    (progn
	      (multiple-value-bind (s b)
		  (parse-line line)
		(when s
		  (incf count)
		  (incf failed (if (do-test s b) 0 1)))))))
    (format t "~D out of ~D tests failed.  (~,2F% success)~%"
	    failed count (* 100.0 (- 1 (/ failed count))))
    ;; Here is one additional test to see if we are properly handling
    ;; surrogate pairs.  The string is "A?B cd4", where ? is the
    ;; codepoint U+1000B: "LINEAR B SYLLABLE B046 JE".  This has word
    ;; break property Aletter, so string-capitalize should not put a
    ;; break there.  The result should be "A?b Cd4".
    (let ((s (map 'string #'code-char
		  '(97 #xd800 #xdc0b 66 32 99 100 52))))
      (assert (string= (string-capitalize s :unicode-word-break t)
		       (map 'string #'code-char '(65 55296 56331 98 32 67 100 52)))))))
