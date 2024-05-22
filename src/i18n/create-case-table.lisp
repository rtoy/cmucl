(defun build-case-table (&optional (stage2-size 6))
  (let ((table (make-array (ash 1 (- 16 stage2-size)))))
    (dotimes (i (length table))
      (setf (aref table i) (make-array (ash 1 stage2-size)
                                       :initial-element 0)))
    (dotimes (i #xFFFF)
      ;; Compute mapping from lower case to upper case.  The offset is
      ;; stored in the low 16 bits of the stage2 table.
      (when (and (/= i (lisp::unicode-upper i))
		 (= i (lisp::unicode-lower (lisp::unicode-upper i))))
	  (let ((stage1 (ldb (byte (- 16 stage2-size) stage2-size) i))
		(stage2 (ldb (byte stage2-size 0) i))
		(delta (ldb (byte 16 0) (- i (lisp::unicode-upper i)))))
	    (setf (aref (aref table stage1) stage2) delta)))
      ;; Compute mapping from upper case to lower case.  The offset is
      ;; stored in the high 16 bits ofthe stage2 table.
      (when (and (/= i (lisp::unicode-lower i))
		 (= i (lisp::unicode-upper (lisp::unicode-lower i))))
	(let ((stage1 (ldb (byte (- 16 stage2-size) stage2-size) i))
	      (stage2 (ldb (byte stage2-size 0) i))
	      (delta (ldb (byte 16 0) (- i (lisp::unicode-lower i)))))
	  (setf (aref (aref table stage1) stage2) (ash delta 16)))))
    (let ((empty (count-if #'(lambda (x) (every #'zerop x)) table)))
      (format t "~D non-empty ~D empty~%" (- (length table) empty) empty)
      (dotimes (k (length table))
        (let ((empty (count-if-not #'zerop (aref table k))))
          (when (zerop empty)
            (setf (aref table k) nil))
          #+nil
          (format t "~3D: ~D: ~A~%" k empty (aref table k))))
      (let ((stage2 (loop for v across table
                          when v
                            sum (length v))))
        (format t "stage1 entries:  ~D~%" (length table))
        (format t "stage2 entries:  ~D (length ~D)~%"
                stage2 (ash 1 stage2-size))
        (format t "total         :  ~D~%" (+ (length table) stage2)))
      table)))

(defun print-table (k table stream)
  (format stream "const uint32_t stage2_~D [~D] = {~%" k (length table))
  (pprint-logical-block (stream nil :prefix "    ")
    (dotimes (n (length table))
      (unless (zerop n)
        (write-char #\, stream)
        (write-char #\space stream)
        (pprint-newline :fill stream))
      (pprint-pop)
      (format stream "0x~8,'0x" (aref table n))))
  (format stream "~%};~%"))

(defun dump-case-table (pathname table)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (format stream "#include <stdint.h>~%")
    (format stream "#include <stddef.h>~2%")
    ;; First dump each stage2 table
    (loop for k from 0
          for s2 across table
          when s2
            do (print-table k s2 stream))
    ;; Now dump the stage1 table
    (format stream "~2%const uint32_t (*case_table[~D])[~D] = {~%"
            (length table)
            (length (aref table (position-if-not #'null table))))
    (loop for s2 across table
          for k from 0
          if s2
            do (format stream "    &stage2_~D,~%" k)
          else
            do (format stream "    NULL,~%"))
    (format stream "};~%")))
