;; Creates a table of tables that maps a lower case letter to an upper
;; case letter or an upper case letter to a lower case letter.  This
;; mapping only works if the roundtrip casing returns the original
;; character, as required by the standard.
;;
;; STAGE2-SIZE is the number of bits to used for the index of the
;; second stage table.
;;
;; Let C be a 16-bit character code.  C is decomposed into two parts.
;; The high bits are used as the index into the first table, and the
;; low bits are used as the index into the second table.  The number
;; of low bits is STAGE2-SIZE.
;;
;; If the second stage table is all zeroes, the table is replaced by
;; NIL since it contains no valid mapping of lower or upper case
;; letters.
;;
;; Each element of this table is 32-bits long.  The low 16 bits
;; contains the mapping of C to the corresponding upper case letter.
;; The high 16 bits maps C to the corresponding lower case letter.
(defun compute-case-mapping-table (stage2-size)
  (let ((table (make-array (ash 1 (- 16 stage2-size)))))
    (dotimes (i (length table))
      (setf (aref table i) (make-array (ash 1 stage2-size)
                                       :initial-element 0
                                       :element-type '(unsigned-byte 32))))
    (dotimes (i char-code-limit)
      (let ((stage1 (ldb (byte (- 16 stage2-size) stage2-size) i))
	    (stage2 (ldb (byte stage2-size 0) i)))
        (let ((upper (lisp::unicode-upper i))
              (lower (lisp::unicode-lower i))
              (entry 0))
          (declare (type (unsigned-byte 32) entry))

          (assert (< upper char-code-limit))
          (assert (< lower char-code-limit))
          
          ;; Compute mapping from lower case to upper case which is
          ;; stored in the low 16 bits of the stage2 table.
          ;;
          ;; Only consider characters that have an upper case letter and
          ;; whose lowercase version returns the original letter.
          (when (and (/= i upper)
		     (= i (lisp::unicode-lower upper)))
	    (setf entry (ldb (byte 16 0) (- i upper))))
          ;; Compute mapping from upper case to lower case which is
          ;; stored in the high 16 bits ofthe stage2 table.
          ;;
          ;; Only consider characters that have a lower case letter and
          ;; whose upper case version returns the original letter.
          (when (and (/= i lower)
		     (= i (lisp::unicode-upper lower)))
            (setf entry (ash (ldb (byte 16 0) (- i lower))
                             16)))

          ;; Note: the entry can only contain a lower case code or an
          ;; upper case code, not both because we a character is
          ;; either lower case or upper case and not both at the same
          ;; time.
	  (setf (aref (aref table stage1) stage2)
                entry))))

    ;; Find each stage2 table that is all zeroes and replace it with
    ;; NIL.
    (dotimes (k (length table))
      (let ((empty (count-if-not #'zerop (aref table k))))
        (when (zerop empty)
          (setf (aref table k) nil))))
    table))

;; Given a case-mapping table TABLE, print some information about the
;; size of the tables.  This includes the number of empty and
;; non-empty stage2 tables.  Also print out how many total non-NIL
;; entries are needed.  This is proportional to the total amount of
;; memory needed to store all the tables.
(defun print-table-stats (table stage2-size)
  (let ((stage1-size (length table))
        (stage2 (loop for v across table
                      when v
                        sum (length v)))
        (empty (count-if #'null table)))
    (format t "stage2-size ~D~%" stage2-size)
    (format t "  stage1 entries:  ~D: " stage1-size)
    (format t "  ~D non-empty ~D empty~%" (- stage1-size empty) empty)
    (format t "  stage2 entries:  ~D (length ~D)~%"
            stage2 (ash 1 stage2-size))
    (format t "  total         :  ~D~%" (+ (length table) stage2))
    (+ (length table) stage2)))

(defun find-optimum-size ()
  (let ((results
          (first
           (sort (loop for stage2-size from 1 to 15
                       collect (list stage2-size
                                     (print-table-stats
                                      (compute-case-mapping-table stage2-size)
                                      stage2-size)))
                 #'<
                 :key #'second))))
    (format t "Optimum table size:  stage2-size ~D, space ~D~%"
            (first results)
            (second results))))

;; Neatly print the K'th stage2 table TABLE to STREAM.  Each table is
;; named "stage2_k".
(defun print-table (table-name table stream)
  (format stream "~%const uint32_t ~A[~D] = {~%"
          table-name
          (length table))
  ;; Neatly wrap the entries.
  (pprint-logical-block (stream nil :prefix "    ")
    (dotimes (n (length table))
      (unless (zerop n)
        (write-char #\, stream)
        (write-char #\space stream)
        (pprint-newline :fill stream))
      (pprint-pop)
      (format stream "0x~8,'0x" (aref table n))))
  (format stream "~%};~%"))

;; Print the case table TABLE to a file named by PATHNAME.
(defun dump-case-mapping-table (pathname table stage2-size)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (format stream 
            "~
/*
 * DO NOT EDIT.
 *
 * This was generated by (BUILD-CASE-MAPPING-TABLE :STAGE2-SIZE ~D) in
 * src/tools/create-case-mapping.lisp.
 */~2%"
            stage2-size)
    (format stream "#include <stdint.h>~%")
    (format stream "#include <stddef.h>~%")
    ;; First, dump the all-zeroes table
    (print-table "stage2_zeroes"
                 (make-array (ash 1 stage2-size) :initial-element 0)
                 stream)
    ;; Second, dump each stage2 table
    (loop for k from 0
          for s2 across table
          if s2
            do (print-table (format nil "stage2_~D" k)
                            s2
                            stream))
    ;; Now dump the stage1 table
    (format stream "~2%const uint32_t (*case_mapping[~D])[~D] = {~%"
            (length table)
            (length (aref table (position-if-not #'null table))))
    (loop for s2 across table
          for k from 0
          if s2
            do (format stream "    &stage2_~D,~%" k)
          else
            do (format stream "    &stage2_zeroes,~%"))
    (format stream "};~%")
    (format t "Wrote ~S~%" (namestring stream))))

(defun build-case-mapping-table (&key (stage2-size 6) (pathname "./src/lisp/case-mapping.c"))
  (let ((table (compute-case-mapping-table stage2-size)))
    (dump-case-mapping-table pathname table stage2-size)))