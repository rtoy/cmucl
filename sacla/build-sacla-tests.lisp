(defun get-sacla-test-files ()
  (let ((test-files
         ;; We only want files named "must-*.lisp", using a cmucl
         ;; pathname extension.
         (directory "lisp/tests/must-*.lisp")))
    test-files))

(defun process-one-file (file)
  (let* ((base-name (pathname-name file))
         (output-file (make-pathname :name (pathname-name file)
                                     :type (pathname-type file)))
         (test-base-name (symbolicate "SACLA-" (string-upcase base-name)))
         (*print-case* :downcase))
  (with-open-file (output output-file
                          :direction :output
                          :if-exists :supersede)
    (format *trace-output* "load-truename ~S~%" *load-truename*)
    (format *trace-output* "Reading ~S~%" file)
    (format *trace-output* "Writing ~S~%" output-file)
    (format output "(in-package #:sacla-lisp-unit)~%")
    (with-open-file (input file)
      (loop for count from 1
            for sexp = (read input nil)
            while sexp
            do (progn
                 (write `(define-test ,(symbolicate test-base-name (format nil ".~d" count))
                           (:tag ,(intern (string test-base-name) :keyword))
                           (assert-true ,sexp))
                        :stream output
                        :circle t
                        :level nil
                        :length nil)
                 (terpri output))))
      (terpri output))))

(defun create-lisp-unit-sacla-tests ()
  (let ((test-files (get-sacla-test-files)))
    ;; For each test file, convert the file to a lisp-unit test file
    ;; by wrapping each sexp in a define-test form.
    (dolist (file test-files)
      (process-one-file file))))
