(in-package :cl-haml)

(defun read-haml-insert-line (stream &optional (eof-error-p nil)
                                               (eof-value +eof+))
  (list +haml+
        (read-contents stream
                       (read-options stream
                                     eof-error-p
                                     eof-value)
                       eof-error-p
                       eof-value)))
