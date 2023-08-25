;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: stream.lisp,v 1.7 2004/05/26 07:57:52 yuji Exp $
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

(defun terpri (&optional output-stream)
 (write-char #\Newline output-stream)
 nil)

(defun read-line (&optional input-stream eof-error-p eof-value recursive-p)
  (let ((str (make-array 160
                         :element-type 'character
                         :fill-pointer 0
                         :adjustable t))
        char)
    (loop
     (setq char (read-char input-stream eof-error-p eof-value recursive-p))
     (when (eql char eof-value)
       (return (values str t)))
     (vector-push-extend char str)
     (when (char= char #\Newline)
       (return (values str nil))))))


(defun write-string (string &optional output-stream &key (start 0) end)
  (unless end (setq end (length string)))
  (do ((i start (1+ i)))
      ((= i end) string)
    (write-char (char string i) output-stream)))

(defun write-line (string &optional output-stream &key (start 0) end)
  (write-string string output-stream :start start :end end)
  (write-char #\Newline output-stream)
  string)

(defun read-sequence (sequence stream &key (start 0) end)
  (let ((pos start)
        (read-element (if (subtypep (stream-element-type stream) 'character)
                          #'read-char
                        #'read-byte))
        new)
    (do-subsequence (element sequence start end nil pos)
      (unless (setq new (funcall read-element stream))
        (return pos))
      (setf element new)
      (incf pos))))


(defun write-sequence (sequence stream &key (start 0) end)
  (let ((write-element (if (subtypep (stream-element-type stream) 'character)
                           #'write-char
                         #'write-byte)))
    (do-subsequence (element sequence start end)
       (funcall write-element element stream))
    sequence))


(defmacro with-open-file ((stream filespec &rest options) &body body)
  (let ((abortp (gensym)))
    (multiple-value-bind (decls forms) (declarations-and-forms body)
      `(let ((,var (open filespec ,@options))
             (,abortp t))
         ,@decls
         (unwind-protect
             (multiple-value-prog1
              (progn ,@forms)
              (setq ,abortp nil))
           (when ,var
             (close ,var :abort ,abortp)))))))

(defmacro with-open-stream ((var stream) &body body)
  (let ((abortp (gensym)))
    (multiple-value-bind (decls forms) (declarations-and-forms body)
      `(let ((,var ,stream)
             (,abortp t))
         ,@decls
         (unwind-protect
             (multiple-value-prog1
              (progn ,@forms)
              (setq ,abortp nil))
           (when ,var
             (close ,var :abort ,abortp)))))))

