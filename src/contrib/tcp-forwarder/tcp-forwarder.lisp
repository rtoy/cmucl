;;; tcp-forwarder.lisp --- forward TCP connnections using SERVE-EVENT
;;;
;;; Author: Eric Marsden <emarsden@mail.dotcom.fr>
;;; Time-stamp: <2001-04-12 emarsden>
;;
;;
;; This program is a port forwarder, or redirector. It redirects TCP
;; connections to another port on another machine. The program can
;; handle multiple concurrent connections.
;;
;; Note the use of CMUCL's SERVE-EVENT facility to handle several
;; concurrent connections without using any multithreading.

(defpackage :cmu-tcp-forwarder
  (:use "COMMON-LISP" "UNIX"))

(in-package :cmu-tcp-forwarder)

(defparameter +target-host+ "poulenc")
(defparameter +target-port+ 8002)
(defparameter +source-port+ 2077)

(defvar *read-buffer* (make-string 1024))

(defvar *forwarded-fds* (make-hash-table))


(defun input-handler (from-fd)
  (declare (type integer from-fd))
  (let ((to-fd (gethash from-fd *forwarded-fds*)))
    (unless to-fd
      (format *debug-io* "Not a forwarded descriptor: ~d~%" from-fd)
      (throw 'forwarder-loop nil))
    (multiple-value-bind (count err)
        (unix-read from-fd (sys:vector-sap *read-buffer*) 100)
     (when (or (null count) (zerop count))
       (unless count
         (format *debug-io* "Error reading from file descriptor ~d: ~a"
                 from-fd (get-unix-error-msg err)))
       (unix-close from-fd)
       (unix-close to-fd)
       (sys:invalidate-descriptor from-fd)
       (sys:invalidate-descriptor to-fd)
       (remhash from-fd *forwarded-fds*)
       (remhash to-fd *forwarded-fds*)
       (return-from input-handler))
     (unix-write to-fd (sys:vector-sap *read-buffer*) 0 count))))

(defun accept-handler (fd)
  (let ((to-fd (ext:connect-to-inet-socket +target-host+ +target-port+))
        (from-fd (ext:accept-tcp-connection fd)))
    (setf (gethash to-fd *forwarded-fds*) from-fd)
    (setf (gethash from-fd *forwarded-fds*) to-fd)
    (sys:add-fd-handler from-fd :input #'input-handler)
    (sys:add-fd-handler to-fd :input #'input-handler)))
 
(defun forward ()
  (system:default-interrupt SIGPIPE)
  (let ((fd (ext:create-inet-listener +source-port+)))
    (sys:add-fd-handler fd :input #'accept-handler)
    (loop (catch 'forwarder-loop (sys:serve-all-events 1)))))

;; EOF
