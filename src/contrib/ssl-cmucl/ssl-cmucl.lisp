;; ssl-cmucl.lisp -- SSL streams for CMUCL, using OpenSSL library
;;
;; Author: Eric Marsden <emarsden@laas.fr>
;; Time-stamp: <2003-10-10 emarsden>
;; Version: 0.3
;;
;;     Copyright (C) 2001, 2003  Eric Marsden
;;   
;;     This library is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU Library General Public
;;     License as published by the Free Software Foundation; either
;;     version 2 of the License, or (at your option) any later version.
;;   
;;     This library is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;     Library General Public License for more details.
;;   
;;     You should have received a copy of the GNU Library General Public
;;     License along with this library; if not, write to the Free
;;     Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Please send suggestions and bug reports to <emarsden@laas.fr>
;; The latest version of this package should be available from
;;
;;     <URL:http://purl.org/net/emarsden/home/downloads/>
;;
;;
;;; Overview =============================================================
;;
;; This library provides SSL streams for CMUCL, by making foreign
;; function interface calls to the OpenSSL libraries. The code hooks
;; into the CMUCL fd-stream functionality. Read and write operations
;; are dispatched to SSL_read and SSL_write instead of UNIX:UNIX-READ
;; and UNIX:UNIX-WRITE. The interface is compatible with that in ACL6.
;; Supports SSLv2, SSLv3 and TLSv1. 
;;
;; This code doesn't do any buffering, so there are more FFI calls
;; than necessary. However, the OpenSSL libraries do buffering internally,
;; so this doesn't translate into an excessive number of read() and
;; write() syscalls.
;;
;;
;;; Entry points =============================================================
;;
;; * (MAKE-SSL-CLIENT-STREAM SOCKET)
;;   
;;   Returns an SSL stream for the client socket descriptor SOCKET
;;   (obtained from EXT:CONNECT-TO-INET-SOCKET for example). All reads
;;   and writes to this client stream will be pushed through the
;;   OpenSSL library. You should not make any further use of the
;;   socket descriptor. The SSL connection can be closed using the
;;   standard CLOSE function.
;;
;;
;; * (MAKE-SSL-SERVER-STREAM SOCKET &key CERTIFICATE KEY OTHER-CERTIFICATES)
;;
;;   Returns an SSL stream for the server socket descriptor SOCKET
;;   (obtained from EXT:ACCEPT-TCP-CONNECTION for example). All reads
;;   and writes to this server stream will be pushed through the
;;   OpenSSL library. You should not make any further use of the
;;   socket descriptor. The SSL connection can be closed using the
;;   standard CLOSE function.
;;
;;   CERTIFICATE is the path to a file containing the PEM-encoded
;;   certificate for your server. KEY is the path to the PEM-encoded
;;   key for the server, which must not be associated with a
;;   passphrase. OTHER-CERTIFICATES is currently ignored. 
;;
;;
;; See <URL:http://www.OpenSSL.org/> for more information. The mod_ssl
;; FAQ at <URL:http://www.modssl.org/> contains information on
;; generating your server's certificate and key.
;;
;;
;;
;; TESTING: The client part of this code can be tested against a HTTPS web
;; server or an NNTPS server or an SSL-enabled SMTP server (as per
;; RFC2487) -- see the TEST-HTTPS-CLIENT and TEST-NNTPS-CLIENT
;; functions at the end of this file. You can also use the openssl
;; commandline tool to simulate an HTTPS server -- see the s_server(3)
;; manual page. The server part of the code can be tested using an
;; HTTPS-capable web browser such as Netscape or IE. You can also
;; connect to it using the openssl commandline tool:
;;
;;    openssl s_client -connect hostname:8080
;;
;; I have tested this code with CMUCL 18e on Linux/x86 and SPARC, with
;; OpenSSL v0.9.6 and v0.9.7c. It has been tested in client mode
;; against various HTTPS servers, and against the snews.gmane.org
;; NNTPS service.
;;
;; The server component has been tested with Mozilla, Firebird, Safari
;; and Internet Explorer (tested versions 5.2 and 6.0). It also works
;; with the openssl client program and with libwww-perl. Performance
;; seems reasonable: according to the siege program I get a
;; transaction rate of around 60 requests per second between two 1GHz
;; Pentium III machines running Linux.
;;
;;
;; CREDITS: the conditions and ENSURE-SSL-FUNCALL are by Jochen
;; Schmidt. You can find his more portable SSL bindings (that use Gray
;; Streams functionality to hook into the Lisp streams) at
;; <URL:http://www.dataheaven.de/>.


(declaim (optimize (speed 3) (space 1) (safety 1) (debug 0) (compilation-speed 0)))


(defpackage "SSL"
  (:use :common-lisp :unix :alien :c-call)
  (:export #:make-ssl-client-stream
           #:make-ssl-server-stream))

(in-package :ssl)


;; you may need to adapt this to your site
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun load-libssl ()
    (cond ((probe-file "/usr/lib/libssl.so")
           (sys::load-object-file "libssl.so"))
          ((probe-file "/opt/OpenSSL/lib/libssl.a")
           (ext:load-foreign "/opt/OpenSSL/lib/libssl.a"
                           :libraries '("-lc" "/opt/OpenSSL/lib/libcrypto.a")))
          (t
           (error "Can't find libssl"))))

  (load-libssl))



(defparameter *ssl-global-context* nil)
(defparameter *ssl-global-method* nil)

(defun ssl-initialized-p ()
  (and *ssl-global-context* *ssl-global-method*))


(defconstant +random-entropy+ 256)

(defconstant +ssl-filetype-pem+ 1)
(defconstant +ssl-filetype-asn1+ 2)
(defconstant +ssl-filetype-default+ 3)

(defconstant +ssl-error-none+ 0)
(defconstant +ssl-error-ssl+ 1)
(defconstant +ssl-error-want-read+ 2)
(defconstant +ssl-error-want-write+ 3)
(defconstant +ssl-error-want-x509-lookup+ 4)
(defconstant +ssl-error-syscall+ 5)
(defconstant +ssl-error-zero-return+ 6)
(defconstant +ssl-error-want-connect+ 7)


;; (alien:load-foreign "/opt/OpenSSL/lib/libssl.a"
;;    :libraries '("-u" "SSL_new" "-L" "/opt/OpenSSL/lib" "-lcrypto"))

;; this may be a little tricky to get right: libssl probably depends
;; on libcrypto. Your system's linker may resolve the dependencies
;; automatically, or you may have to do it manually.


(declaim (inline ssl-write ssl-read ssl-connect ssl-accept))


(def-alien-type ssl-method (* integer))
(def-alien-type ssl-ctx (* integer))
(def-alien-type ssl-pointer (* integer))

(def-alien-routine ("SSL_get_version" ssl-get-version) c-string
  (ssl ssl-pointer))
(def-alien-routine ("SSL_load_error_strings" ssl-load-error-strings) void)
(def-alien-routine ("SSL_library_init" ssl-library-init) int)
(def-alien-routine ("SSLv2_client_method" ssl-v2-client-method) ssl-method)
(def-alien-routine ("SSLv23_client_method" ssl-v23-client-method) ssl-method)
(def-alien-routine ("SSLv23_server_method" ssl-v23-server-method) ssl-method)
(def-alien-routine ("SSLv23_method" ssl-v23-method) ssl-method)
(def-alien-routine ("SSLv3_client_method" ssl-v3-client-method) ssl-method)
(def-alien-routine ("SSLv3_server_method" ssl-v3-server-method) ssl-method)
(def-alien-routine ("SSLv3_method" ssl-v3-method) ssl-method)
(def-alien-routine ("TLSv1_client_method" ssl-TLSv1-client-method) ssl-method)
(def-alien-routine ("TLSv1_server_method" ssl-TLSv1-server-method) ssl-method)
(def-alien-routine ("TLSv1_method" ssl-TLSv1-method) ssl-method)

(def-alien-routine ("SSL_CTX_new" ssl-ctx-new) ssl-ctx (method ssl-method))
(def-alien-routine ("SSL_new" ssl-new) ssl-pointer (ctx ssl-ctx))
(def-alien-routine ("SSL_set_fd" ssl-set-fd) int
  (ssl ssl-pointer)
  (fd int))
(def-alien-routine ("SSL_get_error" ssl-get-error) int
  (ssl ssl-pointer)
  (ret int))
(def-alien-routine ("SSL_set_connect_state" ssl-set-connect-state) void (ssl ssl-pointer))
(def-alien-routine ("SSL_set_accept_state" ssl-set-accept-state) void (ssl ssl-pointer))
(def-alien-routine ("SSL_connect" ssl-connect) int (ssl ssl-pointer))
(def-alien-routine ("SSL_accept" ssl-accept) int (ssl ssl-pointer))
(def-alien-routine ("SSL_write" ssl-write) int
  (ssl ssl-pointer)
  (buf (array (unsigned 8)))
  (num int))
(def-alien-routine ("SSL_read" ssl-read) int
  (ssl ssl-pointer)
  (buf (array (unsigned 8)))
  (num int))
(def-alien-routine ("SSL_shutdown" ssh-shutdown) void (ssl ssl-pointer))
(def-alien-routine ("SSL_free" ssl-free) void (ssl ssl-pointer))
(def-alien-routine ("SSL_CTX_free" ssl-ctx-free) void (ctx ssl-ctx))
(def-alien-routine ("RAND_seed" rand-seed) void
  (buf (array (unsigned 8)))
  (num integer))

(def-alien-routine ("ERR_get_error" err-get-error) unsigned-long)
(def-alien-routine ("ERR_error_string" err-error-string) c-string
  (e unsigned-long)
  (buf c-string))

(def-alien-routine ("SSL_set_cipher_list" ssl-set-cipher-list) int
  (ssl ssl-pointer)
  (str c-string))
(def-alien-routine ("SSL_use_RSAPrivateKey_file" ssl-use-rsa-privatekey-file) int
  (ssl ssl-pointer)
  (str c-string)
  (type integer))                       ; either +ssl-filetype-pem+ or +ssl-filetype-asn1+
 (def-alien-routine ("SSL_CTX_use_RSAPrivateKey_file" ssl-ctx-use-rsa-privatekey-file) int
   (ctx ssl-ctx)
   (type integer))
(def-alien-routine ("SSL_use_certificate_file" ssl-use-certificate-file) int
  (ssl ssl-pointer)
  (str c-string)
  (type integer))
(def-alien-routine ("SSL_CTX_load_verify_locations" ssl-ctx-load-verify-locations) int
  (ctx ssl-ctx)
  (CAfile c-string)
  (CApath c-string))
(def-alien-routine ("SSL_CTX_set_client_CA_list" ssl-ctx-set-client-ca-list) void
  (ctx ssl-ctx)
  (list ssl-pointer))
(def-alien-routine ("SSL_load_client_CA_file" ssl-load-client-ca-file) ssl-pointer
  (file c-string))

(def-alien-routine ("SSL_CTX_ctrl" ssl-ctx-ctrl) long
  (ctx ssl-ctx)
  (cmd integer)
  (larg long)
  (parg long))



(define-condition ssl-error (error)
  ())

(define-condition ssl-error/handle (error)
  ((ret :initarg :ret
        :reader ssl-error-ret)
   (handle :initarg :handle
           :reader ssl-error-handle))
  (:report (lambda (condition stream)
             (format stream "Unspecified error ~A on handle ~A" 
                     (ssl-error-ret condition)
                     (ssl-error-handle condition)))))

(define-condition ssl-error-initialize (ssl-error)
  ((reason  :initarg :reason
            :reader ssl-error-reason))
  (:report (lambda (condition stream)
             (format stream "SSL initialization error: ~A"
                     (ssl-error-reason condition))
             (print-ssl-error-queue))))


(define-condition ssl-error-want-something (ssl-error/handle)
  ())

;;;SSL_ERROR_NONE
(define-condition ssl-error-none (ssl-error/handle)
  ()
  (:documentation
   "The TLS/SSL I/O operation completed. This result code is returned if and
    only if ret > 0.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A completed. (ret was ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition)))))

;; SSL_ERROR_ZERO_RETURN
(define-condition ssl-error-zero-return (ssl-error/handle)
  ()
  (:documentation
   "The TLS/SSL connection has been closed. If the protocol version is SSL 3.0
    or TLS 1.0, this result code is returned only if a closure alert has
    occurred in the protocol, i.e. if the connection has been closed cleanly.
    Note that in this case SSL_ERROR_ZERO_RETURN
    does not necessarily indicate that the underlying transport has been
    closed.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL connection on handle ~A has been closed. (ret was ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition)))))

;; SSL_ERROR_WANT_READ
(define-condition ssl-error-want-read (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. If, by then, the underlying BIO has data available for 
    reading (if the result code is SSL_ERROR_WANT_READ) or allows writing data
    (SSL_ERROR_WANT_WRITE), then some TLS/SSL protocol progress will take place,
    i.e. at least part of an TLS/SSL record will be read or written. Note that
    the retry may again lead to a SSL_ERROR_WANT_READ or SSL_ERROR_WANT_WRITE
    condition. There is no fixed upper limit for the number of iterations that
    may be necessary until progress becomes visible at application protocol
    level.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a READ. (ret was ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition)))))

;; SSL_ERROR_WANT_WRITE
(define-condition ssl-error-want-write (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. If, by then, the underlying BIO has data available for 
    reading (if the result code is SSL_ERROR_WANT_READ) or allows writing data
    (SSL_ERROR_WANT_WRITE), then some TLS/SSL protocol progress will take place,
    i.e. at least part of an TLS/SSL record will be read or written. Note that
    the retry may again lead to a SSL_ERROR_WANT_READ or SSL_ERROR_WANT_WRITE
    condition. There is no fixed upper limit for the number of iterations that
    may be necessary until progress becomes visible at application protocol
    level.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a WRITE. (ret was ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition)))))

;; SSL_ERROR_WANT_CONNECT
(define-condition ssl-error-want-connect (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete; the same TLS/SSL I/O function should be
    called again later. The underlying BIO was not connected yet to the peer
    and the call would block in connect()/accept(). The SSL
    function should be called again when the connection is established. These
    messages can only appear with a BIO_s_connect() or
    BIO_s_accept() BIO, respectively. In order to find out, when
    the connection has been successfully established, on many platforms
    select() or poll() for writing on the socket file
    descriptor can be used.")
  (:report (lambda (condition stream)
            (format stream "The TLS/SSL operation on handle ~A did not complete: It wants a connect first. (ret was ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition)))))

;; SSL_ERROR_WANT_X509_LOOKUP
(define-condition ssl-error-want-x509-lookup (ssl-error-want-something)
  ()
  (:documentation
   "The operation did not complete because an application callback set by
    SSL_CTX_set_client_cert_cb() has asked to be called again. The
    TLS/SSL I/O function should be called again later. Details depend on the
    application.")
  (:report (lambda (condition stream)
             (format stream "The TLS/SSL operation on handle ~A did not complete: An application callback wants to be called again. (ret was ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition)))))

;; SSL_ERROR_SYSCALL
(define-condition ssl-error-syscall (ssl-error/handle)
  ((syscall :initarg :syscall))
  (:documentation
   "Some I/O error occurred. The OpenSSL error queue may contain more
    information on the error. If the error queue is empty (i.e. ERR_get_error() returns 0),
    ret can be used to find out more about the error: If ret == 0, an EOF was observed that
    violates the protocol. If ret == -1, the underlying BIO reported an I/O error (for socket
    I/O on Unix systems, consult errno for details).")
  (:report (lambda (condition stream)
             (format stream "Unix errno is ~d~%" unix:unix-errno)
             (print-ssl-error-queue)
             (if (zerop (err-get-error))
                 (case (ssl-error-ret condition)
                   (0 (format stream "An I/O error occurred: An unexpected EOF was observed on handle ~A. (ret was ~A)"
                              (ssl-error-handle condition)
                              (ssl-error-ret condition)))
                   (-1 (format stream "An I/O error occurred in the underlying BIO. (ret was ~A)"
                               (ssl-error-ret condition)))
                   (otherwise (format stream "An I/O error occurred: undocumented reason. (ret was ~A)"
                                      (ssl-error-ret condition))))
                 (format stream "An UNKNOWN I/O error occurred in the underlying BIO. (ret was ~A)"
                         (ssl-error-ret condition))))))

;; SSL_ERROR_SSL
(define-condition ssl-error-ssl (ssl-error/handle)
  ()
  (:documentation
   "A failure in the SSL library occurred, usually a protocol error. The
    OpenSSL error queue contains more information on the error.")
  (:report (lambda (condition stream)
             (format stream "A failure in the SSL library occurred on handle ~A. (ret was ~A)"
                     (ssl-error-handle condition)
                     (ssl-error-ret condition))
             (print-ssl-error-queue))))

(defun print-ssl-error-queue ()
  (format *debug-io* "SSL error queue: ~%")
  (loop :for error-code = (err-get-error)
        :until (zerop error-code) :do
        (format *debug-io* "~a~%"
                (err-error-string error-code nil)))
  (force-output t))


(defun ssl-signal-error (handle syscall error-code original-error)
  (case error-code
    (#.+ssl-error-none+
     (error (make-condition 'ssl-error-none :handle handle :ret error-code)))
    (#.+ssl-error-ssl+
     (error (make-condition 'ssl-error-ssl :handle handle :ret error-code)))
    (#.+ssl-error-want-read+
     (error (make-condition 'ssl-error-want-read :handle handle :ret error-code)))
    (#.+ssl-error-want-write+
     (error (make-condition 'ssl-error-want-write :handle handle :ret error-code)))
    (#.+ssl-error-want-x509-lookup+
     (error (make-condition 'ssl-error-want-x509-lookup :handle handle :ret error-code)))
    (#.+ssl-error-zero-return+
     (error (make-condition 'ssl-error-zero-return :handle handle :ret error-code)))
    (#.+ssl-error-want-connect+
     (error (make-condition 'ssl-error-want-connect :handle handle :ret error-code)))
    (#.+ssl-error-syscall+
     (if (zerop original-error)
         (error (make-condition 'ssl-error-zero-return :handle handle :ret error-code))
         (error (make-condition 'ssl-error-syscall :handle handle 
                                :ret error-code
                                :syscall syscall ))))
    (otherwise
     (error (make-condition 'ssl-error/handle
                            :handle handle
                            :ret error-code)))))


(declaim (inline ensure-ssl-funcall))

(defun ensure-ssl-funcall (handle func sleep-time &rest args)
  (loop :for ret =
        (handler-case
            (let ((ret (apply func args)))
              (if (> ret 0)
                  ret
                  (ssl-signal-error handle func (ssl-get-error handle ret) ret)))
          (ssl-error-want-something (condition)
            (declare (ignore condition))
            (sleep sleep-time)
            -1))
        :when (> ret 0) :return ret))

(defun start-ssl-sockets (&key (force nil)
                          (method 'ssl-v23-method))
  "Load all ciphers and errorstrings. Create a global context"
  (when (or force (not (ssl-initialized-p)))
    (format *debug-io* ";; Initializing SSL...~%")
    (ssl-load-error-strings)
    (ssl-library-init)
    ;; this initialization of random entropy is not necessary on
    ;; Linux, since the OpenSSL library automatically reads from
    ;; /dev/urandom if it exists. On Solaris it is necessary.
    (with-alien ((buf (array (unsigned 8) #.+random-entropy+)))
       (dotimes (i +random-entropy+)
         (setf (deref buf i) (random +random-entropy+)))
       (rand-seed buf +random-entropy+))
    (setf *ssl-global-method* (funcall method))
    (setf *ssl-global-context* (ssl-ctx-new *ssl-global-method*))
    #+debug
    (format *debug-io* "Using OpenSSL version ~A~%"
            (ssl-get-version *ssl-global-method*))
    (ssl-ctx-set-session-cache-mode *ssl-global-context* 3)))  ;; 3 was 31


(defun %print-ssl-stream (ssl-stream stream depth)
  (declare (ignore depth)
           (type stream stream))
  (format stream "#<SSL stream for descriptor ~d>"
          (sys:fd-stream-fd ssl-stream)))

(defun %print-ssl-server-stream (ssl-stream stream depth)
  (declare (ignore depth)
           (type stream stream))
  (format stream "#<SSL server stream on descriptor ~d>"
          (sys:fd-stream-fd ssl-stream)))

(declaim (inline %ssl-stream-in-routine))
(defun %ssl-stream-in-routine (stream eof-error eof-value)
  (declare (type ssl-stream stream))
  (let ((buf (ssl-stream-io-buffer stream)))
    (declare (type (alien:alien (array (alien:unsigned 8) 2048)) buf))
    (handler-case
        (progn
          (ensure-ssl-funcall (ssl-stream-handle stream)
                              #'ssl-read 5.5 
                              (ssl-stream-handle stream)
                              buf 1)
          (code-char (deref buf 0)))
      ;; SSL_read returns 0 on end-of-file
      (ssl-error-zero-return (condition)
        (declare (ignore condition))
        (if eof-error (error 'end-of-file :stream stream) eof-value)))))

(declaim (inline %ssl-stream-out-routine))
(defun %ssl-stream-out-routine (stream char)
  (declare (type ssl-stream stream)
           (type base-char char))
  (let ((buf (ssl-stream-io-buffer stream))
        (handle (ssl-stream-handle stream)))
    (declare (type (alien:alien (array (alien:unsigned 8) 2048)) buf))
    (setf (deref buf 0) (char-code char))
    (ensure-ssl-funcall handle #'ssl-write 0.5
                        handle buf 1))
  char)

(declaim (inline %ssl-stream-sout-routine))

(defun %ssl-stream-sout-routine (stream thing start end)
  (declare (type ssl-stream stream)
           (type fixnum start end)
           (type (simple-array base-char (*)) thing))
  (let ((buf (ssl-stream-io-buffer stream))
        (handle (ssl-stream-handle stream)))
    (declare (type (alien:alien (array (alien:unsigned 8) 2048)) buf))
    (loop :for index :from start :below end
          :for count :from 0
          :do (setf (deref buf count) (char-code (aref thing index)))
          :finally (ensure-ssl-funcall handle #'ssl-write 0.5
                                       handle buf (- end start)))))

;; some of these are bogus, since we don't have any buffering code.
;; However, most of them will not be called.
(defun %ssl-stream-misc-routine (stream operation &optional arg1 arg2)
  (declare (type ssl-stream stream))
  (case operation
    (:listen (lisp::fd-stream-misc-routine stream arg1 arg2))
    ;; really need to handle a buffer for this
    (:unread nil)
    (:close
     (unix:unix-close (sys:fd-stream-fd stream))
     (lisp::set-closed-flame stream))
    (:clear-input nil)
    (:force-output t)
    (:finish-output (sys:serve-all-events 0))
    (:element-type 'character)
    (:interactive-p nil)
    (:line-length 80)
    (:charpos nil)
    (:file-length
     (error 'simple-type-error
            :datum stream
            :expected-type 'file-stream
            :format-control "~s is not a stream association with a file."
            :format-arguments (list stream)))
    (:file-position 0)))


;; see SSL_CTX_set_session_cache_mode(3)
;;
;; <openssl/ssl.h> says SSL_CTRL_SET_SESS_CACHE_MODE = 44
(defun ssl-ctx-set-session-cache-mode (ctx mode)
  (declare (type integer mode))
  (ssl-ctx-ctrl ctx 44 mode 0))

(defstruct (ssl-stream
            (:print-function %print-ssl-stream)
            (:constructor %make-ssl-stream)
            (:include sys:fd-stream
                      (input t)
                      (output t)
                      (out #'%ssl-stream-out-routine)
                      (sout #'%ssl-stream-sout-routine)
                      (in #'%ssl-stream-in-routine)
                      (misc #'%ssl-stream-misc-routine)))
  (handle nil)
  (io-buffer (alien:deref (make-alien (array (unsigned 8) 2048)))
             :type (alien:alien (array (alien:unsigned 8) 2048))))

(defstruct (ssl-server-stream
            (:print-function %print-ssl-server-stream)
            (:constructor %make-ssl-server-stream)
            (:include ssl-stream))
  (certificate nil)
  (key nil)
  (other-certificates (list)))


(defun make-ssl-client-stream (socket &key (method 'ssl-v23-client-method))
  "Returns an SSL stream for the client socket descriptor SOCKET."
  (declare (type unix:unix-fd socket))
  (unless (ssl-initialized-p) (start-ssl-sockets :method method))
  (let ((stream (%make-ssl-stream :fd socket))
        (handle (ssl-new *ssl-global-context*)))
    (setf (ssl-stream-handle stream) handle)
    (ssl-set-fd handle socket)
    (ssl-set-connect-state handle)
    (ensure-ssl-funcall handle #'ssl-connect 0.25 handle)
    stream))

(defun make-ssl-server-stream (socket &key certificate key other-certificates
                               (method 'ssl-v23-server-method))
  "Returns an SSL stream for the server socket descriptor SOCKET.
CERTIFICATE is the path to a file containing the PEM-encoded certificate for your
server. KEY is the path to the PEM-encoded key for the server, which must not be
associated with a passphrase.
OTHER-CERTIFICATES is currently ignored."
  (declare (type unix:unix-fd socket))
  (format *debug-io* "HTTPS connection from ~A~%"
          (ext:ip-string (ext:get-peer-host-and-port socket)))
  (unless (ssl-initialized-p) (start-ssl-sockets :method method))
  (let ((stream (%make-ssl-server-stream :fd socket
                                         :certificate certificate
                                         :key key
                                         :other-certificates other-certificates))
        (handle (ssl-new *ssl-global-context*)))
    (setf (ssl-stream-handle stream) handle)
    (ssl-set-fd handle socket)
    (ssl-set-accept-state handle)
    (when (zerop (ssl-set-cipher-list handle "ALL"))
      (error 'ssl-error-initialize :reason "Can't set SSL cipher list"))
    (when key
      (unless (eql 1 (ssl-use-rsa-privatekey-file handle key +ssl-filetype-pem+))
        (error 'ssl-error-initialize :reason "Can't load RSA private key ~A")))
    (when certificate
      (unless (eql 1 (ssl-use-certificate-file handle certificate +ssl-filetype-pem+))
        (error 'ssl-error-initialize :reason "Can't load certificate ~A" certificate)))
    (ensure-ssl-funcall handle #'ssl-accept 0.25 handle)
    stream))


(defun read-line-crlf (stream &optional eof-error-p eof-value)
  (let* ((l (read-line stream eof-error-p eof-value))
         (last (1- (length l))))
    (when l
      (if (eql #\return (schar l last))
          (subseq l 0 last)
          l))))

(defun test-nntps-client (&optional (host "snews.gmane.org") (port 563))
  (let* ((fd (ext:connect-to-inet-socket host port))
         (nntps (make-ssl-client-stream fd)))
    (format t "NNTPS> ~A~%" (read-line-crlf nntps))
    (write-line "HELP" nntps)
    (force-output nntps)
    (loop :for line = (read-line-crlf nntps nil)
          :until (string-equal "." line)
          :do (format t "NNTPS> ~A~%" line))))


;; open an HTTPS connection to a secure web server and make a
;; HEAD request
(defun test-https-client (host &optional (port 443))
  (let* ((fd (ext:connect-to-inet-socket host port))
         (https (make-ssl-client-stream fd)))
    (format https "HEAD / HTTP/1.0~%Host: ~a~%~%" host)
    (force-output https)
    (loop :for line = (read-line-crlf https nil)
          :while line :do
          (format t "HTTPS> ~a~%" line))))



(defun batch-stop-on-interrupts ()
  "Complement to -batch mode. Don't break into the debugger
on certain signals (such as Ctrl-C) ; quit immediately."
  (setq *debugger-hook*
        (lambda (condition old-debugger-hook)
          (declare (ignore old-debugger-hook))
          (handler-case
           (progn
             (format *error-output*
                     "~@<unhandled condition ~2I~_~A~:>"
                     condition)
             (finish-output *error-output*)
             (debug:backtrace 15))
           (condition () (unix:unix-exit 2)))))
  ;; (system:default-interrupt unix:SIGINT)
  (system:default-interrupt unix:SIGQUIT)
  (system:default-interrupt unix:SIGILL)
  (system:default-interrupt unix:SIGBUS)
  (system:default-interrupt unix:SIGSEGV)
  (system:default-interrupt unix:SIGFPE))


;; start a simple HTTPS server. See the mod_ssl documentation at
;; <URL:http://www.modssl.org/> for information on generating the
;; server certificate and key
;;
;; You can stress-test the server with
;;
;;    siege -c 10 -u https://host:8080/foobar
;;
(defun test-https-server (&optional (port 8080))
  (format t "~&SSL server listening on port ~d~%" port)
  (system:ignore-interrupt unix:SIGPIPE)
  (batch-stop-on-interrupts)
  (let* ((server (ext:create-inet-listener port :stream :reuse-address t)))
      (loop
       (ignore-errors
         (let ((client (make-ssl-server-stream (ext:accept-tcp-connection server)
                                               :certificate "/home/emarsden/tmp/ssl_stuff/server.pem"
                                               :key "/home/emarsden/tmp/ssl_stuff/server.key")))
           (unwind-protect
                (progn
                  (loop :for line = (read-line-crlf client nil)
                        :while (> (length line) 1) :do
                        (format t "HTTPS> ~a~%" line))
                  (format client "HTTP/1.0 200 OK~%")
                  (format client "Server: SSL-CMUCL/1.1~%")
                  (format client "Content-Type: text/plain~%")
                  (terpri client)
                  (format client "G'day at ~A!~%"
                          (ext:format-universal-time nil (get-universal-time) :style :iso8601))
                  (format client "SSL-CMUCL running in ~A ~A~%"
                          (lisp-implementation-type) (lisp-implementation-version)))
             (close client)))))))


(eval-when (:load-toplevel)
  (pushnew :ssl *features*))

;; EOF
