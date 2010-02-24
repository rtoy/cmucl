;; embed.lisp --- Embed C code in Lisp
;;
;; This package provides a macro to embed compiled C code in a Lisp
;; fasl file.  It's inspired by the GForth FFI.  
;; The primary reason to use the C compiler are C macros
;; which can't be converted to Lisp without parsing C.
;; It's much easier to use the C compiler to parse instead
;; of doing it Lisp.
;;
;; Example:
;;
;; (define-c-code (*example*)
;;  "#include <stdio.h>"
;; 
;;  (defstub hello ((msg :string))
;;    "printf(\"Hello, World!\\n%s\", msg);"
;;    "fflush(0);")
;; 
;;  "#include <sys/types.h>"
;;  "#include <sys/wait.h>"
;;  (defstub exitedp ((status :int) => :int)
;;    "return WIFEXITED(status);"))
;;
;; DEFINE-C-CODE invokes the C compiler on the body and embeds the
;; object code as a byte vector in the Lisp code.  At load time, the
;; byte vector is passed to dlopen and callout functions to the C
;; functions are initialized.
;;
;; TODO:
;; - make it possible to embed C code in core files
;;

(defpackage #:embedded-c
  (:use #:cl)
  (:export #:define-c-code #:defstub #:#
	   #:*c-compiler* #:c-compiler-flags*))
(in-package #:embedded-c)

(defvar *c-compiler* "gcc"
  "C compiler to be used")

(defvar *c-compiler-flags*
  #-solaris '("-Wall" "-Werror" "-shared" "-rdynamic")
  #+solaris '("-Wall" "-Werror" "-G")
  "List of flags to be passed to the C compiler")
  
(defmacro define-c-code ((name &key 
			       (cc *c-compiler*)
			       (cflags *c-compiler-flags*))
			 &body body &environment env)
  (multiple-value-bind (c-string lisp) (parse-c-body body env)
    `(progn
       (defparameter ,name
	 (progn
	   (when (boundp ',name)
	     (release-object-code ,name))
	   (load-time-value (load-object-code
			     ',(compile-c-code c-string cc cflags))
			    t)))
       (assert ,name)
       (macrolet ((object-code-variable () ',name))
	 ,@lisp))))

(defun parse-c-body (body env)
  (let (strings forms)
    (dolist (e body)
      (etypecase e
	(string e (push e strings))
	(cons (destructuring-bind (&key (c-code "") lisp) (macroexpand e env)
		(push c-code strings)
		(push lisp forms)))))
    (values (format nil "~{~a~%~}" (reverse strings))
	    (reverse forms))))

(defmacro defstub (name (&rest args) &body body)
  (let* ((tail (member-if (lambda (x) 
			    (or (and (symbolp x) (string-equal x '=>))
				(progn (check-arg-syntax x) nil)))
			  args))
	 (return-type (if tail (cadr tail)))
	 (args (ldiff args tail))
	 (arg-names (mapcar #'car args))
	 (alien-types (mapcar (lambda (arg) 
				(type-spec-alien-type (cadr arg)))
			      args)))
    (list :c-code (with-output-to-string (*standard-output*)
		    (emit-stub name return-type args body))
	  :lisp
	  `(defun ,name ,arg-names
	     (alien::alien-funcall
	      (alien:sap-alien
	       ,(generate-resolve-code name)
	       (function ,(type-spec-alien-type return-type) 
			 ,@alien-types))
	      ,@arg-names)))))

(defun generate-resolve-code (name)
  `(macrolet ((resolve (name &environment env)
		(let ((var (macroexpand '(object-code-variable) env)))
		  (assert (symbolp var))
		  `(load-time-value 
		    (let (#+(or)(*break-on-signals* t))
		      (dlsym-or-lose (object-code-handle ,var)
				     ',(string name)))
		    t))))
     (resolve ,name)))

(defun check-arg-syntax (arg)
  (unless (and (consp arg)
	       (= (length arg) 2))
    (error "Invalid arg syntax: ~s~%Should match (NAME TYPE)." arg)))

(defun emit-stub (name return-type args body)
  (format t "~a ~a "
	  (if return-type (type-spec-c-name return-type) "void")
	  (string name))
  (if args
      (format t "(~{~a ~a~^, ~})"
	      (loop for (name type) in args append
		    (list (type-spec-c-name type)
			  (string-downcase name))))
      (format t "(void)"))
  (format t " {~%~{~a~%~}}~%" body))

(defun type-spec-alien-type (spec)
  (ecase spec
    (:int 'c-call:int)
    (:address 'c-call:unsigned-long)
    (:float 'c-call:float)
    (:string 'c-call:c-string)
    ((nil) 'c-call:void)))

(defun type-spec-c-name (spec)
  (ecase spec
    (:int "int")
    (:address "unsigned long")
    (:float "double")
    (:string "char*")))

#-solaris
(alien:def-alien-routine mkdtemp c-call:c-string (template c-call:c-string))

#+solaris
(alien:def-alien-routine tempnam c-call:c-string
  (dir c-call:c-string)
  (pfx c-call:c-string))

#+solaris
(defun mkdtemp (template)
  ;; This doesn't handle errors if the directory already exists.
  (let ((name (tempnam "/tmp" template)))
    (unix:unix-mkdir name #o700)
    name))

(defun with-tmpdir (template fun)
  (let ((dir (mkdtemp (copy-seq (or template "/tmp/cmucl-embedXXXXXX")))))
    (unless dir 
      (error "~a" (unix:get-unix-error-msg)))
    (setq dir (format nil "~a/" dir))
    (unwind-protect (funcall fun dir)
      (mapc #'delete-file (directory dir))
      (or (unix:unix-rmdir dir)
	  (error "~a" (unix:get-unix-error-msg))))))

(defun compile-c-code (string cc cflags)
  (with-tmpdir nil
    (lambda (dir)
      (let ((cfile (format nil "~a/x.c" dir))
	    (sofile (format nil "~a/x.so" dir)))
	(with-open-file (c cfile :direction :output :if-does-not-exist :create)
	  (write-string string c)
	  (force-output c))
	(run-cc cc `(,@cflags ,cfile "-o" ,sofile))
	(slurp-file sofile)))))

#+(or)
(load-object-code
 (compile-c-code 
  (format nil "~{~a~%~}" 
	  '("#include <stdio.h>"
	    "int main () { "
	    "printf (\"Hello, World\\n\");"
	    "return 0;"
	    "}"))
  "gcc" '("-Wall" #+(or)"-Werror" "-shared" "-rdynamic")))

(defun run-cc (cc args)
  ;;(print 'run-cc)
  (let* ((out (make-string-output-stream))
	 (proc (ext:run-program cc args :output out :error out)))
    (let ((output (get-output-stream-string out))
	  (exit-code (ext:process-exit-code proc)))
      (unless (zerop exit-code)
	(error "Non-zero exit status ~d~%~a" exit-code output))
      (when (plusp (length output))
	(warn "Output from cc:~%~a" output)))))

(defun slurp-file (filename)
  (with-open-file (file filename :element-type '(unsigned-byte 8))
    (let* ((buffer (make-array (file-length file) 
			       :element-type '(unsigned-byte 8)))
	   (count (read-sequence buffer file)))
      (assert (= count (length buffer)))
      buffer)))

(defstruct object-code
  handle filename bytes)

(defun load-object-code (object-code)
  (with-tmpdir nil
    (lambda (dir)
      (let ((filename (format nil "~a/x.so" dir)))
	(with-open-file (file filename :if-does-not-exist :create
			      :direction :output
			      :element-type '(unsigned-byte 8))
	  (write-sequence object-code file)
	  (force-output file))
	(sys::dlerror)
	(let ((handle (sys::dlopen filename sys::rtld-now)))
	  (when (zerop (sys:sap-int handle))
	    (error "~a" (sys::dlerror)))
	  (make-object-code :handle handle :filename filename 
			    :bytes object-code))))))

(alien:def-alien-routine dlclose c-call:int (handle sys:system-area-pointer))

(defun release-object-code (oc)
  (unless (zerop (dlclose (object-code-handle oc)))
    (error "~a" (sys::dlerror))))

(defun dlsym-or-lose (handle name)
  (sys::dlerror)
  (let ((addr (sys::dlsym handle (string name)))
	(msg (sys::dlerror)))
    (when msg
      (error "~a" msg))
    addr))

(define-symbol-macro \# (enable-sharp-bar))

(defun enable-sharp-bar ()
  (let* ((old *readtable*)
	 (new (copy-readtable old))
	 (reader (lambda (stream char subcar)
		   (declare (ignore char subcar))
		   (unwind-protect (read-sharp-string stream #\|)
		     (setq *readtable* old)))))
    (setq *readtable* new)
    (set-dispatch-macro-character #\# #\| reader)
    (values)))

(defun read-sharp-string (stream end-char)
  (with-output-to-string (buffer)
    (loop 
     (let ((ch (read-char stream)))
       (when (and (char= ch end-char)
		  (char= (peek-char nil stream) #\#))
	 (read-char stream)
	 (return))
       (write-char ch buffer)))))

#+(or)
(define-c-code (*wait-helpers*)
 "#include <sys/types.h>"
 "#include <sys/wait.h>"

 (defstub exitedp ((status :int) => :int)
   "return WIFEXITED(status);")

 (defstub coredumpp ((status :int) => :int)
   "return WCOREDUMP(status);"))

(provide "contrib-embedded-c")
