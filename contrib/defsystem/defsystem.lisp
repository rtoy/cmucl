;;; -*- Mode: LISP; Syntax: Common-Lisp -*-
 
;;; ********************************************************************
;;; Portable Mini-DefSystem ********************************************
;;; ********************************************************************
;;; Fri Feb  2 10:24:17 1990 by Mark Kantrowitz <mkant@GS8.SP.CS.CMU.EDU>

;;; This is a portable system definition facility for Common Lisp. 
;;; Though home-grown, the syntax was inspired by fond memories of the
;;; defsystem facility on Symbolics 3600's. The exhaustive lists of
;;; filename extensions for various lisps and the idea to have one
;;; "operate-on-system" function instead of separate "compile-system"
;;; and "load-system" functions were taken from Xerox Corp.'s PCL 
;;; system.

;;; This system improves on both PCL and Symbolics defsystem utilities
;;; by performing a topological sort of the graph of file-dependency 
;;; constraints. Thus, the components of the system need not be listed
;;; in any special order, because the defsystem command reorganizes them
;;; based on their constraints. It includes all the standard bells and
;;; whistles, such as not recompiling a binary file that is up to date
;;; (unless the user specifies that all files should be recompiled).

;;; Written by Mark Kantrowitz, School of Computer Science, 
;;; Carnegie Mellon University, October 1989.

;;; Copyright (c) 1989 by Mark Kantrowitz. All rights reserved.

;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o no fees or compensation are charged for use, copies, or
;;;        access to this software
;;;      o this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance. 

;;; Please send bug reports, comments and suggestions to mkant@cs.cmu.edu. 


;;; ********************************************************************
;;; How to Use this System *********************************************
;;; ********************************************************************

;;; To use this system,
;;; 1. If you want to have a central registry of system definitions, 
;;;    modify the value of the variable *central-registry* below.
;;; 2. Load this file (defsystem.lisp) in either source or compiled form,
;;; 3. Load the file containing the "defsystem" definition of your system,
;;; 4. Use the function "operate-on-system" to do things to your system.

;;; For more information, see the documentation and examples in defsystem.text.

;;; ****************************************************************
;;; Lisp Code ******************************************************
;;; ****************************************************************

;;; ********************************
;;; Set up Package *****************
;;; ********************************

(in-package "DEFSYSTEM" 
	    :nicknames '("DEFSYS" "MAKE")
	    :use '("USER"
		   "LISP")
	    )

(provide 'defsystem)

(defvar *exports*
   '(defsystem operate-on-system oos afs-binary-directory files-in-system))

(defvar *other-exports* 
  '(*central-registry* *bin-subdir* 
     machine-type-translation software-type-translation
					;require
     allegro-make-system-fasl files-which-need-compilation 
     ))

(export (append *exports* *other-exports*))
#|
;(import *exports* "LISP")
;(import *exports* "USER")
|#

#-(OR CMU :CCL)
(import *exports* "USER")
#+(OR CMU :CCL)
(progn
  (import (cdr *exports*) "USER")
  (shadowing-import (car *exports*) "USER"))

#-PCL(when (find-package "PCL") (push :pcl *modules*)(push :pcl *features*))



;;; ********************************
;;; Customizable System Parameters *
;;; ********************************

;;; Change this variable to set up the location of a central
;;; repository for system definitions if you want one.
(defvar *central-registry* "/afs/cs.cmu.edu/project/oz/oz2/system/Registry/" 
  "Central directory of system definitions" )

(defvar *bin-subdir* ".bin/"
  "The subdirectory of an AFS directory where the binaries are really kept.")

;;; These variables set up defaults for operate-on-system, and are used 
;;; for communication in lieu of parameter passing. Yes, this is bad,
;;; but it keeps the interface small. Also, in the case of the -if-no-binary
;;; variables, parameter passing would require multiple value returns
;;; from some functions. Why make life complicated?
(defvar *tell-user-when-done* nil
  "If T, system will print ...DONE at the end of an operation")
(defvar *oos-verbose* nil 
  "Operate on System Verbose Mode")
(defvar *oos-test* nil 
  "Operate on System Test Mode")
(defvar *load-source-if-no-binary* nil
  "If T, system will try loading the source if the binary is missing")
(defvar *bother-user-if-no-binary* t
  "If T, the system will ask the user whether to load the source if the binary is missing")
(defvar *load-source-instead-of-binary* nil
  "If T, the system will load the source file instead of the binary.")

;;; Particular to CMULisp
(defvar *compile-error-file-type* "err"
  "File type of compilation error file in cmulisp")
(defvar *cmu-errors-to-terminal* t
  "Argument to :errors-to-terminal in compile-file in cmulisp")
(defvar *cmu-errors-to-file* t
  "If T, cmulisp will write an error file during compilation")

;;; ********************************
;;; Global Variables ***************
;;; ********************************

;;; Massage people's *features* into better shape.
(eval-when (compile load eval)  
  (dolist (feature *features*)
    (when (and (symbolp feature)   ; 3600
               (equal (symbol-name feature) "CMU"))
      (pushnew :CMU *features*)))
  
  #+Lucid
  (when (search "IBM RT PC" (machine-type))
    (pushnew :ibm-rt-pc *features*))
  )

;;; *filename-extensions* is a cons of the source and binary extensions.
(defvar *filename-extensions*
  (car '(#+(and Symbolics Lispm)              ("lisp" . "bin")
         #+(and dec common vax (not ultrix))  ("LSP"  . "FAS")
         #+(and dec common vax ultrix)        ("lsp"  . "fas")
         #+KCL                                ("lsp"  . "o")
         #+IBCL                               ("lsp"  . "o")
         #+Xerox                              ("lisp" . "dfasl")
         #+(and Lucid MC68000)                ("lisp" . "lbin")
         #+(and Lucid Vax)                    ("lisp" . "vbin")   
         #+(and Lucid Prime)                  ("lisp" . "pbin")
         #+(and Lucid SUNRise)                ("lisp" . "sbin")
         #+(and Lucid SPARC)                  ("lisp" . "sbin")
         #+(and Lucid :IBM-RT-PC)              ("lisp" . "bbin")
         #+excl                               ("cl"   . "fasl")
         #+:CMU                               ("lisp" . "fasl")
	 #+PRIME                              ("lisp" . "pbin")
         #+HP                                 ("l"    . "b")
         #+TI ("lisp" . #.(string (si::local-binary-file-type)))
         #+:gclisp                            ("LSP"  . "F2S")
         #+pyramid                            ("clisp" . "o")
         #+:coral                             ("lisp" . "fasl")
         
         ;; Otherwise,
         ("lisp" . "lbin")))
  "Filename extensions for Common Lisp. A cons of the form
   (Source-Extension . Binary-Extension). If the system is 
   unknown (as in *features* not known), defaults to lisp and lbin.")

;;; There is no real support for this variable being nil, so don't change it.
;;; Note that in any event, the toplevel system (defined with defsystem)
;;; will have its dependencies delayed. Not having dependencies delayed
;;; might be useful if we define several systems within one defsystem.
(defvar *system-dependencies-delayed* t 
  "If T, system dependencies are expanded at run time")

(defun non-empty-listp (list)
  (and list (listp list)))

;;; ********************************
;;; Alist Manipulation *************
;;; ********************************
(defun alist-lookup (name alist &key test test-not key)
  (cdr (assoc name alist :test test :test-not test-not :key key)))

(defmacro set-alist-lookup ((name alist &key test test-not key) value)
  (let ((pair (gensym)))
    `(let ((,pair (assoc ,name ,alist :test ,test :test-not ,test-not :key ,key)))
       (if ,pair
	   (rplacd ,pair ,value)
	 (push (cons ,name ,value) ,alist)))))

(defvar *component-operations* nil
  "Alist of (operation-name function) pairs" )
(defun component-operation (name &optional operation)
  (if operation
      (set-alist-lookup (name *component-operations*) operation)
    (alist-lookup name *component-operations*)))

;;; ********************************
;;; AFS @sys immitator *************
;;; ********************************
(defun afs-binary-directory (root-directory)
  ;; Function for obtaining the directory AFS's @sys feature would have
  ;; chosen when we're not in AFS. This function is useful as the argument
  ;; to :binary-pathname in defsystem.
  (let ((machine (machine-type-translation (machine-type)))
	(software (software-type-translation (software-type))))
    ;; pmax_mach rt_mach sun3_35 sun3_mach vax_mach
    (format nil "~A~@[~A~]~@[~A/~]" 
	    (namestring root-directory)
	    *bin-subdir*
	    (afs-component machine software)
	    )))

(defun afs-component (machine software)
  (format nil "~@[~A~]~@[_~A~]" 
	    machine 
	    (or software "mach")))

(defvar *machine-type-alist* nil
  "Alist for retrieving the machine-type")
(defun machine-type-translation (name &optional operation)
  (if operation
      (set-alist-lookup (name *machine-type-alist* :test #'string-equal) operation)
    (alist-lookup name *machine-type-alist* :test #'string-equal)))

(machine-type-translation "IBM RT PC"   "rt")
(machine-type-translation "DEC 3100"    "pmax")
(machine-type-translation "DEC VAX-11"  "vax")
(machine-type-translation "Sun3"        "sun3")

(defvar *software-type-alist* nil
  "Alist for retrieving the software-type")
(defun software-type-translation (name &optional operation)
  (if operation
      (set-alist-lookup (name *software-type-alist* :test #'string-equal) operation)
    (alist-lookup name *software-type-alist* :test #'string-equal)))

(software-type-translation "BSD UNIX" "mach") ; "unix"
(software-type-translation "Ultrix" "mach") ; "ultrix"
(software-type-translation "SunOS" "SunOS")
(software-type-translation "MACH/4.3BSD" "mach")

;;; ********************************
;;; System Names *******************
;;; ********************************
(defun canonicalize-system-name (name)
  ;; Names of modules or files may be symbols or strings, but
  ;; names of systems must be symbols for accessing the system.
  (if (symbolp name)
      name
      (intern (string-upcase (string name)))))

(defun get-system (name)
  ;; The system definition is stored under the
  ;; 'system property of the system name.
  (get (canonicalize-system-name name) 'system))

(defsetf get-system (name) (value)
  `(setf (get (canonicalize-system-name ,name) 'system) ,value))

;;; ********************************
;;; Directory Pathname Hacking *****
;;; ********************************

;;; There is no CL primitive for tacking a subdirectory onto a directory.
;;; Such a primitive is needed since we use both absolute and relative 
;;; directories. The merge-directories function was a try that failed
;;; (we're keeping it around to prevent future mistakes of that sort),
;;; while the append-directories function seems to work right most
;;; of the time (i.e., it gets the pathname delimiters right).

;;; Unix example: An absolute directory starts with / while a 
;;; relative directory doesn't. A directory ends with /, while
;;; a file's pathname doesn't. This is important 'cause
;;; (pathname-directory "foo/bar") will return "foo" and not "foo/".

(defun append-directories (absolute-directory relative-directory)
  ;; Assumptions: Absolute-directory is a directory, with no
  ;; filename stuck on the end. Relative-directory, however, may
  ;; have a filename stuck on the end.
  (when (or absolute-directory relative-directory)
	#+ExCL(when (and (stringp absolute-directory)
			 (null-string absolute-directory))
		    (setq absolute-directory nil))
	#+CMU(when (pathnamep absolute-directory) 
		   (setq absolute-directory (namestring absolute-directory)))
	#+CMU(when (pathnamep relative-directory) 
		   (setq relative-directory (namestring relative-directory)))
	(namestring (make-pathname :directory absolute-directory 
				   :name relative-directory))))

(defun null-string (s)
  (string-equal s ""))

#|
<cl> (defun d (d n) (namestring (make-pathname :directory d :name n)))

D
<cl> (d "/foo/bar/" "baz/barf.lisp")

"/foo/bar/baz/barf.lisp"
<cl> (d "foo/bar/" "baz/barf.lisp")

"foo/bar/baz/barf.lisp"
<cl> (d "foo/bar" "baz/barf.lisp")

"foo/bar/baz/barf.lisp"
<cl> (d "foo/bar" "/baz/barf.lisp")

"foo/bar//baz/barf.lisp"
<cl> (d "foo/bar" nil)

"foo/bar/"
<cl> (d nil "baz/barf.lisp")

"baz/barf.lisp"
<cl> (d nil nil)

""

|#

#|
(defun merge-directories (absolute-directory relative-directory)
	  ;; replace concatenate with something more intelligent
	  ;; i.e., concatenation won't work with some directories.
	  ;; it should also behave well if the parent directory 
	  ;; has a filename at the end, or if the relative-directory ain't relative
	  (when absolute-directory 
	    (setq absolute-directory (pathname-directory absolute-directory)))
	  (concatenate 'string 
		       (or absolute-directory "")
		       (or relative-directory "")))
|#

(defun namestring-or-nil (pathname)
  (when pathname
    (namestring pathname)))

(defun new-file-type (pathname type)
  (make-pathname
   :host (pathname-host pathname)
   :device (pathname-device pathname)
   :directory (pathname-directory pathname)
   :name (pathname-name pathname)
   :type type
   :version (pathname-version pathname)))

;;; ********************************
;;; Component Defstruct ************
;;; ********************************
(defstruct (topological-sort-node (:conc-name topsort-))
  color
  time)

(defstruct (component (:include topological-sort-node)
                      (:print-function print-component))
  type                ; :system, :module, or :file, or :private-file
  name                ; a symbol or string
  indent              ; number of characters of indent in verbose output to the user.
  host                ; the pathname host (i.e., "/../a")
  device              ; the pathname device
  source-root-dir
  source-pathname     ; relative or absolute (starts with "/"), directory or file (ends with "/")
  source-extension    ; a string, e.g., "lisp". If nil, uses default for machine-type
  binary-pathname
  binary-root-dir
  binary-extension    ; a string, e.g., "fasl". If nil, uses default for machine-type
  package             ; package for use-package
  components          ; a list of components comprising this component's definition
  depends-on          ; a list of the components this one depends on. may refer only
                      ; to the components at the same level as this one.
  initially-do        ; form to evaluate before the operation
  finally-do          ; form to evaluate after the operation
  compile-form        ; for foreign libraries
  load-form           ; for foreign libraries
)

(defun print-component (component stream depth)
  (declare (ignore depth))
  (format stream "#<~:(~A~) ~A>"
          (component-type component)
          (component-name component)))

(defun canonicalize-component-name (component)
  ;; Within the component, the name is a string.
  (if (typep (component-name component) 'string)
      ;; Unnecessary to change it, so just return it, same case
      (component-name component)
    ;; Otherwise, make it a downcase string
    (setf (component-name component) 
	  (string-downcase (string (component-name component))))))

(defun component-pathname (component type)
  (when component
    (case type
      (:source (component-source-pathname component))
      (:binary (component-binary-pathname component))
      (:error  (component-error-pathname component)))))
(defun component-error-pathname (component)
  (let ((binary (component-pathname component :binary)))
    (new-file-type binary *compile-error-file-type*)))
(defsetf component-pathname (component type) (value)
  `(when ,component
     (case ,type
       (:source (setf (component-source-pathname ,component) ,value))
       (:binary (setf (component-binary-pathname ,component) ,value)))))

(defun component-root-dir (component type)
  (when component
    (case type
      (:source (component-source-root-dir component))
      ((:binary :error) (component-binary-root-dir component))
      )))
(defsetf component-root-dir (component type) (value)
  `(when ,component
     (case ,type
       (:source (setf (component-source-root-dir ,component) ,value))
       (:binary (setf (component-binary-root-dir ,component) ,value)))))

(defvar *version-dir* nil
  "The version subdir. bound in oos.")
(defvar *version-replace* nil
  "The version replace. bound in oos.")
(defvar *version* nil
  "Default version")
(defun component-full-pathname (component type &optional (version *version*)
					  &aux version-dir replace)
  (when component
    ;; If the pathname-type is :binary and the root pathname is null,
    ;; distribute the binaries among the sources (= use :source pathname).
    ;; This assumes that the component's :source pathname has been set
    ;; before the :binary one.
    (if version
	(multiple-value-setq (version-dir replace) (translate-version version))
      (setq version-dir *version-dir* replace *version-replace*))
    (let ((pathname
	   (append-directories 
	    (if replace
		version-dir
	      (append-directories (component-root-dir component type)
				  version-dir))
	    (component-pathname component type))))
      (make-pathname :name (pathname-name pathname)
		     :type (component-extension component type)
		     :host (pathname-host (component-host component))
		     :device #+CMU :absolute
		             #-CMU (pathname-device (component-device component))
		     ;; :version :newest
		     ;; Use :directory instead of :defaults
		     :directory (pathname-directory pathname)))))

(defun translate-version (version)
  ;; Value returns the version directory and whether it replaces 
  ;; the entire root (t) or is a subdirectory.
  ;; Version may be nil to signify no subdirectory,
  ;; a symbol, such as alpha, beta, omega, :alpha, mark, which
  ;; specifies a subdirectory of the root, or
  ;; a string, which replaces the root.
  (cond ((null version) 
	 (values "" nil))
	((symbolp version)
	 (values (string-downcase (string version)) nil))
	((stringp version)
	 (values version t))
	(t (error "~&; Illegal version ~S" version))))

(defun component-extension (component type)
  (case type
    (:source (component-source-extension component))
    (:binary (component-binary-extension component))
    (:error  *compile-error-file-type*)))
(defsetf component-extension (component type) (value)
  `(case ,type
     (:source (setf (component-source-extension ,component) ,value))
     (:binary (setf (component-binary-extension ,component) ,value))
     (:error  (setf *compile-error-file-type* ,value))))

;;; ********************************
;;; System Definition **************
;;; ********************************
(defmacro defsystem (name &rest definition-body)    
  `(create-component :system ',name ',definition-body nil 0))

(defun create-component (type name definition-body &optional parent (indent 0))
  (let ((component (apply #'make-component :type type :name name :indent indent definition-body)))
    ;; Initializations/after makes
    (canonicalize-component-name component)

    ;; Type specific setup:
    (when (eq type :system)
      (setf (get-system name) component))

    ;; Set up the component's pathname
    (create-component-pathnames component parent)

    ;; If there are any components of the component, expand them too.
    (expand-component-components component (+ indent 2))

    ;; Make depends-on refer to structs instead of names.
    (link-component-depends-on (component-components component))

    ;; Design Decision: Topologically sort the dependency graph at
    ;; time of definition instead of at time of use. Probably saves a
    ;; little bit of time for the user.

    ;; Topological Sort the components at this level.
    (setf (component-components component)
          (topological-sort (component-components component)))

    ;; Return the component.
    component))

(defun create-component-pathnames (component parent)
  ;; Evaluate the root dir arg
  (setf (component-root-dir component :source)
	(eval (component-root-dir component :source)))
  (setf (component-root-dir component :binary)
	(eval (component-root-dir component :binary)))
  ;; Evaluate the pathname arg
  (setf (component-pathname component :source)
	(eval (component-pathname component :source)))
  (setf (component-pathname component :binary)
	(eval (component-pathname component :binary)))
  ;; Pass along the host and devices
  (setf (component-host component)
	(or (component-host component)
	    (when parent (component-host parent))))
  (setf (component-device component)
	(or (component-device component)
	    (when parent (component-device parent))))
  ;; Set up extension defaults
  (setf (component-extension component :source)
	(or (component-extension component :source) ; for local defaulting
	    (when parent		; parent's default
	      (component-extension parent :source))
	    (car *filename-extensions*))) ; system default
  (setf (component-extension component :binary)
	(or (component-extension component :binary) ; for local defaulting
	    (when parent		; parent's default
	      (component-extension parent :binary))
	    (cdr *filename-extensions*))) ; system default
  ;; Set up pathname defaults -- expand with parent
  ;; We must set up the source pathname before the binary pathname
  ;; to allow distribution of binaries among the sources to work.
  (generate-component-pathname component parent :source)
  (generate-component-pathname component parent :binary))

;; maybe file's inheriting of pathnames should be moved elsewhere?
(defun generate-component-pathname (component parent pathname-type)
  ;; Pieces together a pathname for the component based on its component-type.
  ;; Assumes source defined first.
  ;; Null binary pathnames inherit from source instead of the component's
  ;; name. This allows binaries to be distributed among the source if
  ;; binary pathnames are not specified. Or if the root directory is
  ;; specified for binaries, but no module directories, it inherits
  ;; parallel directory structure.
  (case (component-type component)
    (:system				; Absolute Pathname
     ;; Set the root-dir to be the absolute pathname
     (setf (component-root-dir component pathname-type)
	   (or (component-pathname component pathname-type)
	       (when (eq pathname-type :binary)
		 ;; When the binary root is nil, use source.
		 (component-root-dir component :source))) )
     ;; Set the relative pathname to be nil
     (setf (component-pathname component pathname-type) 
	   nil));; should this be "" instead?
    ;; If the name of the component-pathname is nil, it
    ;; defaults to the name of the component. Use "" to
    ;; avoid this defaulting.
    (:private-file                      ; Absolute Pathname
     ;; Root-dir is the directory part of the pathname
     (setf (component-root-dir component pathname-type)
	   ""
	   #+ignore(or (when (component-pathname component pathname-type)
			 (pathname-directory 
			  (component-pathname component pathname-type)))
		       (when (eq pathname-type :binary)
			 ;; When the binary root is nil, use source.
			 (component-root-dir component :source)))
	   )
     ;; The relative pathname is the name part
     (setf (component-pathname component pathname-type)
	   (or (when (and (eq pathname-type :binary)
			  (null (component-pathname component :binary)))
		 ;; When the binary-pathname is nil use source.
		 (component-pathname component :source))
	       (or (when (component-pathname component pathname-type)
;		     (pathname-name )
		     (component-pathname component pathname-type))
		   (component-name component)))))
    (:module				; Pathname relative to parent.
     ;; Inherit root-dir from parent
     (setf (component-root-dir component pathname-type)
	   (component-root-dir parent pathname-type))
     ;; Tack the relative-dir onto the pathname
     (setf (component-pathname component pathname-type)
	   (or (when (and (eq pathname-type :binary)
			  (null (component-pathname component :binary)))
		 ;; When the binary-pathname is nil use source.
		 (component-pathname component :source))
	       (append-directories
		(component-pathname parent pathname-type)
		(or (component-pathname component pathname-type)
		    (component-name component))))))
    (:file				; Pathname relative to parent.
     ;; Inherit root-dir from parent
     (setf (component-root-dir component pathname-type)
	   (component-root-dir parent pathname-type))
     ;; Tack the relative-dir onto the pathname
     (setf (component-pathname component pathname-type)
	   (or (append-directories
		(component-pathname parent pathname-type)
		(or (component-pathname component pathname-type)
		    (component-name component)
		    (when (eq pathname-type :binary)
		      ;; When the binary-pathname is nil use source.
		      (component-pathname component :source)))))))
    ))	   

(defun expand-component-components (component &optional (indent 0)) 
  (setf (component-components component)
        (mapcar #'(lambda (definition)
                    (expand-component-definition definition component indent))
                (component-components component))))

(defun expand-component-definition (definition parent &optional (indent 0))
  ;; Should do some checking for malformed definitions here.
  (cond ((null definition) nil)
        ((stringp definition) 
         ;; Strings are assumed to be of type :file
         (create-component :file definition nil parent indent))
        ((and (listp definition)
              (not (member (car definition) 
			   '(:system :module :file :private-file))))
         ;; Lists whose first element is not a component type
         ;; are assumed to be of type :file
         (create-component :file (car definition) (cdr definition) parent indent))
        ((listp definition)
         ;; Otherwise, it is (we hope) a normal form definition
         (create-component (car definition)   ; type
                           (cadr definition)  ; name
                           (cddr definition)  ; definition body
                           parent             ; parent
			   indent)            ; indent
         )))

(defun link-component-depends-on (components)
  (dolist (component components)
    (unless (and *system-dependencies-delayed*
                 (eq (component-type component) :system))
      (setf (component-depends-on component)
            (mapcar #'(lambda (dependency)
                        (find (string dependency) components :key #'component-name 
                              :test #'string-equal)) 
                    (component-depends-on component))))))

;;; ********************************
;;; Topological Sort the Graph *****
;;; ********************************
(defun topological-sort (list &aux (time 0))
  ;; The algorithm works by calling depth-first-search to compute the
  ;; blackening times for each vertex, and then sorts the vertices into
  ;; reverse order by blackening time.
  (labels ((dfs-visit (node)
             (setf (topsort-color node) 'gray)
             (unless (and *system-dependencies-delayed*
                          (eq (component-type node) :system))
               (dolist (child (component-depends-on node))
                 (cond ((eq (topsort-color child) 'white)
                        (dfs-visit child))
                       ((eq (topsort-color child) 'gray)
                        (format t "~&Detected cycle containing ~A" child)))))
             (setf (topsort-color node) 'black)
             (setf (topsort-time node) time)
             (incf time)))
    (dolist (node list)
      (setf (topsort-color node) 'white))
    (dolist (node list)
      (when (eq (topsort-color node) 'white)
        (dfs-visit node)))
    (sort list #'< :key #'topsort-time)))

;;; ********************************
;;; Output to User *****************
;;; ********************************
;;; All output to the user is via the tell-user functions.

(defun split-string (string &key (item #\space) (test #'char=))
  ;; Splits the string into substrings at spaces.
  (let ((len (length string))
	(index 0) result)
    (dotimes (i len
		(progn (unless (= index len)
			 (push (subseq string index) result))
		       (reverse result)))
      (when (funcall test (char string i) item)
	(unless (= index i);; two spaces in a row
	  (push (subseq string index i) result))
	(setf index (1+ i))))))

(defun prompt-string (component)
  (format nil "; ~:[~;TEST:~]~V,@T "
	  *oos-test*
	  (component-indent component)))

(defun format-justified-string (prompt contents)
  (format t (concatenate 'string "~%" prompt "-~{~<~%" prompt " ~1,80:; ~A~>~^~}")
	  (split-string contents))
  (finish-output *standard-output*))

(defun tell-user (what component &optional type no-dots force)
  (when (or *oos-verbose* force)
    (format-justified-string (prompt-string component)
     (format nil "~A ~(~A~) ~@[\"~A\"~] ~:[~;...~]"
	     (case what 
	       ((compile :compile) "Compiling")
	       ((load :load) "Loading")
	       (otherwise what))
	     (component-type component)
	     (or (when type
		   (namestring-or-nil (component-full-pathname
				       component type)))
		 (component-name component))
	     (and *tell-user-when-done*
		  (not no-dots))))))

(defun tell-user-done (component &optional force no-dots)
  ;; test is no longer really used, but we're leaving it in.
  (when (and *tell-user-when-done*
	     (or *oos-verbose* force))
    (format t "~&~A~:[~;...~] Done."
	    (prompt-string component) (not no-dots))
    (finish-output *standard-output*)))

(defmacro with-tell-user ((what component &optional type no-dots force) &body body)
  `(progn
     (tell-user ,what ,component ,type ,no-dots ,force)
     ,@body
     (tell-user-done ,component ,force ,no-dots)))

(defun tell-user-no-files (component &optional force)
  (when (or *oos-verbose* force)
    (format-justified-string (prompt-string component)
      (format nil "Binary file ~A ~
             ~:[and source file ~A do~;does~] not exist, not loading."
	      (namestring (component-full-pathname component :binary))
	      (or *load-source-if-no-binary* *load-source-instead-of-binary*)
	      (namestring (component-full-pathname component :source))))))

(defun tell-user-require-system (name parent)
  (when *oos-verbose*
    (format t "~&; ~:[~;TEST:~] - System ~A requires ~S"
	    *oos-test* (component-name parent) name)
    (finish-output *standard-output*)))

(defun tell-user-generic (string)
  (when *oos-verbose*
    (format t "~&; ~:[~;TEST:~] - ~A"
	    *oos-test* string)
    (finish-output *standard-output*)))

;;; ********************************
;;; Y-OR-N-P-WAIT ******************
;;; ********************************
;;; y-or-n-p-wait is like y-or-n-p, but will timeout
;;; after a specified number of seconds
(defun internal-real-time-in-seconds ()
  (float (/ (get-internal-real-time) 
	    internal-time-units-per-second)))

(defun read-char-wait (&optional (timeout 20) input-stream &aux char)
  (do ((start (internal-real-time-in-seconds)))
      ((or (setq char (read-char-no-hang input-stream)) ;(listen *query-io*)
	   (< (+ start timeout) (internal-real-time-in-seconds)))
       char)))

(defun y-or-n-p-wait (&optional (default #\y) (timeout 20) 
				format-string &rest args)
  (clear-input *query-io*)
  (format *query-io* "~&~?" (or format-string "") args)
  (finish-output *query-io*);; needed for CMU and other places
  (let* ((read-char (read-char-wait timeout *query-io*))
	 (char (or read-char default)))
    (clear-input *query-io*)
    (when (null read-char) (format *query-io* "~@[~A~]" default))
    (cond ((null char)  t)
	  ((find char '(#\y #\Y #\space) :test #'char=) t)
	  ((find char '(#\n #\N) :test #'char=) nil)
	  (t (y-or-n-p-wait default timeout 
			    "Type \"y\" for yes or \"n\" for no. ")))))

(defvar *use-timeouts* t)

(defun y-or-n-p-wait-optional (&optional (default #\y) (timeout 20) 
					 format-string &rest args)
  (cond ((and default
	      *use-timeouts*)
	 (apply 'y-or-n-p-wait default timeout format-string args))
	(t       (apply 'y-or-n-p format-string args))))

#|
(y-or-n-p-wait #\y 20 "What? ")
(progn (format t "~&hi")
       (y-or-n-p-wait #\y 10 "1? ")
       (y-or-n-p-wait #\n 10 "2? "))
|#
;;; ********************************
;;; Operate on System **************
;;; ********************************
;;; Operate-on-system
;; Operation is :compile, 'compile, :load or 'load
;; Force is :all or :new-source or :new-source-and-dependents or a list of
;; specific modules.
;;    :all (or T) forces a recompilation of every file in the system
;;    :new-source-and-dependents compiles only those files whose
;;          sources have changed or who depend on recompiled files.
;;    :new-source compiles only those files whose sources have changed
;;    A list of modules means that only those modules and their dependents are recompiled.
;; Test is T to print out what it would do without actually doing it. 
;;      Note: it automatically sets verbose to T if test is T.
;; Verbose is T to print out what it is doing (compiling, loading of
;;      modules and files) as it does it.
;; Dribble should be the pathname of the dribble file if you want to 
;; dribble the compilation.
;; Load-source-instead-of-binary is T to load .lisp instead of binary files.
;; Version may be nil to signify no subdirectory,
;; a symbol, such as alpha, beta, omega, :alpha, mark, which
;; specifies a subdirectory of the root, or
;; a string, which replaces the root.
(defun operate-on-system (name operation &key force
			       (version *version*)
			       (test *oos-test*) (verbose *oos-verbose*)
                               (load-source-instead-of-binary *load-source-instead-of-binary*)
                               (load-source-if-no-binary *load-source-if-no-binary*) 
			       (bother-user-if-no-binary *bother-user-if-no-binary*)
			       dribble)
  (when dribble (dribble dribble))
  (when test (setq verbose t))
  (when (null force);; defaults
    (case operation
      ((load :load) (setq force :all))
      ((compile :compile) (setq force :new-source-and-dependents))))
  ;; Some CL implementations have a variable called *compile-verbose*
  (multiple-value-bind (*version-dir* *version-replace*) 
      (translate-version version)
    (let ((*load-verbose* nil);; CL implementations may uniformly default this to nil
	  (*version* version)
	  (*oos-verbose* verbose)
	  (*oos-test* test)
	  (*load-source-if-no-binary* load-source-if-no-binary)
	  (*bother-user-if-no-binary* bother-user-if-no-binary)
	  (*load-source-instead-of-binary* load-source-instead-of-binary)
	  (system (get-system name)))
      (unless system (error "Can't find system named ~A." name))
      (unless (component-operation operation) (error "Operation ~A undefined." operation))
      (operate-on-component system operation force)))
  (when dribble (dribble)))

(defun operate-on-component (component operation force &aux changed)
  ;; Returns T if something changed and had to be compiled.
  (let ((type (component-type component))
	;; Use the correct package.
	#+ignore(*package*
		 (or (when (component-package component)
		       (tell-user-generic (format nil "Using package ~A" 
						  (component-package component)))
		       (cond (*oos-test* *package*)
			     (t (find-package (component-package component)))))
		     *package*))
)
    (when (component-package component)
      (tell-user-generic (format nil "Using package ~A" 
				 (component-package component)))
      (cond (*oos-test* *package*)
	    (t (unless (find-package (component-package component))
		 (require (component-package component)))
	       (use-package (component-package component)))))

    ;; Load any required systems
    (when (eq type :system)
      (operate-on-system-dependencies component))

    ;; Do any initial actions
    (when (component-initially-do component)
      (tell-user-generic (format nil "Doing initializations for ~A"
				 (component-name component)))
      (or *oos-test*
	  (eval (component-initially-do component))))

    ;; Do operation and set changed flag if necessary.
    (setq changed 
	  (case type
	    ((:file :private-file)
	     (funcall (component-operation operation) component force))
	    ((:module :system)
	     (operate-on-components component operation force changed))))

    ;; Do any final actions
    (when (component-finally-do component)
      (tell-user-generic (format nil "Doing finalizations for ~A"
				 (component-name component)))
      (or *oos-test*
	  (eval (component-finally-do component))))

    ;; Provide the loaded system
    (when (eq type :system)
      (tell-user-generic (format nil "Providing system ~A"
				 (component-name component)))
      (or *oos-test*
	  (provide (canonicalize-system-name (component-name component))))))

  ;; Return t if something changed in this component and hence had to be recompiled.
  changed)

(defun operate-on-system-dependencies (component)
  (when *system-dependencies-delayed*
    (dolist (system (component-depends-on component))
      ;; For each system that this system depends on,
      ;; runs require (my version) on that system to load it.
      ;; Explores the system tree in a DFS manner.
      (cond ((listp system)
	     (tell-user-require-system 
	      (cond ((and (null (car system)) (null (cadr system)))
		     (caddr system))
		    (t system))
	      component)
	     (or *oos-test* (require (car system) nil
				     (eval (cadr system))
				     (caddr system) 
				     (or (car (cdddr system))
					 *version*))))
	    (t
	     (tell-user-require-system system component)
	     (or *oos-test* (require system))))
      )))

(defun operate-on-components (component operation force changed)
  (with-tell-user (operation component)
    (if (component-components component)
	(dolist (module (component-components component))
	  (when (operate-on-component module operation
		  (cond ((and (some #'(lambda (dependent)
					(member dependent changed))
				    (component-depends-on module))
			      (or (non-empty-listp force)
				  (eq force :new-source-and-dependents)))
			 ;; The component depends on a changed file 
			 ;; and force agrees.
			 (if (eq force :new-source-and-dependents)
			     :new-source-all
			   :all))
			((and (non-empty-listp force)
			      (member (component-name module) force
				      :test #'string-equal :key #'string))
			 ;; Force is a list of modules 
			 ;; and the component is one of them.
			 :all)
			(t force)))
	    (push module changed)))
	(case operation
	  ((compile :compile)
	   (eval (component-compile-form component)))
	  ((load :load)
	   (eval (component-load-form component))))))
  changed)

;;; ********************************
;;; Component Operations ***********
;;; ********************************
;;; Define :compile/compile and :load/load operations
(component-operation :compile  'compile-and-load-operation)
(component-operation 'compile  'compile-and-load-operation)
(component-operation :load     'load-file-operation)
(component-operation 'load     'load-file-operation)

(defun compile-and-load-operation (component force)
  (let ((changed (compile-file-operation component force)))
    (load-file-operation component changed)
    changed))

(defun compile-file-operation (component force)
  ;; Returns T if the file had to be compiled.
  (let ((must-compile
	 (or (find force '(:all :new-source-all t) :test #'eq) 
	     (and (find force '(:new-source :new-source-and-dependents)
			:test #'eq)
		  (needs-compilation component)))))

    (cond ((and must-compile
		(probe-file (component-full-pathname component :source)))
	   (with-tell-user ("Compiling source" component :source)
	       (or *oos-test*
		   (compile-file (component-full-pathname component :source)
		       :output-file (component-full-pathname component :binary)
		       #+CMU :error-file #+CMU (and *cmu-errors-to-file* 
						    (component-full-pathname component :error))
		       #+CMU :errors-to-terminal #+CMU *cmu-errors-to-terminal*
		       )))
	   must-compile)
	  (must-compile
	   (tell-user "Source file not found. Not compiling"
		      component :source :no-dots :force)
	   nil)
	  (t nil))))

(defun needs-compilation (component)
  ;; If there is no binary, or it is older than the source
  ;; file, then the component needs to be compiled.
  ;; Otherwise we only need to recompile if it depends on a file that changed.
  (or
   ;; no binary
   (null (probe-file (component-full-pathname component :binary))) 
   ;; old binary
   (< (file-write-date (component-full-pathname component :binary)) 
      (file-write-date (component-full-pathname component :source)))))

(defun load-file-operation (component force)
  ;; Returns T if the file had to be loaded
  (let ((load-source
	 (or *load-source-instead-of-binary*
	     (and (find force '(:new-source :new-source-and-dependents
					    :new-source-all)
			:test #'eq)
		  (needs-compilation component))))
	(load-binary
	 (or (find force '(:all :new-source-all t) :test #'eq) ; Force load. 
	     ;; :new-source* and binary up to date
	     (find force '(:new-source :new-source-and-dependents)
		   :test #'eq))))

    (cond ((and load-source
		(probe-file (component-full-pathname component :source)))
	   (with-tell-user ("Loading source" component :source)
	       (or *oos-test*
		   (load (component-full-pathname component :source))))
	   T)
	  ((and load-binary 
		(probe-file (component-full-pathname component :binary)))
	   (with-tell-user ("Loading binary"   component :binary)
	       (or *oos-test*
		   (load (component-full-pathname component :binary))))
	   T)
	  ((and load-binary
		(probe-file (component-full-pathname component :source))
		(load-source-if-no-binary component))
	   (with-tell-user ("Loading source"   component :source)
	       (or *oos-test*
		   (load (component-full-pathname component :source))))
	   T)
	  ((or load-source load-binary)
	   (tell-user-no-files component :force)
	   nil)
	  (t nil))))

	
;; when the operation = :compile, we can assume the binary exists in test mode.
;;	((and *oos-test*
;;	      (eq operation :compile)
;;	      (probe-file (component-full-pathname component :source)))
;;	 (with-tell-user ("Loading binary"   component :binary)))

(defun load-source-if-no-binary (component)
  (and (not *load-source-instead-of-binary*)
       (or *load-source-if-no-binary*
	   (when *bother-user-if-no-binary*
	     (let* ((prompt (prompt-string component))
		    (load-source
		     (y-or-n-p-wait-optional
		      #\y 30
		      "~A- Binary file ~A does not exist. ~
                       ~&~A  Load source file ~A instead? "
		      prompt
		      (namestring (component-full-pathname component :binary))
		      prompt
		      (namestring (component-full-pathname component :source)))))
	       (setq *bother-user-if-no-binary*
		     (y-or-n-p-wait-optional 
		      #\n 30
		      "~A- Should I bother you if this happens again? "
		      prompt ))
	       (unless *bother-user-if-no-binary*
		 (setq *load-source-if-no-binary* load-source))
	       load-source)))))

;;; ********************************
;;; New Require ********************
;;; ********************************
(defun compute-system-path (module-name definition-pname)
  (let* ((filename (format nil "~A.system" 
			   (if (symbolp module-name)
			       (string-downcase (string module-name))
			     module-name))))
    (or (when definition-pname		; given pathname for system def
	  (probe-file definition-pname))
	(probe-file filename)		; try current dir
	(probe-file (append-directories *central-registry* filename))) ; central registry
    ))

(unless (fboundp 'old-require)
  (setf (symbol-function 'old-require) (symbol-function 'lisp:require))

  (let (#+:CCL (ccl:*warn-if-redefine-kernel* nil))
    (defun lisp:require (module-name &optional pathname definition-pname
				     default-action (version *version*)
				     &aux system-path)
      ;; If the pathname is present, this behaves like the old require.
      (unless (and module-name 
		   (find #-CMU (string module-name)
			 #+CMU (string-downcase (string module-name))
			 *modules* :test #'string=)) 
	(cond (pathname
	       (funcall 'old-require module-name pathname))
	      ;; If the system is defined, load it.
	      ((or (get-system module-name)
		   (and (setq system-path 
			      (compute-system-path module-name definition-pname))
			(load system-path)
			(get-system module-name)))
	       (operate-on-system module-name :load
				  :version version
				  :test *oos-test*
				  :verbose *oos-verbose*
				  :load-source-if-no-binary *load-source-if-no-binary*
				  :bother-user-if-no-binary *bother-user-if-no-binary*
				  :load-source-instead-of-binary *load-source-instead-of-binary*))
	      ;; If there's a default action, do it. This could be a progn which
	      ;; loads a file that does everything. 
	      ((and default-action
		    (eval default-action)))
	      ;; If no system definition file, try regular require.
	      ((funcall 'old-require module-name pathname))
	      ;; If no default action, print a warning or error message.
	      (t
	       (format t "~&Warning: System ~A doesn't seem to be defined..." module-name))
	      )))))

;;; ********************************
;;; Allegro Make System Fasl *******
;;; ********************************
#+:excl
(defun allegro-make-system-fasl (system destination)
  (excl:shell
   (format nil "rm -f ~A; cat~{ ~A~} > ~A" 
	   destination
	   (mapcar #'namestring
		   (files-in-system system :all :binary)))))

(defun files-which-need-compilation (system)
  (mapcar #'(lambda (comp) (namestring (component-full-pathname comp :source)))
	  (remove nil
		  (file-components-in-component
		   (get-system system) :new-source))))

(defun files-in-system (name &optional (force :all) (type :source) version
			     &aux system)
  ;; Returns a list of the pathnames in system in load order.
  (setq system (get-system name))
  (unless system (error "Can't find system named ~A." name))
  (multiple-value-bind (*version-dir* *version-replace*) 
      (translate-version version)
  (let ((*version* version))
    (file-pathnames-in-component system type force))))

(defun file-pathnames-in-component (component type &optional (force :all))
  (mapcar #'(lambda (comp) (component-full-pathname comp type))
	  (file-components-in-component component force)))

(defun file-components-in-component (component &optional (force :all) 
					       &aux result changed)
  (case (component-type component)
    ((:file :private-file)
     (when (setq changed 
		 (or (find force '(:all t) :test #'eq) 
		     (and (not (non-empty-listp force))
			  (needs-compilation component))))
       (setq result
	     (list component))))
    ((:module :system)
     (dolist (module (component-components component))
       (multiple-value-bind (r c)
	   (file-components-in-component 
	    module 
	    (cond ((and (some #'(lambda (dependent)
				  (member dependent changed))
			      (component-depends-on module))
			(or (non-empty-listp force)
			    (eq force :new-source-and-dependents)))
		   ;; The component depends on a changed file and force agrees.
		   :all)
		  ((and (non-empty-listp force)
			(member (component-name module) force
				:test #'string-equal :key #'string))
		   ;; Force is a list of modules and the component is one of them.
		   :all)
		  (t force)))
	 (when c
	   (push module changed)
	   (setq result (append result r)))))))
  (values result changed))

(setf (symbol-function 'oos) (symbol-function 'operate-on-system))

;;; *END OF FILE*
