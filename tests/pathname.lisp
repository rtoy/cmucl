;; Tests for pathnames

(defpackage :pathname-tests
  (:use :cl :lisp-unit))

(in-package "PATHNAME-TESTS")

;; Define "foo:" search list.  /tmp and /usr should exist on all unix
;; systems.
(setf (ext:search-list "foo:")
      '(#p"/tmp/" #p"/usr/"))

;; Define "bar:" search list.  The second entry should match the
;; second entry of the "foo:" search list.
(setf (ext:search-list "bar:")
      '(#p"/bin/" #p"/usr/"))

(define-test pathname-match-p.search-lists
    (:tag :search-list)
  ;; Basic tests where the wild path is search-list

  (assert-false (pathname-match-p "/tmp/foo.lisp" "foo:*"))
  (assert-true (pathname-match-p "/tmp/foo.lisp" "foo:*.*"))
  (assert-false (pathname-match-p "/tmp/zot/foo.lisp" "foo:**/*"))
  (assert-true (pathname-match-p "/tmp/zot/foo.lisp" "foo:**/*.*"))
  (assert-true (pathname-match-p "/tmp/zot/foo.lisp" "foo:**/*.lisp"))
  ;; These match because the second entry of the "foo:" search list is
  ;; "/usr/".
  (assert-false (pathname-match-p "/usr/foo.lisp" "foo:*"))
  (assert-true (pathname-match-p "/usr/foo.lisp" "foo:*.*"))
  (assert-true (pathname-match-p "/usr/bin/foo" "foo:**/*"))
  (assert-true (pathname-match-p "/usr/bin/foo.lisp" "foo:**/*.lisp"))

  ;; This fails because "/bin/" doesn't match any path of the search
  ;; list.
  (assert-false (pathname-match-p "/bin/foo.lisp" "foo:*.*"))

  ;; Basic test where the pathname is a search-list and the wild path is not.
  (assert-false (pathname-match-p "foo:foo.lisp" "/tmp/*"))
  (assert-true (pathname-match-p "foo:foo.lisp" "/tmp/*.*"))
  (assert-true (pathname-match-p "foo:foo" "/usr/*"))
  (assert-true (pathname-match-p "foo:foo" "/usr/*.*"))
  (assert-true (pathname-match-p "foo:zot/foo.lisp" "/usr/**/*.lisp"))

  (assert-false (pathname-match-p "foo:foo" "/bin/*"))

  ;; Tests where both args are search-lists.
  (assert-true (pathname-match-p "foo:foo.lisp" "bar:*.*")))

;; Verify PATHNAME-MATCH-P works with logical pathnames.  (Issue 27)
;; This test modeled after a test from asdf
(defun setup-logical-host ()
  (let ((root *default-pathname-defaults*)
	(bin-type (pathname-type (compile-file-pathname "foo.lisp"))))
    (setf (logical-pathname-translations "ASDFTEST")
	  `((,(format nil "**;*.~a" bin-type)
	      ,(merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)
					       :name :wild :type bin-type :version nil)))
	    (,(format nil "**;*.~a.*" bin-type)
	      ,(merge-pathnames (make-pathname :directory '(:relative "asdf-bin" :wild-inferiors)
					       :name :wild :type bin-type
					       :defaults root)))
	    ("**;*.*.*"
	     ,(merge-pathnames (make-pathname :directory '(:relative "asdf-src" :wild-inferiors)
					      :name :wild :type :wild :version :wild)))
	    ("**;*.*"
	     ,(merge-pathnames (make-pathname :directory '(:relative "asdf-src" :wild-inferiors)
					      :name :wild :type :wild :version nil)))
	    ("**;*"
	     ,(merge-pathnames (make-pathname :directory '(:relative "asdf-src" :wild-inferiors)
					      :name :wild :type nil :version nil)))
	    ("tests;**;*.*"
	     "**/*.*")))
    (setf (logical-pathname-translations "test")
	  '(("**;*.*" "tests/**/*.*")
	    ("**;*.*.*" "tests/**/*.*.~*~")))))
(setup-logical-host)

(define-test pathname-match-p.logical-pathname
  (assert-true (pathname-match-p
		(make-pathname :host "ASDFTEST"
			       :directory '(:absolute "system2" "module4")
			       :name nil :type nil)
		(parse-namestring "ASDFTEST:system2;module4;"))))



(define-test pathname-match-p.unspecific
  ;; Test that a field of :unspecific matches nil.
  (let ((wild-path #p"**/*.*"))
    (assert-true (pathname-match-p (make-pathname :device :unspecific)
				   wild-path))
    (assert-true (pathname-match-p (make-pathname :name :unspecific)
				   wild-path))
    (assert-true (pathname-match-p (make-pathname :type :unspecific)
				   wild-path))
    (assert-true (pathname-match-p (make-pathname :version :unspecific)
				   wild-path))
    ;; Slightly more complicated pathnames with :unspecific
    (assert-true (pathname-match-p (make-pathname :device :unspecific
						  :name "foo"
						  :type "bar")
				   wild-path))
    (assert-true (pathname-match-p (make-pathname :directory '(:relative "a")
						  :name :unspecific
						  :type "bar")
				   wild-path))
    (assert-true (pathname-match-p (make-pathname :directory '(:relative "a")
						  :name "foo"
						  :type :unspecific)
				   wild-path))
    (assert-true (pathname-match-p (make-pathname :directory '(:relative "a")
						  :name "foo"
						  :type "bar"
						  :version :unspecific)
				   wild-path))))

(define-test directory-pathname-match-p
  ;; Test that directory and pathname-match-p are consistent
  (let* ((wild-path #P"**/*.*")
	 (dir (directory wild-path :truenamep nil)))
    (loop for p in dir
	  do
	     (assert-true (pathname-match-p p wild-path)))))

(define-test directory-pathname-match-p.lpn
  ;; Like directory-pathname-match-p but for a logical pathname
  (let* ((wild-path #P"ASDFTEST:**;*.*.*")
	 (dir (directory wild-path :truenamep nil)))
    (loop for p in dir
	  do
	     (assert-true (pathname-match-p p wild-path)))))

(define-test directory-consistent-pn-vs-lpn
  ;; Test the directory with a physical pathname and a logical
  ;; pathname return the same files.
  (let ((dir-pn (directory #P"tests/**/*.*" :truenamep nil))
	(dir-lpn (directory #P"test:**;*.*.*" :truenamep nil)))
    ;; The number of entries should be the same.
    (assert-equal (length dir-pn) (length dir-lpn)
		  dir-pn dir-lpn)
    (loop for pn in dir-pn
	  for lpn in dir-lpn
	  do
	     (assert-equal pn lpn))))

(define-test directory-only
  ;; Test that we only get directories when requested
  (let ((dirs (directory #P"tests/**/" :truenamep nil)))
    (loop for p in dirs
	  do
	     (assert-false (pathname-name p) p)
	     (assert-false (pathname-type p) p)
	     (assert-true (let ((version (pathname-version p)))
			    (or (null version)
				(eq version :newest)))
			  p))))
	
