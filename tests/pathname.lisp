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
					      :name :wild :type nil :version nil)))))))
(setup-logical-host)

(define-test pathname-match-p.logical-pathname
  (assert-true (pathname-match-p
		(make-pathname :host "ASDFTEST"
			       :directory '(:absolute "system2" "module4")
			       :name nil :type nil)
		(parse-namestring "ASDFTEST:system2;module4;"))))
  
