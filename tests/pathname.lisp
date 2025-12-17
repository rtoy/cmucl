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

  (assert-true (pathname-match-p "/tmp/foo.lisp" "foo:*"))
  (assert-true (pathname-match-p "/tmp/zot/foo.lisp" "foo:**/*"))
  (assert-true (pathname-match-p "/tmp/zot/foo.lisp" "foo:**/*.lisp"))
  ;; These match because the second entry of the "foo:" search list is
  ;; "/usr/".
  (assert-true (pathname-match-p "/usr/foo.lisp" "foo:*"))
  (assert-true (pathname-match-p "/usr/bin/foo" "foo:**/*"))
  (assert-true (pathname-match-p "/usr/bin/foo.lisp" "foo:**/*.lisp"))

  ;; This fails because "/bin/" doesn't match any path of the search
  ;; list.
  (assert-false (pathname-match-p "/bin/foo.lisp" "foo:*"))

  ;; Basic test where the pathname is a search-list and the wild path is not.
  (assert-true (pathname-match-p "foo:foo.lisp" "/tmp/*"))
  (assert-true (pathname-match-p "foo:foo" "/usr/*"))
  (assert-true (pathname-match-p "foo:zot/foo.lisp" "/usr/**/*.lisp"))

  (assert-false (pathname-match-p "foo:foo" "/bin/*"))
  
  ;; Tests where both args are search-lists.
  (assert-true "foo:foo.lisp" "bar:*"))

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



(define-test directory.dirs
  (let ((files (directory "src/assembly/**/")))
    ;; Verify that we only returned directories
    (loop for f in files
	  for name = (pathname-name f)
	  and type = (pathname-type f)
	  do
	     (assert-true (and (null name) (null type)) f))))



;; Test that pathnames with :unspecific components are printed using
;; our extension to make :unspecific explicit. 
(define-test issue.171.unspecific
  (:tag :issues)
  (flet ((output (path)
	   (with-output-to-string (s)
	     (write path :stream s))))
    (dolist (test
	     (list
	      (list (make-pathname :name "foo" :type :unspecific)
		    "#P(:NAME \"foo\" :TYPE :UNSPECIFIC)"
		    "foo")
	      (list (make-pathname :name :unspecific :type "foo")
		    "#P(:NAME :UNSPECIFIC :TYPE \"foo\")"
		    ".foo")
	      (list (make-pathname :name "foo" :type "txt" :version :unspecific)
		    "#P(:NAME \"foo\" :TYPE \"txt\" :VERSION :UNSPECIFIC)"
		    "foo.txt")
	      (list (make-pathname :device :unspecific)
		    "#P(:DEVICE :UNSPECIFIC)"
		    "")))
      (destructuring-bind (pathname printed-value namestring)
	  test
	(assert-equal printed-value (output pathname))
	(assert-equal namestring (namestring pathname))))))

(define-test issue.266.pathname-tilde.unknown-user
    (:tag :issues)
  ;; This assumes that there's no user named "zotunknown".
  (assert-error 'simple-error (parse-namestring "~zotunknown/*.*")))

(define-test issue.266.pathname-tilde.1
    (:tag :issues)
  ;; Simple test for ~ in pathnames.  Get a directory list using
  ;; #P"~/*.*".  This should produce exactly the same list as the
  ;; #search-list P"home:*.*".
  (let ((dir-home (directory #p"home:*.*" :truenamep nil :follow-links nil))
        (dir-tilde (directory #p"~/*.*" :truenamep nil :follow-links nil)))
    (assert-equal dir-tilde dir-home)))

(define-test issue.266.pathname-tilde.2
    (:tag :issues)
  ;; Simple test for ~ in pathnames.  Get a directory list using
  ;; #P"~user/*.*".  This should produce exactly the same list as the
  ;; #search-list P"home:*.*".  We determine the user name via getuid
  ;; #and getpwuid.
  (let ((user-name (unix:user-info-name (unix:unix-getpwuid (unix:unix-getuid)))))
    (assert-true user-name)
    (let* ((dir-home (directory #p"home:*.*" :truenamep nil :follow-links nil))
         
           (dir-tilde (directory (concatenate 'string
                                              "~"
                                              user-name
                                              "/*.*")
                                 :truenamep nil :follow-links nil)))
      (assert-equal dir-tilde dir-home))))

(define-test delete-directory
  (:tag :issues)
  (ext:with-temporary-directory (path)
    (let ((dir (ensure-directories-exist (merge-pathnames "tmp/a/b/c/" path))))
      ;; Try to delete the directory.  It should fail..
      (assert-error 'kernel:simple-file-error
		    (ext:delete-directory (merge-pathnames "tmp/" path)))
      ;; Now recursively delete the directory.
      (assert-true (ext:delete-directory (merge-pathnames "tmp/" path)
					 :recursive t))
      (assert-false (directory "tmp/")))))
