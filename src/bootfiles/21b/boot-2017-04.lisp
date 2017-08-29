;; Bootstrap file for linuu to move the heap to a higher address so
;; that executables will work on newer OSes, as mentioned in issue
;; #40.
;;
;; Use "bin/build.sh -B boot-2017-04" to build this.
;;
;; Also need to move the foreign linkage start to a different address
;; because this also overlaps where the C code is placed in an
;; executable image.
;;
;; This reduces the max total heap space on older systems, but it
;; looks like newer systems have the C libraries and stack mapped at a
;; much higher address.

#+linux
(setf (c::backend-foreign-linkage-space-start c::*target-backend*)
      #x5f000000)

#+linux
(handler-bind
    ((error (lambda (c)
	      (declare (ignore c))
	      (invoke-restart 'continue))))
  (defconstant vm::target-foreign-linkage-space-start
    (c:backend-foreign-linkage-space-start c::*target-backend*))

  (defconstant vm::target-dynamic-space-start
    #x60000000))
