;;; -*- Package: MACH -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/mach.lisp,v 1.1 1992/01/24 04:31:37 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the low-level support for MACH features not found
;;; in UNIX.
;;;

(in-package "MACH")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "SYSTEM")

(export '(
	  port mach-task_self mach-task_data mach-task_notify
	  vm_allocate vm_copy vm_deallocate vm_statistics))

(def-alien-type port int)


;;;; Standard ports.

(def-alien-routine ("task_self" mach-task_self) int)
(def-alien-routine ("thread_reply" mach-task_data) int)
(def-alien-routine ("task_notify" mach-task_notify) int)



;;;; VM routines.

(export '(vm_allocate vm_copy vm_deallocate vm_statistics))

(def-alien-routine ("vm_allocate" vm_allocate) int
  (task port)
  (address system-area-pointer :in-out)
  (size unsigned-long)
  (anywhere boolean))

(def-alien-routine ("vm_copy" vm_copy) int
  (task port)
  (source system-area-pointer)
  (count unsigned-long)
  (dest system-area-pointer))

(def-alien-routine ("vm_deallocate" vm_deallocate) int
  (task port)
  (address system-area-pointer)
  (size unsigned-long))


(def-alien-type nil
  (struct vm_statistics
    (pagesize long)
    (free_count long)
    (active_count long)
    (inactive_count long)
    (wire_count long)
    (zero_fill_count long)
    (reactivations long)
    (pageins long)
    (pageouts long)
    (faults long)
    (cow_faults long)
    (lookups long)
    (hits long)))

(defun vm_statistics (task)
  (with-alien ((vm_stats (struct vm_statistics)))
    (values
     (alien-funcall (extern-alien "vm_statistics"
				  (function int
					    port
					    (* (struct vm_statistics))))
		    task (alien-sap vm_stats))
     (slot vm_stats 'pagesize)
     (slot vm_stats 'free_count)
     (slot vm_stats 'active_count)
     (slot vm_stats 'inactive_count)
     (slot vm_stats 'wire_count)
     (slot vm_stats 'zero_fill_count)
     (slot vm_stats 'reactivations)
     (slot vm_stats 'pageins)
     (slot vm_stats 'pageouts)
     (slot vm_stats 'faults)
     (slot vm_stats 'cow_faults)
     (slot vm_stats 'lookups)
     (slot vm_stats 'hits))))
