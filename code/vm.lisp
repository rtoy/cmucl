;;; -*- Log: code.log; Package: MACH -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/vm.lisp,v 1.2 1991/02/08 13:36:37 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/vm.lisp,v 1.2 1991/02/08 13:36:37 ram Exp $
;;;
;;; This file contains stubs for interfacing MACH's vm primitives.
;;;
(in-package "MACH")

(export '(vm_allocate vm_copy vm_deallocate vm_statistics))

(def-c-pointer *sap system-area-pointer)


(def-c-routine ("vm_allocate" vm_allocate) (int)
  (task port)
  (address *sap :in-out)
  (size unsigned-long)
  (anywhere boolean))

(def-c-routine ("vm_copy" vm_copy) (int)
  (task port)
  (source system-area-pointer)
  (count unsigned-long)
  (dest system-area-pointer))

(def-c-routine ("vm_deallocate" vm_deallocate) (int)
  (task port)
  (address system-area-pointer)
  (size unsigned-long))




;;;; vm_statistics

(def-c-record vm_statistics
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
  (hits long))

(def-c-routine ("vm_statistics" %vm_statistics) (int)
  (task port)
  (vm_stats system-area-pointer))

(defun vm_statistics (task)
  (with-stack-alien (vm_stats vm_statistics (c-sizeof 'vm_statistics))
    (values
     (%vm_statistics task (alien-sap (alien-value vm_stats)))
     (alien-access (vm_statistics-pagesize (alien-value vm_stats)))
     (alien-access (vm_statistics-free_count (alien-value vm_stats)))
     (alien-access (vm_statistics-active_count (alien-value vm_stats)))
     (alien-access (vm_statistics-inactive_count (alien-value vm_stats)))
     (alien-access (vm_statistics-wire_count (alien-value vm_stats)))
     (alien-access (vm_statistics-zero_fill_count (alien-value vm_stats)))
     (alien-access (vm_statistics-reactivations (alien-value vm_stats)))
     (alien-access (vm_statistics-pageins (alien-value vm_stats)))
     (alien-access (vm_statistics-pageouts (alien-value vm_stats)))
     (alien-access (vm_statistics-faults (alien-value vm_stats)))
     (alien-access (vm_statistics-cow_faults (alien-value vm_stats)))
     (alien-access (vm_statistics-lookups (alien-value vm_stats)))
     (alien-access (vm_statistics-hits (alien-value vm_stats))))))

