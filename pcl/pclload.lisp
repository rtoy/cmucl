(in-package "PCL")
(import 'kernel:funcallable-instance-p)
(load "pcl:defsys")
(load-pcl)
(use-package "PCL" "USER")

;; hack, hack...
;; It seems that the first time we do this call, it spuriously says no matching
;; methods, but subsequent calls work.
(ignore-errors
 (with-output-to-string (*standard-output*)
   (describe #'print-object)))
