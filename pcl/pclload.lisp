(in-package "PCL")
(import 'kernel:funcallable-instance-p)
(load "target:pcl/defsys")
(load-pcl)

;; hack, hack...
#+nil
(ignore-errors
 (with-output-to-string (*standard-output*)
   (describe #'print-object)))
