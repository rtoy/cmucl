(in-package "PCL")
(import 'kernel:funcallable-instance-p)
(load "pcl:defsys")
(load-pcl)

;; hack, hack...
(ignore-errors
 (with-output-to-string (*standard-output*)
   (describe #'print-object)))
