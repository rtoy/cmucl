(in-package "PCL")
(unless (find-package "SLOT-ACCESSOR-NAME")
  (make-package "SLOT-ACCESSOR-NAME"))
(rename-package "PCL" "PCL" '("OLD-PCL"))
(rename-package "SLOT-ACCESSOR-NAME" "SLOT-ACCESSOR-NAME"
		'("OLD-SLOT-ACCESSOR-NAME"))
(import 'kernel:funcallable-instance-p)
(load "target:pcl/defsys")
(load-pcl)
(rename-package "PCL" "PCL" '())
(rename-package "SLOT-ACCESSOR-NAME" "SLOT-ACCESSOR-NAME" '())

