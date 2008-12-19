(in-package "PCL")

#+(or loadable-pcl bootable-pcl)
(progn
  (defvar *pcl-system-date* "$Date: 2008/12/19 01:31:34 $")
  (setf (getf *herald-items* :pcl)
	`("    CLOS based on Gerd's PCL " ,(if (>= (length *pcl-system-date*) 26)
					       (subseq *pcl-system-date* 7 26)
					       ""))))

