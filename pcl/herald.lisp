(in-package "PCL")

#+(or loadable-pcl bootable-pcl)
(progn
  (defvar *pcl-system-date* "$Date: 2008/11/12 16:36:41 $")
  (setf (getf *herald-items* :pcl)
	`("    CLOS based on Gerd's PCL " ,(if (>= (length *pcl-system-date*) 26)
					       (subseq *pcl-system-date* 7 26)
					       ""))))

