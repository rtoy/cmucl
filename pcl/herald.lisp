(in-package "PCL")
(intl:textdomain "cmucl")

#+(or loadable-pcl bootable-pcl)
(progn
  (defvar *pcl-system-date* "$Date: 2010/02/08 17:15:53 $")
  (setf (getf *herald-items* :pcl)
	`("    CLOS based on Gerd's PCL " ,(if (>= (length *pcl-system-date*) 26)
					       (subseq *pcl-system-date* 7 26)
					       ""))))

