(in-package "PCL")

#+(or loadable-pcl bootable-pcl)
(progn
  (defvar *pcl-system-date* "$Date: 2003/03/22 16:15:16 $")
  (setf (getf ext:*herald-items* :pcl)
	`("    CLOS based on Gerd's PCL " ,(subseq *pcl-system-date* 7 26))))

