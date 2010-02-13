(in-package "PCL")
(intl:textdomain "cmucl")

#+(or loadable-pcl bootable-pcl)
(progn
  (defvar *pcl-system-date* "$Date: 2010/02/13 01:28:04 $")
  (setf (getf *herald-items* :pcl)
	`(,#'(lambda (stream)
	       (write-string _"    CLOS based on Gerd's PCL " stream))
	  ,(if (>= (length *pcl-system-date*) 26)
	       (subseq *pcl-system-date* 7 26)
	       ""))))

