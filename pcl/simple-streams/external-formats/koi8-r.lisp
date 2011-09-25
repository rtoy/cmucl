;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/koi8-r.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

(defconstant +koi8-r+
  (make-array 128 :element-type '(unsigned-byte 16)
     :initial-contents #(9472 9474 9484 9488 9492 9496 9500 9508 9516 9524
			 9532 9600 9604 9608 9612 9616 9617 9618 9619 8992
			 9632 8729 8730 8776 8804 8805  160 8993  176  178
			  183  247 9552 9553 9554 1105 9555 9556 9557 9558
			 9559 9560 9561 9562 9563 9564 9565 9566 9567 9568
			 9569 1025 9570 9571 9572 9573 9574 9575 9576 9577
			 9578 9579 9580  169 1102 1072 1073 1094 1076 1077
			 1092 1075 1093 1080 1081 1082 1083 1084 1085 1086
			 1087 1103 1088 1089 1090 1091 1078 1074 1100 1099
			 1079 1096 1101 1097 1095 1098 1070 1040 1041 1062
			 1044 1045 1060 1043 1061 1048 1049 1050 1051 1052
			 1053 1054 1055 1071 1056 1057 1058 1059 1046 1042
			 1068 1067 1047 1064 1069 1065 1063 1066)))

(define-external-format :koi8-r (:base :mac-roman :documentation
"KOI8-R is an 8-bit character encoding designed to cover Russian.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +koi8-r+)))
