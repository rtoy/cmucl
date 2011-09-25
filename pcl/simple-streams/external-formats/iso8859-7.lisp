;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/iso8859-7.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

(defconstant +iso-8859-7+
  (make-array 96 :element-type '(unsigned-byte 16)
     :initial-contents #(160 8216 8217 163 65534 65534 166 167 168 169 65534
                         171 172 173 65534 8213 176 177 178 179 900 901 902 183
                         904 905 906 187 908 189 910 911 912 913 914 915 916
                         917 918 919 920 921 922 923 924 925 926 927 928 929
                         65534 931 932 933 934 935 936 937 938 939 940 941 942
                         943 944 945 946 947 948 949 950 951 952 953 954 955
                         956 957 958 959 960 961 962 963 964 965 966 967 968
                         969 970 971 972 973 974 65534)))

(define-external-format :iso8859-7 (:base :iso8859-2 :documentation
"ISO8859-7 is an 8-bit character encoding for the modern Greek
language.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +iso-8859-7+)))
