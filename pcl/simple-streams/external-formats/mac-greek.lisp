;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/mac-greek.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

(defconstant +mac-greek+
  (make-array 128 :element-type '(unsigned-byte 16)
     :initial-contents #(196 185 178 201 179 214 220 901 224 226 228 900 168
                         231 233 232 234 235 163 8482 238 239 8226 189 8240 244
                         246 166 173 249 251 252 8224 915 916 920 923 926 928
                         223 174 169 931 938 167 8800 176 903 913 177 8804 8805
                         165 914 917 918 919 921 922 924 934 939 936 937 940
                         925 172 927 929 8776 932 171 187 8230 160 933 935 902
                         904 339 8211 8213 8220 8221 8216 8217 247 905 906 908
                         910 941 942 943 972 911 973 945 946 968 948 949 966
                         947 951 953 958 954 955 956 957 959 960 974 961 963
                         964 952 969 962 967 965 950 970 971 912 944 65534)))

(define-external-format :mac-greek (:base :mac-roman :documentation
"MAC-GREEK is an 8-bit character encoding for Greek text on Apple
Macintosh computers.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +mac-greek+)))
