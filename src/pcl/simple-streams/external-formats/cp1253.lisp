;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment "$Header: src/pcl/simple-streams/external-formats/cp1253.lisp $")

(in-package "STREAM")
(intl:textdomain "cmucl")

;; See http://unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1253.TXT
;;
;; For undefined characters we use U+FFFE

(defconstant +ms-cp1253+
  (make-array 128
              :element-type '(unsigned-byte 16)
              :initial-contents #(8364 65534 8218 402 8222 8230 8224 8225
                                  65534 8240 65534 8249 65534 65534 65534
                                  65534 65534 8216 8217 8220 8221 8226
                                  8211 8212 65534 8482 65534 8250 65534
                                  65534 65534 65534 160 901 902 163 164
                                  165 166 167 168 169 65534 171 172 173
                                  174 8213 176 177 178 179 900 181 182 183
                                  904 905 906 187 908 189 910 911 912 913
                                  914 915 916 917 918 919 920 921 922 923
                                  924 925 926 927 928 929 65534 931 932
                                  933 934 935 936 937 938 939 940 941 942
                                  943 944 945 946 947 948 949 950 951 952
                                  953 954 955 956 957 958 959 960 961 962
                                  963 964 965 966 967 968 969 970 971 972
                                  973 974 65534)))

(define-external-format :cp1253 (:base :mac-roman :documentation
"CP1253 is a Windows code page for Greek.

By default, illegal inputs are replaced by the Unicode replacement
character and illegal outputs are replaced by a question mark.")
  ((table +ms-cp1253+)))
