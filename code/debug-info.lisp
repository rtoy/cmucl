;;; -*- Log: code.log; Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains structures used for recording debugger information.
;;;
(in-package "C")

(defstruct location)

(defstruct (compiled-location (:include location))
  ;;
  ;; This variable's name and package, represented as strings.  This allows the
  ;; debugger to recover the symbol without requiring the symbol to be created
  ;; at load time.  Package-Name is NIL for gensyms.
  (name nil :type simple-string)
  (package-name nil :type (or simple-string null))
  ;;
  ;; The SC and offset, save SC and save offset encoded as bit-fields.  Also
  ;; some sort of ID that makes the name/package unique.  All locations for the
  ;; same var have the same ID.  A single variable will have multiple locations
  ;; (in different functions) when it is closed over.
  (bits0 0 :type fixnum)
  (bits1 0 :type fixnum)
  ;;
  ;; The variable's type, represented as list-style type descriptor.
  type)


(defmacro compiled-location-offset (str)
  `(ldb (byte 17 0) (compiled-location-bits0 ,str)))

(defmacro compiled-location-sc (str)
  `(ldb (byte 5 17) (compiled-location-bits0 ,str)))

(defmacro compiled-location-save-sc (str)
  `(ldb (byte 5 22) (compiled-location-bits0 ,str)))

(defmacro compiled-location-save-offset (str)
  `(ldb (byte 17 0) (compiled-location-bits1 ,str)))

(defmacro compiled-location-id (str)
  `(ldb (byte 10 17) (compiled-location-bits1 ,str)))


(defstruct debug-block)

;;; If a block has elsewhere (error) code, then there will be two debug-block
;;; structures to represent it: one for the normal code and one for the
;;; elsewhere code.
;;;
;;; ### Actually, this should probably all be represented in some packed binary
;;; form in an i-vector in the debug-function.
;;;
(defstruct (compiled-debug-block (:include debug-block))
  ;;
  ;; The PC that this block starts at.
  (pc nil :type index)
  ;;
  ;; Alternating offsets into the DEBUG-FUNCTION's source map and PC
  ;; increments, represented in some variable-length encoding.
  (forms nil :type (simple-array ??? (*)))
  ;;
  ;; A bit-vector indicating which vars are live somewhere in this block.  This
  ;; vector parallels the DEBUG-FUNCTION-VARIABLES, with a 1 for stored in each
  ;; location that corresponds to a live variable.
  (live-variables nil :type simple-bit-vector)
  ;;
  ;; If NIL, this block has no successor.  If an INDEX, the PC of this
  ;; block's successor.  If a CONS, then the block's two successors are the CAR
  ;; and CDR.
  (successor nil :type (or null index cons)))


(defstruct debug-function)

(defstruct (compiled-debug-function (:include debug-function))
  ;;
  ;; The name of this function.  If from a DEFUN, etc., then this is the
  ;; function name, otherwise it is a descriptive string.
  (name nil :type (or simple-string cons symbol))
  ;;
  ;; The kind of function (same as FUNCTIONAL-KIND):
  (kind nil :type (member nil :optional :external :top-level :cleanup))
  ;;
  ;; A vector of the Location structures for the variables that the argument
  ;; values are stored in within this function.  The Locations are in the order
  ;; that the arguments are actually passed in, but special marker symbols can
  ;; be interspersed to indicate the orignal call syntax:
  ;;
  ;; DELETED
  ;;    There was an argument to the function in this position, but it was
  ;;    deleted due to lack of references.  The value cannot be recovered.
  ;;
  ;; SUPPLIED-P
  ;;    The following location is the supplied-p value for the preceding
  ;;    keyword or optional.
  ;;
  ;; REST-ARG
  ;;    The following location holds the list of rest args.
  ;;
  ;; MORE-ARG
  ;;    The following two locations are the more arg context and count.
  ;;
  ;; <any other symbol>
  ;;    The following location is the value of the keyword argument with the
  ;;    specified name.
  ;;
  ;; This may be NIL to save space.
  (arguments nil :type (or simple-vector null))
  ;;
  ;; A vector of the locations of all the variables in this function,
  ;; alphabetically sorted by the NAME.  This may be NIL to save space.
  (variables nil :type (or simple-vector null))
  ;;
  ;; A vector of Locations describing the return locations, or :Standard if the
  ;; function returns using the standard unknown-values convention.  This may
  ;; be NIL to save space.  These locations only meaningful slots are OFFSET
  ;; and SC.
  (returns nil :type (or simple-vector (member :standard nil)))
  ;;
  ;; A vector of all the DEBUG-BLOCKs in this function, in the order they were
  ;; emitted.  The first block is the start of the function.  This may be NIL
  ;; to save space.
  (blocks nil :type (or simple-vector null))
  ;;
  ;; Register where the return PC and return CONT are kept, or NIL if they are
  ;; kept on the stack in the standard location.
  (return-pc nil :type (or (unsigned-byte 8) null))
  (old-cont nil :type (or (unsigned-byte 8) null)))


;;; ### We may ultimately want a vector of the start positions of each source
;;; form, since that would make it easier for the debugger to locate the
;;; source.
;;;
(defstruct debug-source
  ;;
  ;; This slot indicates where the definition came from:
  ;;    :File - from a file (Compile-File)
  ;;    :Lisp - from Lisp (Compile)
  ;;  :Stream - from a non-file stream (Compile-From-Stream)
  (from nil :type (member :file :stream :lisp))
  ;;
  ;; If :File, the file name, if :Lisp, the lambda compiled, otherwise some
  ;; descriptive string.
  (name nil :type (or pathname list simple-string))
  ;;
  ;; The universal time that the source was written, or NIL if unavailable.
  (created nil :type (or unsigned-byte null))
  ;;
  ;; The universal time that the source was compiled.
  (compiled nil :type unsigned-byte)
  ;;
  ;; The source path root number of the first form read from this source (i.e.
  ;; the total number of forms converted previously in this compilation.)
  (source-root 0 :type index)
  ;;
  ;; The file-position of the first form read from this source (if applicable).
  (start-position nil :type (or index null)))

(defstruct debug-info)

(defstruct (compiled-debug-info (:include debug-info))
  ;;
  ;; Some string describing something about the code in this component.
  (name nil :type simple-string)
  ;;
  ;; A list of DEBUG-SOURCE structures describing where the code for this
  ;; component came from, in the order that they were read.
  ;;
  ;; *** NOTE: the offset of this slot is wired into the fasl dumper so that it
  ;; *** can backpatch the source info when compilation is complete.
  (source nil :type list)
  ;;
  ;; A simple-vector of alternating Debug-Function structures and fixnum
  ;; PCs.  This is used to map PCs to functions, so that we can figure out
  ;; what function we were running in.  The function is valid between the PC
  ;; before it (inclusive) and the PC after it (exclusive).  The PCs are in
  ;; sorted order, so we can binary-search.  We omit the first and last PC,
  ;; since their values are 0 and the length of the code vector.  Null only
  ;; temporarily.
  (function-map nil :type (or simple-vector null))
  ;;
  ;; Representation of the tree of source paths for code in this component.
  (source-paths nil :type (or (simple-array (unsigned-byte 4) (*)) null)))
