;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/insts.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Description of the ARM architecture,
;;;
(in-package "SPARC")

(use-package "NEW-ASSEM")
(use-package "EXT")
(use-package "C")

(def-assembler-params
    :scheduler-p t
  :max-locations 101)			; TODO: How many locations?


;;;; Constants, types, conversion functions, some disassembler stuff.

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (zero zero-offset)
    (null null-offset)
    (t
     (if (eq (sb-name (sc-sb (tn-sc tn))) 'registers)
	 (tn-offset tn)
	 (error (intl:gettext "~S isn't a register.") tn)))))

(defun fp-reg-tn-encoding (tn)
  (declare (type tn tn))
  (unless (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers)
    (error (intl:gettext "~S isn't a floating-point register.") tn))
  (let ((offset (tn-offset tn)))
    (cond ((> offset 31)
	   ;; Use the sparc v9 double float register encoding.
	   (assert (backend-featurep :sparc-v9))
	   ;; No single register encoding greater than reg 31.
	   (assert (zerop (mod offset 2)))
	   ;; Upper bit of the register number is encoded in the low bit.
	   (1+ (- offset 32)))
	  (t
	   (tn-offset tn)))))

(disassem:set-disassem-params :instruction-alignment 32
			      :opcode-column-width 11)


(def-vm-support-routine location-number (loc)
  (etypecase loc
    (null)
    (number)
    (fixup)
    (tn
     (ecase (sb-name (sc-sb (tn-sc loc)))
       (registers
	(unless (zerop (tn-offset loc))
	  (tn-offset loc)))
       (float-registers
	(sc-case loc
	  (single-reg
	   (+ (tn-offset loc) 32))
	  (double-reg
	   (let ((offset (tn-offset loc)))
	     (assert (zerop (mod offset 2)))
	     (values (+ offset 32) 2)))
	  #+long-float
	  (long-reg
	   (let ((offset (tn-offset loc)))
	     (assert (zerop (mod offset 4)))
	     (values (+ offset 32) 4)))))
       (control-registers
	96)
       (immediate-constant
	nil)))
    (symbol
     (ecase loc
       (:memory 0)
       (:psr 97)
       (:fsr 98)
       (:y 99)
       (:tick 100)))))

;;; symbols used for disassembly printing
;;;
(defparameter reg-symbols
  (map 'vector
       #'(lambda (name)
	   (cond ((null name) nil)
		 (t (make-symbol (concatenate 'string "%" name)))))
       *register-names*)
  "The Lisp names for the ARM integer registers")

(defun get-reg-name (index)
  (if *disassem-use-lisp-reg-names*
      (aref reg-symbols index)
      (aref sparc-reg-symbols index)))


(eval-when (compile load eval)
(defun reg-arg-printer (value stream dstate)
  (declare (stream stream) (fixnum value))
  (let ((regname (get-reg-name value)))
    (princ regname stream)
    (disassem:maybe-note-associated-storage-ref value
						'registers
						regname
						dstate)
    (maybe-add-notes value dstate)))
) ; eval-when
      
(disassem:define-argument-type reg
  :printer #'reg-arg-printer)

(defparameter float-reg-symbols
  (coerce 
   (loop for n from 0 to 63 collect (make-symbol (format nil "%F~d" n)))
   'vector))

(disassem:define-argument-type fp-reg
  :printer #'(lambda (value stream dstate)
	       (declare (stream stream) (fixnum value))
	       (let ((regname (aref float-reg-symbols value)))
		 (princ regname stream)
		 (disassem:maybe-note-associated-storage-ref
		  value
		  'float-registers
		  regname
		  dstate))))

;; From DDI0406C_b, Table A8-1
(defconstant conditions-codes
  '(:eq :ne :cs :cc :mi :pl :vs :vc :hi :ls :ge :lt :gt :le :al))


;;; Define instruction formats. See DDI0406C_b, section A5 for details.
;;;

;; A5.2: Data-processing and misc instructions

(disassem:define-instruction-format
    (format-data-proc 32 :default-printer '())
  (cond :field (byte 4 28) :value #b1111)
  (zero :field (byte 2 26) :value 0)
  (op   :field (byte 1 25))
  (op1  :field (byte 5 20))
  (op2  :field (byte 4 4)))

;; A5.2.1: Data-processing (register)
(disassem:define-instruction-format
    (format-data-proc-reg 32 :default-printer '())
  (cond :field (byte 4 28) :value #b1111)
  (zero :field (byte 2 26) :value 0)
  (op   :field (byte 1 25))
  (op1  :field (byte 4 21))
  (s    :field (byte 1 20))
  (rn  :field (byte 4 16))
  (rd   :field (byte 4 12))
  (imm5 :field (byte 5 7))
  (type :field (byte 2 5))
  (z2   :field (byte 1 4) :value 0)
  (rm  :field (byte 4 0)))

;; A5.3 Load/store word and unsigned byte

(disassem:define-instruction-format
    (format-load-store-a0 32 :default-printer '())
  (cond  :field (byte 4 28) :value #b1111)
  (ls    :field (byte 2 26) :value 1)
  (a     :field (bye 1 25) :value 0)
  (op1   :field (byte 5 20))
  (rn    :field (byte 4 16))
  (rt    :field (byte 4 12))
  (imm12 :field (byte 0 12)))


(disassem:define-instruction-format
    (format-load-store-a1 32 :default-printer '())
  (cond :field (byte 4 28) :value #b1111)
  (ls   :field (byte 2 26) :value 1)
  (a    :field (bye 1 25) :value 1)
  (op1  :field (byte 5 20))
  (rn   :field (byte 4 16))
  (rt   :field (byte 4 12))
  (imm5 :field (byte 5 7))
  (type :field (byte 2 5))
  (z2   :field (byte 1 4))
  (rm   :field (byte 0 4)))

;; A5.5 Branch, branch with link, and block data transfer
(disassem:define-instruction-format
    (format-branch-link-bdt :default-printer '())
  (cond :field (byte 4 28) :value #b1111)
  (const :field (byte 2 26) :value 2)
  (op    :field (byte 6 20))
  (rn    :field (byte 4 16))
  (r     :field (byte 1 15))
  (rlist :field (byte 16 0))




