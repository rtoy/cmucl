;;; -*- Mode: Lisp; Package: Compiler -*-

;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; The  DISASSEMBLE function as described in the Common Lisp manual.
;;; 
;;; Written by Don Mathis
;;;
;;;
;;; Modified 11/83 by Robert Rose to put an asterisk before lines
;;;  that are branched to.
;;;
;;; Heavily Modified 1/84 to use new instruction set.
;;;
;;; Modified by David B. McDonald to disassemble the Romp instruction
;;; set.
;;;
;;; Hacked by Rob MacLachlan for the interim RT function format.  Hopefully
;;; this file will die after the port.
;;;
;;; **********************************************************************
;;;
(in-package 'compiler :use '("LISP" "SYSTEM"))
(export 'lisp::disassemble (find-package 'lisp))

(proclaim '(special romp-4bit-opcode-symbol romp-8bit-opcode-symbol))

;;;;   The Main function, DISASSEMBLE

(defun disassemble (function &optional (*standard-output* *standard-output*))
  "The argument should be either a function object, a lambda expression, or
   a symbol with a function definition. If the relevant function is not a
   compiled function, it is first compiled. In any case, the compiled code
   is then 'reverse assembled' and printed out in a symbolic format."
  (etypecase function
    (function
     (ecase (%primitive get-vector-subtype function)
       ((#.%function-entry-subtype #.%function-closure-entry-subtype)
	(prin-prelim-info function)
	(Output-macro-instructions function (branch-list function)))
       (#.%function-closure-subtype
	(disassemble (%primitive header-ref function %function-name-slot)))))
    (symbol
     (disassemble (symbol-function function)))))



;;;   PRIN-PRELIM-INFO takes a function object and extracts from it and
;;; prints out the following information:
;;;   - The argument list of the function.
;;;   - The number of Locals allocated by the function.
;;;   - Whether the function does or does not evaluate its arguments.

(defun prin-prelim-info (function)
  (format t "~%Disassembly of ~S.~%"
	  (%primitive header-ref function %function-name-slot))
  (format t "~%Its arg list is: ~A.~%"
	  (%primitive header-ref function %function-entry-arglist-slot)))


;;;   OUTPUT-MACRO-INSTRUCTIONS takes a function object and prints out the
;;; corresponding macro. (Not executable macro, just macro that looks good!)
 
(defun Output-Macro-Instructions (function branches)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((byte-vector
	  (%primitive header-ref function %function-code-slot))
	 (offset
	  (- (%primitive header-ref function %function-offset-slot)
	     i-vector-header-size))
	 (vector-length (length byte-vector)))
    (declare (fixnum vector-length))
    (do ((i 0))
	((= i vector-length))
      (declare (fixnum i))
      (when (= i offset)
	(format t "~&*** Enter here:~%"))
      (let* ((opcode (aref byte-vector i))
	     (symbol (find-symbol-name opcode))
	     (inst-type (get symbol 'romp-instruction-type)))
	(case inst-type
	  (ji (print-ji-instruction opcode byte-vector i branches)
	      (setq i (the fixnum (+ i 2))))
	  (x (print-x-instruction opcode byte-vector i branches)
	     (setq i (the fixnum (+ i 2))))
	  (ds (print-ds-instruction opcode byte-vector i branches function)
	      (setq i (the fixnum (+ i 2))))
	  (r (print-r-instruction opcode byte-vector i branches)
	     (setq i (the fixnum (+ i 2))))
	  (bi (print-bi-instruction opcode byte-vector i branches)
	      (setq i (the fixnum (+ i 4))))
	  (ba (print-ba-instruction opcode byte-vector i branches)
	      (setq i (the fixnum  (+ i 4))))
	  (d (setq i (the fixnum
			  (+ i (the fixnum
				    (print-d-instruction opcode byte-vector i
							 branches function ))))))
	  (T (error "Illegal instruction type: ~A for instruction ~A.~%"
		    inst-type symbol)))))))


(defun find-symbol-name (opcode)
  (declare (fixnum opcode))
  (let ((symbol (svref romp-4bit-opcode-symbol (logand (the fixnum (ash opcode -4)) #xFF))))
    (if symbol
	symbol
	(svref romp-8bit-opcode-symbol opcode))))
		  

;;;   BRANCH-LIST is very much like output-macro-instructions,
;;;   but instead of actually creating all the instructions it just
;;;   creates the branch instruction labels.  A list of these labels
;;;   is returned.


(defun branch-list (function)
  (let* ((byte-vector
	  (%primitive header-ref function %function-code-slot))
	 (vector-length (length byte-vector))
	 (branches nil))
    (declare (fixnum vector-length))
    (do ((i 0))
	((>= i vector-length))
      (declare (fixnum i))
      (let* ((opcode (aref byte-vector i))
	     (symbol (find-symbol-name opcode))
             (inst-type (get symbol 'romp-instruction-type)))
	(case inst-type
	  (ji (push (the fixnum (+ i (the fixnum (sign-extend-ji byte-vector i)))) branches)
	      (setq i (the fixnum (+ i 2))))
	  ((x ds r) (setq i (the fixnum (+ i 2))))
	  (bi (push (the fixnum (+ i (the fixnum (sign-extend-bi byte-vector i)))) branches)
	      (setq i (the fixnum (+ i 4))))
	  ((ba d) (setq i (the fixnum (+ i 4))))
	  (T (error "Unknown instruction type: ~A, for instruction ~A.~%"
		    inst-type symbol)))))
    branches))

(defun print-ji-instruction (opcode byte-vector index branches)
  (declare (fixnum opcode index))
  (format t "~6D~A  (~A ~A ~A)~%"
	  index (if (memq index branches) "*" " ")
	  (if (= (logand opcode #x8) 0) "JNB" "JB")
	  (romp-condition-code (+ 8 (logand opcode #x7)))
	  `(**address** ,(the fixnum (+ index (the fixnum (sign-extend-ji byte-vector index)))))))

(defun print-x-instruction (opcode byte-vector index branches)
  (declare (fixnum opcode index))
  (let* ((rega (get-register-name (logand opcode #xF)))
	 (operand (aref byte-vector (the fixnum (1+ index))))
	 (regb (get-register-name (logand (the fixnum (ash operand -4)) #xF)))
	 (regc (get-register-name (logand operand #xF)))
	 (star (if (memq index branches) "*" " ")))
    (declare (fixnum operand))
    (if (eq regc 'NL0)
	(cond ((and (eq rega 'NL0) (eq regb 'NL0))
	       (format t "~6D~A  (LR NL0 NL0)		; Padding for previous execute instruction.~%"
		       index star))
	      (T (format t "~6D~A  (LR ~A ~A)~%"
			 index (if (memq index branches) "*" " ")
			 rega regb)))
	(format t "~6D~A  (CAS ~A ~A ~A)~%"
		index (if (memq index branches) "*" " ")
		rega regb regc))))

(defun print-ds-instruction (opcode byte-vector index branches function)
  (declare (fixnum opcode index))
  (let* ((symbol (find-symbol-name opcode))
	 (operand (aref byte-vector (the fixnum (1+ index))))
	 (rega (get-register-name (logand (the fixnum (ash operand -4)) #xF)))
	 (regb (get-register-name (logand operand #xF)))
	 (offset (ash (logand opcode #xF) 2)))
    (declare (fixnum offset operand))
    (cond ((and (memq symbol '(ls sts))
		(memq regb '(ENV CONT)))
	   (print-special-access symbol rega regb offset function
				 index branches))
	  (T (format t "~6D~A  (~A ~A ~A ~A)~%"
		     index (if (memq index branches) "*" " ")
		     (symbol-name symbol)
		     (get-register-name (logand (the fixnum (ash operand -4)) #xF))
		     (get-register-name (logand operand #xF))
		     (case symbol
		       ((ls sts) (ash (logand opcode #xF) 2))
		       ((lhs lhas sths) (ash (logand opcode #xF) 1))
		       (T (logand opcode #xF))))))))

(defun print-r-instruction (opcode byte-vector index branches)
  (declare (fixnum index opcode))
  (let* ((symbol (find-symbol-name opcode))
	 (operand (aref byte-vector (the fixnum (1+ index))))
	 (rega (logand (the fixnum (ash operand -4)) #xF))
	 (regb (logand operand #xF)))
    (declare (fixnum operand))
    (cond ((memq symbol '(inc dec lis mftbil mftbiu mttbil mttbiu
			      ais cis sis clrbl clrbu setbl setbu
			      sari sari16 sri sri16 srpi srpi16
			      sli sli16 slpi slpi16))
	   (format t "~6D~A  (~A ~A ~A)~%"
		   index (if (memq index branches) "*" " ")
		   (symbol-name symbol)
		   (get-register-name rega)
		   regb))
	  ((memq symbol '(bbr bbrx bnbr bnbrx))
	   (format t "~6D~A  (~A ~A ~A)~%"
		   index (if (memq index branches) "*" " ")
		   (symbol-name symbol)
		   (romp-condition-code rega)
		   (get-register-name regb)))
	  ((memq symbol '(mts mfs))
	   (format t "~6D~A  (~A ~A ~A)~%"
		   index (if (memq index branches) "*" " ")
		   (symbol-name symbol)
		   rega regb))
	  ((memq symbol '(clrsb setsb))
	   (format t "~6D~A  (~A ~A ~A)~%"
		   index (if (memq index branches) "*" " ")
		   (symbol-name symbol)
		   rega regb))
	  (T (format t "~6D~A  (~A ~A ~A)~%"
		     index (if (memq index branches) "*" " ")
		     (symbol-name symbol)
		     (get-register-name rega)
		     (get-register-name regb))))))

(defun print-bi-instruction (opcode byte-vector index branches)
  (declare (fixnum index opcode))
  (let* ((symbol (find-symbol-name opcode))
	 (cc (romp-condition-code
	      (logand (ash (the fixnum (aref byte-vector (the fixnum (1+ index)))) -4) #xF)))
	 (label (the fixnum (+ index (the fixnum (sign-extend-bi byte-vector index))))))
    (format t "~6D~A  (~A ~A ~A)~%"
	    index (if (memq index branches) "*" " ")
	    (symbol-name symbol) cc `(**address** ,label))))

(defun print-ba-instruction (opcode byte-vector index branches)
  (declare (fixnum index opcode))
  (let* ((symbol (find-symbol-name opcode))
	 (operand (the fixnum (logior (ash (the fixnum (aref byte-vector (the fixnum (1+ index)))) 16)
				      (ash (the fixnum (aref byte-vector (the fixnum (+ index 2)))) 8)
				      (the fixnum (aref byte-vector (the fixnum (+ index 3)))))))
	 (miscop (find-miscop-name operand)))
    (format t "~6D~A  (~A ~A)		; Call miscop ~A.~%"
	    index (if (memq index branches) "*" " ")
	    (symbol-name symbol)
	    miscop
	    miscop))
  4)

(defun print-d-instruction (opcode byte-vector index branches function)
  (declare (fixnum index opcode))
  (let* ((symbol (find-symbol-name opcode))
	 (operand (aref byte-vector (the fixnum (1+ index))))
	 (rega (get-register-name (logand (the fixnum (ash operand -4)) #xF)))
	 (regb (get-register-name (logand operand #xF)))
	 (offset (sign-extend-d byte-vector index)))
    (declare (fixnum offset operand))
    (cond ((eq symbol 'ci)
	   (format t"~6D~A  (~A ~A ~A)~%"
		   index (if (memq index branches) "*" " ")
		   (symbol-name symbol)
		   rega offset)
	   4)
	  ((and (memq symbol '(l st))
		(memq regb '(ENV CONT)))
	   (print-special-access symbol rega regb offset function
				 index branches)
	   4)
	  (T (format t "~6D~A  (~A ~A ~A ~A)~A~%"
		     index (if (memq index branches) "*" " ")
		     (symbol-name symbol)
		     rega regb offset
		     (if (eq (setq offset (logand offset #xFFFF)) nil-16)
			 "		; NIL."
			 (if (eq offset t-16)
			     "		; T."
			     "")))
	     4))))


(defun print-special-access (symbol rega regb offset function index branches)
  (declare (fixnum index offset))
  (let ((star (if (memq index branches) "*" " "))
	(*print-level* 3)
	(*print-length* 10))
    (cond
     ((not (eq regb 'ENV))
      (format t "~6D~A  (~A ~A ~A ~A)		; Stack slot ~D.~%"
	      index star (symbol-name symbol)
	      rega regb offset
	      (ash offset -2)))
     ((= offset (ash %function-code-slot 2))
      (format t "~6D~A  (~A ~A ~A ~A)		; Function code.~%"
	      index star (symbol-name symbol)
	      rega regb offset))
     ((= offset (ash %function-offset-slot 2))
      (format t "~6D~A  (~A ~A ~A ~A)		; Function offset.~%"
	      index star (symbol-name symbol)
	      rega regb offset))
     (t
      (format t "~6D~A  (~A ~A ~A ~A)		; Constant: ~S.~%"
	      index star (symbol-name symbol)
	      rega regb offset
	      (%primitive header-ref
			  (%primitive header-ref function
				      %function-entry-constants-slot)
			  (ash (the fixnum
				    (- offset g-vector-header-size)) -2)))))))


(defun sign-extend-ji (byte-vector index)
  (declare (fixnum index))
  (let ((byte (aref byte-vector (the fixnum (1+ index)))))
    (declare (fixnum byte))
    (ash (if (= (logand byte #x80) 0)
	     byte
	     (the fixnum (- (the fixnum (1+ (logand (lognot byte) #x7F))))))
	 1)))

(defun sign-extend-bi (byte-vector index)
  (declare (fixnum index))
  (let ((int (logior (the fixnum (ash (logand (the fixnum (aref byte-vector (the fixnum (1+ index)))) #xF) 16))
		     (the fixnum (ash (aref byte-vector (the fixnum (+ index 2))) 8))
		     (the fixnum (aref byte-vector (the fixnum (+ index 3)))))))
    (declare (fixnum int))
    (ash (if (= (logand int #x80000) 0)
	     int
	     (the fixnum (- (the fixnum (1+ (logand (lognot int) #x7FFFF))))))
	 1)))

(defun sign-extend-d (byte-vector index)
  (declare (fixnum index))
  (let ((hword (logior (the fixnum (ash (aref byte-vector (the fixnum (+ index 2))) 8))
		       (the fixnum (aref byte-vector (the fixnum (+ index 3)))))))
    (declare (fixnum hword))
    (if (= (logand hword #x8000) 0)
	hword
	(the fixnum (- (the fixnum (1+ (logand (lognot hword) #xFFFF))))))))

(defun romp-condition-code (cc)
  (if (<= 8 cc 16)
      (svref '#(pz lt eq gt cz reserved ov tb) (- cc 8))
      '??))

(defun get-register-name (reg)
  (svref '#(NL0 A0 NL1 A1 A3 A2 SP L0 L1 L2 L3 L4 BS CONT ENV PC) reg))

(defvar miscop-cache NIL)

(defun find-miscop-name (index)
  (if (null miscop-cache) (initialize-miscop-cache))
  (gethash index miscop-cache))

(defun initialize-miscop-cache ()
  (setq miscop-cache (make-hash-table :size 500))
  (do-symbols (x (find-package "CLC"))
    (let ((v (get x 'lisp::%loaded-address)))
      (when v
	(setf (gethash v miscop-cache) x))))

  (dolist (x lisp::*user-defined-miscops*)
    (setf (gethash (get x 'lisp::%loaded-address) miscop-cache) x)))
