;;; -*- Package: NEW-ASSEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/new-assem.lisp,v 1.10 1992/07/27 16:49:38 hallgren Exp $")
;;;
;;; **********************************************************************
;;;
;;; Effecient retargetable scheduling assembler.
;;;
;;; Written by William Lott.
;;;
(in-package :new-assem)

(export '(emit-byte emit-skip emit-back-patch emit-chooser emit-postit
	  define-emitter define-instruction define-instruction-macro
	  def-assembler-params without-scheduling

	  segment make-segment segment-name assemble align inst
	  label label-p gen-label emit-label label-position
	  append-segment finalize-segment
	  segment-map-output release-segment))


;;;; Assembly control parameters.

(defstruct (assem-params
	    (:print-function %print-assem-params))
  (backend (ext:required-argument) :type c::backend)
  (scheduler-p nil :type (member t nil))
  (instructions (make-hash-table :test #'equal) :type hash-table))
;;;
(c::defprinter assem-params
  (backend :prin1 (c:backend-name backend)))

;;; DEF-ASSEMBLER-PARAMS -- Interface.
;;;
(defmacro def-assembler-params (&rest options)
  "Set up the assembler."
  `(eval-when (compile load eval)
     (setf (c:backend-assembler-params c:*target-backend*)
	   (make-assem-params :backend c:*target-backend*
			      ,@options))))


;;;; Constants.

;;; ASSEMBLY-UNIT-BITS -- Number of bits in the minimum assembly unit,
;;; (also refered to as a ``byte'').  Hopefully, different instruction
;;; sets won't require chainging this.
;;; 
(defconstant assembly-unit-bits 8)

(deftype assembly-unit ()
  `(unsigned-byte ,assembly-unit-bits))

;;; OUTPUT-BLOCK-SIZE -- The size (in bytes) to use per output block.  Each
;;; output block is a chunk of raw memory, pointed to by a sap.
;;;
(defconstant output-block-size (* 8 1024))

(deftype output-block-index ()
  `(integer 0 ,output-block-size))

;;; MAX-ALIGNMENT -- The maximum alignment we can guarentee given the object
;;; format.  If the loader only loads objects 8-byte aligned, we can't do
;;; any better then that ourselves.
;;;
(defconstant max-alignment 3)

(deftype alignment ()
  `(integer 0 ,max-alignment))

;;; MAX-INDEX -- The maximum an index will ever become.  Well, actually,
;;; just a bound on it so we can define a type.  There is no real hard
;;; limit on indexes, but we will run out of memory sometime.
;;; 
(defconstant max-index (1- most-positive-fixnum))

(deftype index ()
  `(integer 0 ,max-index))

;;; MAX-POSN -- Like MAX-INDEX, except for positions.
;;; 
(defconstant max-posn (1- most-positive-fixnum))

(deftype posn ()
  `(integer 0 ,max-posn))



;;;; The SEGMENT structure.

;;; SEGMENT -- This structure holds the state of the assembler.
;;; 
(defstruct (segment
	    (:print-function %print-segment)
	    (:constructor make-segment (&key name run-scheduler inst-hook)))
  ;;
  ;; The name of this segment.  Only using in trace files.
  (name "Unnamed" :type simple-base-string)
  ;;
  ;; Wether or not run the scheduler.  Note: if the instruction defintions
  ;; were not compiled with the scheduler turned on, this has no effect.
  (run-scheduler nil)
  ;;
  ;; If a function, then it is funcalled for each inst emitted with the
  ;; segment, the VOP, the name of the inst (as a string), and the inst
  ;; arguments.
  (inst-hook nil :type (or function null))
  ;;
  ;; Where to deposit the next byte.
  (fill-pointer (system:int-sap 0) :type system:system-area-pointer)
  ;;
  ;; Where the current output block ends.  If fill-pointer is ever sap= to
  ;; this, don't deposit a byte.  Move the fill pointer into a new block.
  (block-end (system:int-sap 0) :type system:system-area-pointer)
  ;;
  ;; What position does this correspond to.  Initially, positions and indexes
  ;; are the same, but after we start collapsing choosers, positions can change
  ;; while indexes stay the same.
  (current-posn 0 :type posn)
  ;;
  ;; Were in the output blocks are we currently outputing.
  (current-index 0 :type index)
  ;;
  ;; A vector of the output blocks.
  (output-blocks (make-array 4 :initial-element nil) :type simple-vector)
  ;;
  ;; A list of all the annotations that have been output to this segment.
  (annotations nil :type list)
  ;;
  ;; A pointer to the last cons cell in the annotations list.  This is
  ;; so we can quickly add things to the end of the annotations list.
  (last-annotation nil :type list)
  ;;
  ;; The number of bits of alignment at the last time we synchronized.
  (alignment max-alignment :type alignment)
  ;;
  ;; The position the last time we synchronized.
  (sync-posn 0 :type posn)
  ;;
  ;; The posn and index everything ends at.  This is not maintained while the
  ;; data is being generated, but is filled in after.  Basically, we copy
  ;; current-posn and current-index so that we can trash them while processing
  ;; choosers and back-patches.
  (final-posn 0 :type posn)
  (final-index 0 :type index)
  ;;
  ;; *** State used by the scheduler during instruction queueing.
  ;;
  ;; List of postit's.  These are accumulated between instructions.
  (postits nil :type list)
  ;;
  ;; A-lists mapping locations to the instruction that reads them and
  ;; instructions that write them.  If maintaining these as a-lists is
  ;; too expensive (consing, time, or whatever) we could represent this
  ;; information in a slot in the location.  But then the scheduler would not
  ;; be re-enterent.
  (readers nil :type list)
  (writers nil :type list)
  ;;
  ;; The number of additional cycles before the next control transfer, or NIL
  ;; if a control transfer hasn't been queued.  When a delayed branch is
  ;; queued, this slot is set to the delay count.
  (branch-countdown nil :type (or null (and fixnum unsigned-byte)))
  ;;
  ;; *** These two slots are used both by the queuing noise and the
  ;; scheduling noise.
  ;;
  ;; A list of all the instructions that are pending and don't have any
  ;; unresolved dependents.  We don't list branches here even if they would
  ;; otherwise qualify.  They are listed above.
  ;;
  (emittable-insts nil :type list)
  ;;
  ;; List of queued branches.  We handle these specially, because they have to
  ;; be emitted at a specific place (e.g. one slot before the end of the
  ;; block).
  (queued-branches nil :type list)
  ;;
  ;; *** State used by the scheduler duing instruction scheduling.
  ;;
  ;; The instructions who would have had a read dependent removed if it were
  ;; not for a delay slot.  This is a list of lists.  Each element in the
  ;; top level list corresponds to yet another cycle of delay.  Each element
  ;; in the second level lists is a dotted pair, holding the dependency
  ;; instruction and the dependent to remove.
  (delayed nil :type list))

(c::defprinter segment name)


;;;; Structures/types used by the scheduler.

(c:def-boolean-attribute instruction
  ;;
  ;; This attribute is set if the scheduler can freely flush this instruction
  ;; if it thinks it is not needed.  Examples are NOP and instructions that
  ;; have no side effect not described by the writes.
  flushable
  ;;
  ;; This attribute is set when an instruction can cause a control transfer.
  ;; For test instructions, the delay is used to determine how many
  ;; instructions follow the branch.
  branch
  )

(defstruct (instruction
	    (:print-function %print-instruction)
	    (:conc-name inst-)
	    (:constructor make-instruction (emitter attributes delay)))
  ;;
  ;; The function to envoke to actually emit this instruction.  Gets called
  ;; with the segment as its one argument.
  (emitter (required-argument) :type (or null function))
  ;;
  ;; The attributes of this instruction.
  (attributes (instruction-attributes) :type c:attributes)
  ;;
  ;; Number of instructions or cycles of delay before additional instructions
  ;; can read our writes.
  (delay 0 :type (and fixnum unsigned-byte))
  ;;
  ;; The maximum number of instructions in the longest dependency chain from
  ;; this instruction to one of the independent instructions.  This is used
  ;; as a heuristic at to which instructions should be scheduled first.
  (depth nil :type (or null (and fixnum unsigned-byte)))
  ;;
  ;; ** When trying remember which of the next four is which, note that the
  ;; ``read'' or ``write'' always referes to the dependent (second)
  ;; instruction.
  ;;
  ;; List of the instructions whos writes this instruction tries to read.
  (read-dependencies nil :type list)
  ;;
  ;; List of the instructions whos writes or reads are overwritten by this
  ;; instruction.
  (write-dependencies nil :type list)
  ;;
  ;; List of the instructions who write what we read or write.
  (write-dependents nil :type list)
  ;;
  ;; List of the instrucitons who read what we write.
  (read-dependents nil :type list))
;;;
(defvar *inst-ids* (make-hash-table :test #'eq))
(defvar *next-inst-id* 0)
(defun %print-instruction (inst stream depth)
  (declare (ignore depth))
  (print-unreadable-object (inst stream :type t :identity t)
    (princ (or (gethash inst *inst-ids*)
	       (setf (gethash inst *inst-ids*)
		     (incf *next-inst-id*)))
	   stream)
    (format stream " emitter=~S" (inst-emitter inst))
    (when (inst-depth inst)
      (format stream ", depth=~D" (inst-depth inst)))))

(defun reset-inst-ids ()
  (clrhash *inst-ids*)
  (setf *next-inst-id* 0))


;;;; Locations, and operations thereon.

(defun canonicalize-locations (locations)
  (let ((results nil))
    (labels ((grovel (location)
	       (typecase location
		 (list (dolist (x location) (grovel x)))
		 (c:tn (push (c:tn-location location) results))
		 (number) ; blow off immediates
		 (t (push location results)))))
      (grovel locations))
    results))



;;;; The scheduler itself.

;;; WITHOUT-SCHEDULING -- interface.
;;;
(defmacro without-scheduling ((&optional (segment '*current-segment*))
			      &body body)
  "Execute BODY (as a progn) without scheduling any of the instructions
   generated inside it.  DO NOT throw or return-from out of it."
  (let ((var (gensym))
	(seg (gensym)))
    `(let* ((,seg ,segment)
	    (,var (segment-run-scheduler ,seg)))
       (when ,var
	 (schedule-pending-instructions ,seg)
	 (setf (segment-run-scheduler ,seg) nil))
       ,@body
       (setf (segment-run-scheduler ,seg) ,var))))

;;; QUEUE-INST -- internal.
;;;
;;; This routine is called by due to uses of the INST macro when the scheduler
;;; is turned on.  We compute the change to the dependency graph and check to
;;; see if the basic block is terminated.
;;; 
(defun queue-inst (segment inst reads writes)
  #+debug (format *trace-output* "Queuing ~S~%" inst)
  (assert (segment-run-scheduler segment))
  (let* ((writers (segment-writers segment))
	 (readers (segment-readers segment))
	 (read-dependencies nil)
	 (write-dependencies nil)
	 (emittable-insts (segment-emittable-insts segment)))
    (dolist (read reads)
      (let ((writer (assoc read writers :test #'eq)))
	(when writer
	  (let ((prev-inst (cdr writer)))
	    (pushnew prev-inst read-dependencies)
	    (pushnew inst (inst-read-dependents prev-inst))
	    (setf emittable-insts
		  (delete prev-inst emittable-insts :test #'eq)))))
      (push (cons read inst) readers))
    (dolist (write writes)
      (let ((writer (assoc write writers :test #'eq)))
	(when writer
	  (let ((prev-inst (cdr writer)))
	    (pushnew prev-inst write-dependencies)
	    (pushnew inst (inst-write-dependents prev-inst))
	    (setf emittable-insts
		  (delete prev-inst emittable-insts :test #'eq)))
	  (setf writers (delete writer writers :test #'eq))))
      (let ((reader (assoc write readers :test #'eq)))
	(when reader
	  (let ((prev-inst (cdr reader)))
	    (unless (eq prev-inst inst)
	      (pushnew prev-inst write-dependencies)
	      (pushnew inst (inst-write-dependents prev-inst))
	      (setf emittable-insts
		    (delete prev-inst emittable-insts :test #'eq)))
	    (setf readers (delete reader readers :test #'eq))))))
    #+debug
    (when read-dependencies
      (format *trace-output* " Reads values produced by ~:S.~%"
	      read-dependencies))
    #+debug
    (when write-dependencies
      (format *trace-output* " Writes values used by ~:S.~%"
	      write-dependencies))
    (setf (inst-read-dependencies inst) read-dependencies)
    (setf (inst-write-dependencies inst) write-dependencies)
    (dolist (read reads)
      (push (cons read inst) readers))
    (setf (segment-readers segment) readers)
    (dolist (write writes)
      (push (cons write inst) writers))
    (setf (segment-writers segment) writers)
    (let ((countdown (segment-branch-countdown segment)))
      (cond ((instruction-attributep (inst-attributes inst) branch)
	     (unless countdown
	       (setf countdown (inst-delay inst)))
	     (push (cons countdown inst)
		   (segment-queued-branches segment)))
	    (t
	     (setf emittable-insts (cons inst emittable-insts))))
      (setf (segment-emittable-insts segment) emittable-insts)
      (when countdown
	(if (minusp (decf countdown))
	    (schedule-pending-instructions segment)
	    (setf (segment-branch-countdown segment) countdown)))))
  (ext:undefined-value))

;;; SCHEDULE-PENDING-INSTRUCTIONS -- internal.
;;;
;;; Emit all the pending instructions, and reset any state.  This is called
;;; whenever we hit a label (i.e. an entry point of some kind) and when the
;;; user turns the scheduler off (otherwise, the queued instructions would
;;; sit there until the scheduler was turned back on, and emitted in the
;;; wrong place).
;;; 
(defun schedule-pending-instructions (segment)
  (assert (segment-run-scheduler segment))
  ;;
  ;; Quick blow-out if nothing to do.
  (unless (or (segment-emittable-insts segment)
	      (segment-queued-branches segment))
    (return-from schedule-pending-instructions
		 (ext:undefined-value)))
  ;;
  ;; Grovel through the entire graph in the forward direction finding all
  ;; the leaf instructions.
  (labels ((grovel-inst (inst)
	     (let ((write-deps (inst-write-dependencies inst))
		   (read-deps (inst-read-dependencies inst))
		   (max 0))
	       (dolist (dep write-deps)
		 (let ((dep-depth (or (inst-depth dep) (grovel-inst dep))))
		   (when (> dep-depth max)
		     (setf max dep-depth))))
	       (dolist (dep read-deps)
		 (let ((dep-depth
			(+ (or (inst-depth dep) (grovel-inst dep))
			   (inst-delay dep))))
		   (when (> dep-depth max)
		     (setf max dep-depth))))
	       (cond ((and (null (inst-read-dependents inst))
			   (instruction-attributep (inst-attributes inst)
						   flushable)
			   (not (member inst (segment-writers segment)
					:key #'cdr :test #'eq)))
		      #+debug
		      (format *trace-output* "Flushing ~S~%" inst)
		      (setf (inst-emitter inst) nil)
		      (setf (inst-depth inst) max))
		     (t
		      (setf (inst-depth inst) #+nil (1+ max) max))))))
    (let ((really-emittable-insts nil)
	  (delayed nil))
      (dolist (inst (segment-emittable-insts segment))
	(grovel-inst inst)
	(if (zerop (inst-delay inst))
	    (push inst really-emittable-insts)
	    (setf delayed
		  (add-to-nth-list delayed inst (1- (inst-delay inst))))))
      (setf (segment-emittable-insts segment)
	    (sort really-emittable-insts #'> :key #'inst-depth))
      (setf (segment-delayed segment) delayed)))
  #+debug
  (format *trace-output* "Queued branches: ~S~%"
	  (segment-queued-branches segment))
  #+debug
  (format *trace-output* "Initially emittable: ~S~%"
	  (segment-emittable-insts segment))
  #+debug
  (format *trace-output* "Initially delayed: ~S~%"
	  (segment-delayed segment))
  ;;
  ;; Accumulate the results in reverse order.  Well, actually, this list will
  ;; be in forward order, because we are generating the reverse order in
  ;; reverse.
  (let ((results nil))
    ;;
    ;; Schedule all the branches in their exact locations.
    (let ((insts-from-end 0))
      (dolist (branch (segment-queued-branches segment))
	(dotimes (i (- (car branch) insts-from-end))
	  #+debug
	  (format *trace-output* "Filling branch delay slot...~%")
	  (let ((inst (or (schedule-one-inst segment) :nop)))
	    (push inst results))
	  (advance-one-inst segment)
	  (incf insts-from-end))
	(let ((inst (cdr branch)))
	  (note-resolved-dependencies segment inst)
	  (push inst results))
	#+debug
	(format *trace-output* "Emitting ~S~%" (cdr branch))
	(advance-one-inst segment)))
    ;;
    ;; Keep scheduling stuff until we run out.
    (loop
      (let ((inst (schedule-one-inst segment)))
	(unless inst
	  (return))
	(push inst results)
	(advance-one-inst segment)))
    ;;
    ;; Now call the emitters, but turn the scheduler off for the duration.
    (setf (segment-run-scheduler segment) nil)
    (dolist (inst results)
      (if (eq inst :nop)
	  (c:emit-nop segment)
	  (funcall (inst-emitter inst) segment)))
    (setf (segment-run-scheduler segment) t))
  ;;
  ;; Clear out any residue left over.
  (setf (segment-queued-branches segment) nil)
  (setf (segment-branch-countdown segment) nil)
  (setf (segment-readers segment) nil)
  (setf (segment-writers segment) nil)
  ;;
  ;; That's all folks.
  (ext:undefined-value))

;;; ADD-TO-NTH-LIST -- internal.
;;;
;;; Utility for maintaining the segment-delayed list.  We cdr down list
;;; n times (extending it if necessary) and then push thing on into the car
;;; of that cons cell.
;;; 
(defun add-to-nth-list (list thing n)
  (do ((cell (or list (setf list (list nil)))
	     (or (cdr cell) (setf (cdr cell) (list nil))))
       (i n (1- i)))
      ((zerop i)
       (push thing (car cell))
       list)))

;;; SCHEDULE-ONE-INST -- internal.
;;;
;;; Find the next instruction to schedule and return it after updating
;;; any dependency information.  If we can't do anything useful right
;;; now, but there is more work to be done, return :NOP to indicate that
;;; a nop must be emitted.  If we are all done, return NIL.
;;; 
(defun schedule-one-inst (segment)
  (let ((inst (pop (segment-emittable-insts segment))))
    (cond (inst
	   ;; We've got us a live one here.  Go for it.
	   (assert (null (inst-write-dependents inst)))
	   (assert (null (inst-read-dependents inst)))
	   #+debug
	   (format *Trace-output* "Emitting ~S~%" inst)
	   (note-resolved-dependencies segment inst)
	   ;; Are we wanting to flush this instruction?
	   (if (inst-emitter inst)
	       ;; Nope, it's still a go.  So return it.
	       inst
	       ;; Yep.  Find some other instruction to do.
	       (schedule-one-inst segment)))
	  ((segment-delayed segment)
	   ;; No emittable instructions, but we have more work to do.  Emit
	   ;; a NOP to fill in a delay slot.
	   #+debug (format *trace-output* "Emitting a NOP.~%")
	   :nop)
	  (t
	   ;; All done.
	   nil))))

;;; NOTE-RESOLVED-DEPENDENCIES -- internal.
;;;
;;; This function is called whenever an instruction has been scheduled, and we
;;; want to know what possibilities that opens up.  So look at all the
;;; instructions that this one depends on, and remove this instruction from
;;; their dependents list.  If we were the last dependent, then that
;;; dependency can be emitted now.
;;;
(defun note-resolved-dependencies (segment inst)
  (dolist (dep (inst-write-dependencies inst))
    ;; These are the instructions who have to be completed before our
    ;; write fires.  Doesn't matter how far before, just before.
    (let ((dependents (inst-write-dependents dep)))
      (setf dependents (delete inst dependents :test #'eq))
      (setf (inst-write-dependents dep) dependents)
      (when (and (null dependents)
		 (null (inst-read-dependents dep)))
	(insert-emittable-inst segment dep))))
  (dolist (dep (inst-read-dependencies inst))
    ;; These are the instructions who write values we read.  If there
    ;; is no delay, then just remove us from the dependent list.
    ;; Otherwise, record the fact that in n cycles, we should be
    ;; removed.
    (if (zerop (inst-delay dep))
	(let ((dependents (inst-read-dependents dep)))
	  (setf dependents (delete inst dependents :test #'eq))
	  (setf (inst-read-dependents dep) dependents)
	  (when (and (null dependents)
		     (null (inst-write-dependents dep)))
	    (insert-emittable-inst segment dep)))
	(setf (segment-delayed segment)
	      (add-to-nth-list (segment-delayed segment)
			       (cons dep inst)
			       (inst-delay dep)))))
  (ext:undefined-value))

;;; ADVANCE-ONE-INST -- internal.
;;;
;;; Process the next entry in segment-delayed.  This is called whenever anyone
;;; emits an instruction.
;;;
(defun advance-one-inst (segment)
  (let ((delayed-stuff (pop (segment-delayed segment))))
    (dolist (stuff delayed-stuff)
      (if (consp stuff)
	  (let* ((dependency (car stuff))
		 (dependent (cdr stuff))
		 (dependents (inst-read-dependents dependency)))
	    (setf dependents (delete dependent dependents :test #'eq))
	    (setf (inst-read-dependents dependency) dependents)
	    (when (and (null dependents)
		       (null (inst-write-dependents dependency)))
	      (insert-emittable-inst segment dependency)))
	  (insert-emittable-inst segment stuff)))))

;;; INSERT-EMITTABLE-INST -- internal.
;;;
;;; Note that inst is emittable by sticking it in the SEGMENT-EMITTABLE-INSTS
;;; list.  We keep the emittable-insts sorted with the largest ``depths''
;;; first.
;;;
(defun insert-emittable-inst (segment inst)
  #+debug
  (format *Trace-output* "Now emittable: ~S~%" inst)
  (do ((my-depth (inst-depth inst))
       (remaining (segment-emittable-insts segment) (cdr remaining))
       (prev nil remaining))
      ((or (null remaining) (> my-depth (inst-depth (car remaining))))
       (if prev
	   (setf (cdr prev) (cons inst remaining))
	   (setf (segment-emittable-insts segment) (cons inst remaining)))
       (ext:undefined-value))))


;;;; Structure used during output emission.

;;; ANNOTATION -- Common supertype for all the different kinds of annotations.
;;; 
(defstruct (annotation
	    (:constructor nil))
  ;;
  ;; Where in the raw output stream was this annotation emitted.
  (index 0 :type index)
  ;;
  ;; What position does that correspond to.
  (posn nil :type (or index null)))

;;; LABEL -- Doesn't need any additional information beyond what is in the
;;; annotation structure.
;;; 
(defstruct (label
	    (:include annotation)
	    (:constructor gen-label ())
	    (:print-function %print-label))
  )
;;;
(defun %print-label (label stream depth)
  (declare (ignore depth))
  (if (or *print-escape* *print-readably*)
      (print-unreadable-object (label stream :type t)
	(prin1 (c:label-id label) stream))
      (format stream "L~D" (c:label-id label))))

;;; ALIGNMENT-NOTE -- A constraint on how the output stream must be aligned.
;;; 
(defstruct (alignment-note
	    (:include annotation)
	    (:conc-name alignment-)
	    (:predicate alignment-p)
	    (:constructor make-alignment (bits size)))
  ;;
  ;; The minimum number of low-order bits that must be zero.
  (bits 0 :type alignment)
  ;;
  ;; The amount of filler we are assuming this alignment op will take.
  (size 0 :type (integer 0 #.(1- (ash 1 max-alignment)))))

;;; BACK-PATCH -- a reference to someplace that needs to be back-patched when
;;; we actually know what label positions, etc. are.
;;; 
(defstruct (back-patch
	    (:include annotation)
	    (:constructor make-back-patch (size function)))
  ;;
  ;; The area effected by this back-patch.
  (size 0 :type index)
  ;;
  ;; The function to use to generate the real data
  (function nil :type function))

;;; CHOOSER -- Similar to a back-patch, but also an indication that the amount
;;; of stuff output depends on label-positions, etc.  Back-patches can't change
;;; their mind about how much stuff to emit, but choosers can.
;;; 
(defstruct (chooser
	    (:include annotation)
	    (:constructor make-chooser
			  (size alignment maybe-shrink worst-case-fun)))
  ;;
  ;; The worst case size for this chooser.  There is this much space allocated
  ;; in the output buffer.
  (size 0 :type index)
  ;;
  ;; The worst case alignment this chooser is guarenteed to preserve.
  (alignment 0 :type alignment)
  ;;
  ;; The function to call to determine of we can use a shorter sequence.  It
  ;; returns NIL if nothing shorter can be used, or emits that sequence and
  ;; returns T.
  (maybe-shrink nil :type function)
  ;;
  ;; The function to call to generate the worst case sequence.  This is used
  ;; when nothing else can be condensed.
  (worst-case-fun nil :type function))

;;; FILLER -- Used internally when we figure out a chooser or alignment doesn't
;;; really need as much space as we initially gave it.
;;;
(defstruct (filler
	    (:include annotation)
	    (:constructor make-filler (bytes)))
  ;;
  ;; The number of bytes of filler here.
  (bytes 0 :type index))



;;;; Output buffer utility functions.

;;; A list of all the output-blocks we have allocated but aren't using.
;;; We free-list them because allocation more is slow and the garbage collector
;;; doesn't know about them, so it can't be slowed down by use keep ahold of
;;; them.
;;; 
(defvar *available-output-blocks* nil)

;;; A list of all the output-blocks we have ever allocated.  We don't really
;;; need to keep tract of this if RELEASE-OUTPUT-BLOCK were always called,
;;; but...
;;;
(defvar *all-output-blocks* nil)

;;; NEW-OUTPUT-BLOCK -- internal.
;;;
;;; Return a new output block, allocating one if necessary.
;;;
(defun new-output-block ()
  (if *available-output-blocks*
      (pop *available-output-blocks*)
      (let ((block (system:allocate-system-memory output-block-size)))
	(push block *all-output-blocks*)
	block)))

;;; RELEASE-OUTPUT-BLOCK -- internal.
;;;
;;; Return block to the list of avaiable blocks.
;;;
(defun release-output-block (block)
  (push block *available-output-blocks*))

;;; FORGET-OUTPUT-BLOCKS -- internal.
;;;
;;; We call this whenever a core starts up, because system-memory isn't
;;; saves with the core.  If we didn't, we would find our hands full of
;;; bogus SAPs, which would make all sorts of things unhappy.
;;;
(defun forget-output-blocks ()
  (setf *all-output-blocks* nil)
  (setf *available-output-blocks* nil))
;;;
(pushnew 'forget-output-blocks ext:*after-save-initializations*)



;;;; Output functions.

;;; FIND-NEW-FILL-POINTER -- internal.
;;;
;;; Find us a new fill pointer for the current index in segment.  Allocate
;;; any additional storage as necessary.
;;; 
(defun find-new-fill-pointer (segment)
  (declare (type segment segment))
  (let* ((index (segment-current-index segment))
	 (blocks (segment-output-blocks segment))
	 (num-blocks (length blocks)))
    (multiple-value-bind
	(block-num offset)
	(truncate index output-block-size)
      (when (>= block-num num-blocks)
	(setf blocks
	      (adjust-array blocks (+ block-num 3) :initial-element nil))
	(setf (segment-output-blocks segment) blocks))
      (let ((block (or (aref blocks block-num)
		       (setf (aref blocks block-num) (new-output-block)))))
	(setf (segment-block-end segment)
	      (system:sap+ block output-block-size))
	(setf (segment-fill-pointer segment) (system:sap+ block offset))))))

;;; EMIT-BYTE -- interface.
;;;
;;; Emit the supplied BYTE to SEGMENT, growing it if necessary.
;;; 
(declaim (inline emit-byte))
(defun emit-byte (segment byte)
  "Emit BYTE to SEGMENT."
  (declare (type segment segment)
	   (type (or assembly-unit (signed-byte #.assembly-unit-bits)) byte))
  (let* ((orig-ptr (segment-fill-pointer segment))
	 (ptr (if (system:sap= orig-ptr (segment-block-end segment))
		  (find-new-fill-pointer segment)
		  orig-ptr)))
    (setf (system:sap-ref-8 ptr 0) (ldb (byte assembly-unit-bits 0) byte))
    (setf (segment-fill-pointer segment) (system:sap+ ptr 1)))
  (incf (segment-current-posn segment))
  (incf (segment-current-index segment))
  (ext:undefined-value))

;;; EMIT-SKIP -- interface.
;;; 
(defun emit-skip (segment amount)
  "Output AMOUNT zeros (in bytes) to SEGMENT."
  (declare (type segment segment)
	   (type index amount))
  (dotimes (i amount)
    (emit-byte segment 0))
  (ext:undefined-value))

;;; EMIT-ANNOTATION -- internal.
;;;
;;; Used to handle the common parts of annotation emision.  We just
;;; assign the posn and index of the note and tack it on to the end
;;; of the segment's annotations list.
;;; 
(defun emit-annotation (segment note)
  (declare (type segment segment)
	   (type annotation note))
  (when (annotation-posn note)
    (error "Attempt to emit ~S for the second time."))
  (setf (annotation-posn note) (segment-current-posn segment))
  (setf (annotation-index note) (segment-current-index segment))
  (let ((last (segment-last-annotation segment))
	(new (list note)))
    (setf (segment-last-annotation segment)
	  (if last 
	      (setf (cdr last) new)
	      (setf (segment-annotations segment) new))))
  (ext:undefined-value))

;;; EMIT-BACK-PATCH -- interface.
;;; 
(defun emit-back-patch (segment size function)
  "Note that the instruction stream has to be back-patched when label positions
   are finally known.  SIZE bytes are reserved in SEGMENT, and function will
   be called with two arguments: the segment and the position.  The function
   should look at the position and the position of any labels it wants to
   and emit the correct sequence.  (And it better be the same size as SIZE).
   SIZE can be zero, which is useful if you just want to find out where things
   ended up."
  (emit-annotation segment (make-back-patch size function))
  (emit-skip segment size))

;;; EMIT-CHOOSER -- interface.
;;; 
(defun emit-chooser (segment size alignment maybe-shrink worst-case-fun)
  "Note that the instruction stream here depends on the actual positions of
   various labels, so can't be output until label positions are known.  Space
   is made in SEGMENT for at least SIZE bytes.  When all output has been
   generated, the MAYBE-SHRINK functions for all choosers are called with
   three arguments: the segment, the position, and a magic value.  The MAYBE-
   SHRINK decides if it can use a shorter sequence, and if so, emits that
   sequence to the segment and returns T.  If it can't do better than the
   worst case, it should return NIL (without emitting anything).  When calling
   LABEL-POSITION, it should pass it the position and the magic-value it was
   passed so that LABEL-POSITION can return the correct result.  If the chooser
   never decides to use a shorter sequence, the WORST-CASE-FUN will be called,
   just like a BACK-PATCH.  (See EMIT-BACK-PATCH.)"
  (declare (type segment segment) (type index size) (type alignment alignment)
	   (type function maybe-shrink worst-case-fun))
  (let ((chooser (make-chooser size alignment maybe-shrink worst-case-fun)))
    (emit-annotation segment chooser)
    (emit-skip segment size)
    (adjust-alignment-after-chooser segment chooser)))

;;; ADJUST-ALIGNMENT-AFTER-CHOOSER -- internal.
;;;
;;; Called in EMIT-CHOOSER and COMPRESS-SEGMENT in order to recompute the
;;; current alignment information in light of this chooser.  If the alignment
;;; guarenteed byte the chooser is less then the segments current alignment,
;;; we have to adjust the segments notion of the current alignment.
;;;
;;; The hard part is recomputing the sync posn, because it's not just the
;;; choosers posn.  Consider a chooser that emits either one or three words.
;;; It preserves 8-byte (3 bit) alignments, because the difference between
;;; the two choices is 8 bytes.
;;;
(defun adjust-alignment-after-chooser (segment chooser)
  (declare (type segment segment) (type chooser chooser))
  (let ((alignment (chooser-alignment chooser))
	(seg-alignment (segment-alignment segment)))
    (when (< alignment seg-alignment)
      ;; The chooser might change the alignment of the output.  So we have
      ;; to figure out what the worst case alignment could be.
      (setf (segment-alignment segment) alignment)
      (let* ((posn (chooser-posn chooser))
	     (sync-posn (segment-sync-posn segment))
	     (offset (- posn sync-posn))
	     (delta (logand offset (1- (ash 1 alignment)))))
	(setf (segment-sync-posn segment) (- posn delta)))))
  (ext:undefined-value))

;;; EMIT-FILLER -- internal.
;;;
;;; Used internally whenever a chooser or alignment decides it doesn't need
;;; as much space as it originally though.
;;;
(defun emit-filler (segment bytes)
  (let ((last (segment-last-annotation segment)))
    (cond ((and last (filler-p (car last)))
	   (incf (filler-bytes (car last)) bytes))
	  (t
	   (emit-annotation segment (make-filler bytes)))))
  (incf (segment-current-index segment) bytes)
  (setf (segment-fill-pointer segment) (system:int-sap 0))
  (setf (segment-block-end segment) (system:int-sap 0))
  (ext:undefined-value))

;;; %EMIT-LABEL -- internal.
;;;
;;; EMIT-LABEL (the interface) basically just expands into this, supplying
;;; the segment and vop.
;;; 
(defun %emit-label (segment vop label)
  (when (segment-run-scheduler segment)
    (schedule-pending-instructions segment))
  (let ((postits (segment-postits segment)))
    (setf (segment-postits segment) nil)
    (dolist (postit postits)
      (emit-back-patch segment 0 postit)))
  (let ((hook (segment-inst-hook segment)))
    (when hook
      (funcall hook segment vop :label label)))
  (emit-annotation segment label))

;;; EMIT-ALIGNMENT -- internal.
;;;
;;; Called by the ALIGN macro to emit an alignment note.  We check to see
;;; if we can guarentee the alignment restriction by just outputing a fixed
;;; number of bytes.  If so, we do so.  Otherwise, we create and emit
;;; an alignment note.
;;; 
(defun emit-alignment (segment vop bits)
  (when (segment-run-scheduler segment)
    (schedule-pending-instructions segment))
  (let ((hook (segment-inst-hook segment)))
    (when hook
      (funcall hook segment vop :align bits)))
  (let ((alignment (segment-alignment segment))
	(offset (- (segment-current-posn segment)
		   (segment-sync-posn segment))))
    (cond ((> bits alignment)
	   ;; We need more bits of alignment.  First emit enough noise
	   ;; to get back in sync with alignment, and then emit an alignment
	   ;; note to cover the rest.
	   (let ((slop (logand offset (1- (ash 1 alignment)))))
	     (unless (zerop slop)
	       (emit-skip segment (- (ash 1 alignment) slop))))
	   (let ((size (logand (1- (ash 1 bits))
			       (lognot (1- (ash 1 alignment))))))
	     (assert (> size 0))
	     (emit-annotation segment (make-alignment bits size))
	     (emit-skip segment size))
	   (setf (segment-alignment segment) bits)
	   (setf (segment-sync-posn segment) (segment-current-posn segment)))
	  (t
	   ;; The last alignment was more restrictive then this one.
	   ;; So we can just figure out how much noise to emit assuming
	   ;; the last alignment was met.
	   (let* ((mask (1- (ash 1 bits)))
		  (new-offset (logand (+ offset mask) (lognot mask))))
	     (emit-skip segment (- new-offset offset)))
	   ;; But we emit an alignment with size=0 so we can verify
	   ;; that everything works.
	   (emit-annotation segment (make-alignment bits 0)))))
  (ext:undefined-value))


;;; FIND-ALIGNMENT -- internal.
;;;
;;; Used to find how ``aligned'' different offsets are.  Returns the number
;;; of low-order 0 bits, up to MAX-ALIGNMENT.
;;; 
(defun find-alignment (offset)
  (dotimes (i max-alignment max-alignment)
    (when (logbitp i offset)
      (return i))))

;;; EMIT-POSTIT -- Internal.
;;;
;;; Emit a postit.  The function will be called as a back-patch with the
;;; position the following instruction is finally emitted.  Postits do not
;;; interfere at all with scheduling.
;;;
(defun %emit-postit (segment function)
  (push function (segment-postits segment))
  (ext:undefined-value))



;;;; Output compression/position assignment stuff

;;; COMPRESS-OUTPUT -- internal.
;;;
;;; Grovel though all the annotations looking for choosers.  When we find
;;; a chooser, invoke the maybe-shrink function.  If it returns T, it output
;;; some other byte sequence.
;;; 
(defun compress-output (segment)
  (dotimes (i 5) ; it better not take more than one or two passes.
    (let ((delta 0))
      (setf (segment-alignment segment) max-alignment)
      (setf (segment-sync-posn segment) 0)
      (do* ((prev nil)
	    (remaining (segment-annotations segment) next)
	    (next (cdr remaining) (cdr remaining)))
	   ((null remaining))
	(let* ((note (car remaining))
	       (posn (annotation-posn note)))
	  (unless (zerop delta)
	    (decf posn delta)
	    (setf (annotation-posn note) posn))
	  (cond
	   ((chooser-p note)
	    (setf (segment-current-index segment) (chooser-index note))
	    (setf (segment-current-posn segment) posn)
	    (setf (segment-fill-pointer segment) (system:int-sap 0))
	    (setf (segment-block-end segment) (system:int-sap 0))
	    (setf (segment-last-annotation segment) prev)
	    (cond
	     ((funcall (chooser-maybe-shrink note) segment posn delta)
	      ;; It emitted some replacement.
	      (let ((new-size (- (segment-current-index segment)
				 (chooser-index note)))
		    (old-size (chooser-size note)))
		(when (> new-size old-size)
		  (error "~S emitted ~D bytes, but claimed it's max was ~D"
			 note new-size old-size))
		(let ((additional-delta (- old-size new-size)))
		  (when (< (find-alignment additional-delta)
			   (chooser-alignment note))
		    (error "~S shrunk by ~D bytes, but claimed that it ~
			    preserve ~D bits of alignment."
			   note additional-delta (chooser-alignment note)))
		  (incf delta additional-delta)
		  (emit-filler segment additional-delta))
		(setf prev (segment-last-annotation segment))
		(if prev
		    (setf (cdr prev) (cdr remaining))
		    (setf (segment-annotations segment)
			  (cdr remaining)))))
	     (t
	      ;; The chooser passed on shrinking.  Make sure it didn't emit
	      ;; anything.
	      (unless (= (segment-current-index segment) (chooser-index note))
		(error "Chooser ~S passed, but not before emitting ~D bytes."
		       note
		       (- (segment-current-index segment)
			  (chooser-index note))))
	      ;; Act like we just emitted this chooser.
	      (let ((size (chooser-size note)))
		(incf (segment-current-index segment) size)
		(incf (segment-current-posn segment) size))
	      ;; Adjust the alignment accordingly.
	      (adjust-alignment-after-chooser segment note)
	      ;; And keep this chooser for next time around.
	      (setf prev remaining))))
	   ((alignment-p note)
	    (unless (zerop (alignment-size note))
	      ;; Re-emit the alignment, letting it collapse if we know anything
	      ;; more about the alignment guarentees of the segment.
	      (let ((index (alignment-index note)))
		(setf (segment-current-index segment) index)
		(setf (segment-current-posn segment) posn)
		(setf (segment-fill-pointer segment) (system:int-sap 0))
		(setf (segment-block-end segment) (system:int-sap 0))
		(setf (segment-last-annotation segment) prev)
		(emit-alignment segment nil (alignment-bits note))
		(let* ((new-index (segment-current-index segment))
		       (size (- new-index index))
		       (old-size (alignment-size note))
		       (additional-delta (- old-size size)))
		  (when (minusp additional-delta)
		    (error "Alignment ~S needs more space now?  It was ~D, ~
			    and is ~D now."
			   note old-size size))
		  (when (plusp additional-delta)
		    (emit-filler segment additional-delta)
		    (incf delta additional-delta)))
		(setf prev (segment-last-annotation segment))
		(if prev
		    (setf (cdr prev) (cdr remaining))
		    (setf (segment-annotations segment)
			  (cdr remaining))))))
	   (t
	    (setf prev remaining)))))
      (when (zerop delta)
	(return))
      (decf (segment-final-posn segment) delta)))
  (ext:undefined-value))

;;; FINALIZE-POSITIONS -- internal.
;;;
;;; We have run all the choosers we can, so now we have to figure out exactly
;;; how much space each alignment note needs.
;;; 
(defun finalize-positions (segment)
  (let ((delta 0))
    (do* ((prev nil)
	  (remaining (segment-annotations segment) next)
	  (next (cdr remaining) (cdr remaining)))
	 ((null remaining))
      (let* ((note (car remaining))
	     (posn (- (annotation-posn note) delta)))
	(cond
	 ((alignment-p note)
	  (let* ((bits (alignment-bits note))
		 (mask (1- (ash 1 bits)))
		 (new-posn (logand (+ posn mask) (lognot mask)))
		 (size (- new-posn posn))
		 (old-size (alignment-size note))
		 (additional-delta (- old-size size)))
	    (assert (<= 0 size old-size))
	    (unless (zerop additional-delta)
	      (setf (segment-last-annotation segment) prev)
	      (incf delta additional-delta)
	      (setf (segment-current-index segment) (alignment-index note))
	      (setf (segment-current-posn segment) posn)
	      (emit-filler segment additional-delta)
	      (setf prev (segment-last-annotation segment)))
	    (if prev
		(setf (cdr prev) next)
		(setf (segment-annotations segment) next))))
	 (t
	  (setf (annotation-posn note) posn)
	  (setf prev remaining)
	  (setf next (cdr remaining))))))
    (unless (zerop delta)
      (decf (segment-final-posn segment) delta)))
  (ext:undefined-value))

;;; PROCESS-BACK-PATCHES -- internal.
;;;
;;; Grovel over segment, filling in any backpatches.  If any choosers are left
;;; over, we need to emit their worst case varient.
;;;
(defun process-back-patches (segment)
  (do* ((prev nil)
	(remaining (segment-annotations segment) next)
	(next (cdr remaining) (cdr remaining)))
      ((null remaining))
    (let ((note (car remaining)))
      (flet ((fill-in (function old-size)
	       (let ((index (annotation-index note))
		     (posn (annotation-posn note)))
		 (setf (segment-current-index segment) index)
		 (setf (segment-current-posn segment) posn)
		 (setf (segment-fill-pointer segment) (system:int-sap 0))
		 (setf (segment-block-end segment) (system:int-sap 0))
		 (setf (segment-last-annotation segment) prev)
		 (funcall function segment posn)
		 (let ((new-size (- (segment-current-index segment) index)))
		   (unless (= new-size old-size)
		     (error "~S emitted ~D bytes, but claimed it's was ~D"
			    note new-size old-size)))
		 (let ((tail (segment-last-annotation segment)))
		   (if tail
		       (setf (cdr tail) next)
		       (setf (segment-annotations segment) next)))
		 (setf next (cdr prev)))))
	(cond ((back-patch-p note)
	       (fill-in (back-patch-function note)
			(back-patch-size note)))
	      ((chooser-p note)
	       (fill-in (chooser-worst-case-fun note)
			(chooser-size note)))
	      (t
	       (setf prev remaining)))))))


;;;; Interface to the rest of the compiler.

;;; *CURRENT-SEGMENT* -- internal.
;;;
;;; The holds the current segment while assembling.  Use ASSEMBLE to change
;;; it.
;;; 
(defvar *current-segment*)

;;; *CURRENT-VOP* -- internal.
;;;
;;; Just like *CURRENT-SEGMENT*, but holds the current vop.  Used only to keep
;;; track of which vops emit which insts.
;;; 
(defvar *current-vop* nil)

;;; ASSEMBLE -- interface.
;;;
;;; We also symbol-macrolet *current-segment* to a local holding the segment
;;; so uses of *current-segment* inside the body don't have to keep
;;; dereferencing the symbol.  Given that ASSEMBLE is the only interface to
;;; *current-segment*, we don't have to worry about the special value becomming
;;; out of sync with the lexical value.  Unless some bozo closes over it,
;;; but nobody does anything like that...
;;;
(defmacro assemble ((&optional segment vop &key labels) &body body
		    &environment env)
  "Execute BODY (as a progn) with SEGMENT as the current segment."
  (flet ((label-name-p (thing)
	   (and thing (symbolp thing))))
    (let* ((seg-var (gensym "SEGMENT-"))
	   (vop-var (gensym "VOP-"))
	   (visable-labels (remove-if-not #'label-name-p body))
	   (inherited-labels
	    (multiple-value-bind
		(expansion expanded)
		(macroexpand '..inherited-labels.. env)
	      (if expanded expansion nil)))
	   (new-labels (append labels
			       (set-difference visable-labels
					       inherited-labels)))
	   (nested-labels (set-difference (append inherited-labels new-labels)
					  visable-labels)))
      (when (intersection labels inherited-labels)
	(error "Duplicate nested labels: ~S"
	       (intersection labels inherited-labels)))
      `(let* ((,seg-var ,(or segment '*current-segment*))
	      (,vop-var ,(or vop '*current-vop*))
	      ,@(when segment
		  `((*current-segment* ,seg-var)))
	      ,@(when vop
		  `((*current-vop* ,vop-var)))
	      ,@(mapcar #'(lambda (name)
			    `(,name (gen-label)))
			new-labels))
	 (symbol-macrolet ((*current-segment* ,seg-var)
			   (*current-vop* ,vop-var)
			   ,@(when (or inherited-labels nested-labels)
			       `((..inherited-labels.. ,nested-labels))))
	   ,@(mapcar #'(lambda (form)
			 (if (label-name-p form)
			     `(emit-label ,form)
			     form))
		     body))))))

;;; INST -- interface.
;;; 
(defmacro inst (&whole whole instruction &rest args &environment env)
  "Emit the specified instruction to the current segment."
  (let ((inst (gethash (symbol-name instruction)
		       (assem-params-instructions
			(c:backend-assembler-params c:*target-backend*)))))
    (cond ((null inst)
	   (error "Unknown instruction: ~S" instruction))
	  ((functionp inst)
	   (funcall inst (cdr whole) env))
	  (t
	   `(,inst *current-segment* *current-vop* ,@args)))))

;;; EMIT-LABEL -- interface.
;;; 
(defmacro emit-label (label)
  "Emit LABEL at this location in the current segment."
  `(%emit-label *current-segment* *current-vop* ,label))

;;; EMIT-POSTIT -- interface.
;;;
(defmacro emit-postit (function)
  `(%emit-postit *current-segment* ,function))

;;; ALIGN -- interface.
;;; 
(defmacro align (bits)
  "Emit an alignment restriction to the current segment."
  `(emit-alignment *current-segment* *current-vop* ,bits))

;;; LABEL-POSITION -- interface.
;;; 
(defun label-position (label &optional if-after delta)
  "Return the current position for LABEL.  Chooser maybe-shrink functions
   should supply IF-AFTER and DELTA to assure correct results."
  (let ((posn (label-posn label)))
    (if (and if-after (> posn if-after))
	(- posn delta)
	posn)))

;;; APPEND-SEGMENT -- interface.
;;; 
(defun append-segment (segment other-segment)
  "Append OTHER-SEGMENT to the end of SEGMENT.  Don't use OTHER-SEGMENT
   for anything after this."
  (when (segment-run-scheduler segment)
    (schedule-pending-instructions segment))
  (let ((postits (segment-postits segment)))
    (setf (segment-postits segment) (segment-postits other-segment))
    (dolist (postit postits)
      (emit-back-patch segment 0 postit)))
  (emit-alignment segment nil max-alignment)
  (let ((offset-in-last-block (rem (segment-current-index segment)
				   output-block-size)))
    (unless (zerop offset-in-last-block)
      (emit-filler segment (- output-block-size offset-in-last-block))))
  (let* ((blocks (segment-output-blocks segment))
	 (next-block-num (floor (segment-current-index segment)
				output-block-size))
	 (other-blocks (segment-output-blocks other-segment)))
    (setf blocks
	  (adjust-array blocks (+ next-block-num (length other-blocks))))
    (setf (segment-output-blocks segment) blocks)
    (replace blocks other-blocks :start1 next-block-num))
  (let ((index-delta (segment-current-index segment))
	(posn-delta (segment-current-posn segment))
	(other-annotations (segment-annotations other-segment)))
    (setf (segment-current-index segment)
	  (+ index-delta (segment-current-index other-segment)))
    (setf (segment-current-posn segment)
	  (+ posn-delta (segment-current-posn other-segment)))
    (setf (segment-fill-pointer segment) (system:int-sap 0))
    (setf (segment-block-end segment) (system:int-sap 0))
    (when other-annotations
      (dolist (note other-annotations)
	(incf (annotation-index note) index-delta)
	(incf (annotation-posn note) posn-delta))
      (let ((last (segment-last-annotation segment)))
	(if last
	    (setf (cdr last) other-annotations)
	    (setf (segment-annotations segment) other-annotations))
	(setf (segment-last-annotation segment)
	      (segment-last-annotation other-segment)))))
  (ext:undefined-value))

;;; FINALIZE-SEGMENT -- interface.
;;; 
(defun finalize-segment (segment)
  "Does any final processing of SEGMENT and returns the total number of bytes
   covered by this segment."
  (when (segment-run-scheduler segment)
    (schedule-pending-instructions segment))
  (setf (segment-run-scheduler segment) nil)
  (let ((postits (segment-postits segment)))
    (setf (segment-postits segment) nil)
    (dolist (postit postits)
      (emit-back-patch segment 0 postit)))
  (setf (segment-final-index segment) (segment-current-index segment))
  (setf (segment-final-posn segment) (segment-current-posn segment))
  (setf (segment-inst-hook segment) nil)
  (compress-output segment)
  (finalize-positions segment)
  (process-back-patches segment)
  (segment-final-posn segment))

;;; SEGMENT-MAP-OUTPUT -- interface.
;;;
(defun segment-map-output (segment function)
  "Call FUNCTION on all the output accumulated in SEGMENT.  FUNCTION is called
   zero or more times with two arguments: a SAP and a number of bytes."
  (let ((old-index 0)
	(blocks (segment-output-blocks segment))
	(sap (system:int-sap 0))
	(end (system:int-sap 0)))
    (labels ((map-until (index)
	       (unless (system:sap< sap end)
		 (multiple-value-bind
		     (block-num block-offset)
		     (floor old-index output-block-size)
		   (let ((block (aref blocks block-num)))
		     (setf sap (system:sap+ block block-offset))
		     (setf end (system:sap+ block output-block-size)))))
	       (let* ((desired (- index old-index))
		      (available (system:sap- end sap))
		      (amount (min desired available)))
		 (funcall function sap amount)
		 (incf old-index amount)
		 (setf sap (system:sap+ sap amount))
		 (when (< amount desired)
		   (map-until index)))))
      (dolist (note (segment-annotations segment))
	(when (filler-p note)
	  (let ((index (filler-index note)))
	    (when (< old-index index)
	      (map-until index)))
	  (let ((bytes (filler-bytes note)))
	    (incf old-index bytes)
	    (setf sap (system:sap+ sap bytes)))))
      (let ((index (segment-final-index segment)))
	(when (< old-index index)
	  (map-until index))))))

;;; RELEASE-SEGMENT -- interface.
;;; 
(defun release-segment (segment)
  "Releases any output buffers held on to by segment."
  (let ((blocks (segment-output-blocks segment)))
    (loop
      for block across blocks
      do (when block
	   (release-output-block block))))
  (ext:undefined-value))


;;;; Interface to the instruction set definition.

;;; DEFINE-EMITTER -- Interface.
;;;
;;; Define a function named NAME that merges it's arguments into a single
;;; integer and then emits the bytes of that integer in the correct order
;;; based on the endianness of the target-backend.
;;;
(defmacro define-emitter (name total-bits &rest byte-specs)
  (ext:collect ((arg-names) (arg-types))
    (let* ((total-bits (eval total-bits))
	   (overall-mask (ash -1 total-bits))
	   (num-bytes (multiple-value-bind
			  (quo rem)
			  (truncate total-bits assembly-unit-bits)
			(unless (zerop rem)
			  (error "~D isn't an even multiple of ~D"
				 total-bits assembly-unit-bits))
			quo))
	   (bytes (make-array num-bytes :initial-element nil))
	   (segment-arg (gensym "SEGMENT-")))
      (dolist (byte-spec-expr byte-specs)
	(let* ((byte-spec (eval byte-spec-expr))
	       (byte-size (byte-size byte-spec))
	       (byte-posn (byte-position byte-spec))
	       (arg (gensym (format nil "~:@(ARG-FOR-~S-~)" byte-spec-expr))))
	  (when (ldb-test (byte byte-size byte-posn) overall-mask)
	    (error "Byte spec ~S either overlaps another byte spec, or ~
		    extends past the end."
		   byte-spec-expr))
	  (setf (ldb byte-spec overall-mask) -1)
	  (arg-names arg)
	  (arg-types `(type (integer ,(ash -1 (1- byte-size))
				     ,(1- (ash 1 byte-size)))
			    ,arg))
	  (multiple-value-bind
	      (start-byte offset)
	      (floor byte-posn assembly-unit-bits)
	    (let ((end-byte (floor (1- (+ byte-posn byte-size))
				   assembly-unit-bits)))
	      (flet ((maybe-ash (expr offset)
		       (if (zerop offset)
			   expr
			   `(ash ,expr ,offset))))
		(declare (inline maybe-ash))
		(cond ((zerop byte-size))
		      ((= start-byte end-byte)
		       (push (maybe-ash `(ldb (byte ,byte-size 0) ,arg)
					offset)
			     (svref bytes start-byte)))
		      (t
		       (push (maybe-ash
			      `(ldb (byte ,(- assembly-unit-bits offset) 0)
				    ,arg)
			      offset)
			     (svref bytes start-byte))
		       (do ((index (1+ start-byte) (1+ index)))
			   ((>= index end-byte))
			 (push
			  `(ldb (byte ,assembly-unit-bits
				      ,(- (* assembly-unit-bits
					     (- index start-byte))
					  offset))
				,arg)
			  (svref bytes index)))
		       (let ((len (rem (+ byte-size offset)
				       assembly-unit-bits)))
			 (push
			  `(ldb (byte ,(if (zerop len)
					   assembly-unit-bits
					   len)
				      ,(- (* assembly-unit-bits
					     (- end-byte start-byte))
					  offset))
				,arg)
			  (svref bytes end-byte))))))))))
      (unless (= overall-mask -1)
	(error "There are holes."))
      (let ((forms nil))
	(dotimes (i num-bytes)
	  (let ((pieces (svref bytes i)))
	    (assert pieces)
	    (push `(emit-byte ,segment-arg
			      ,(if (cdr pieces)
				   `(logior ,@pieces)
				   (car pieces)))
		  forms)))
	`(defun ,name (,segment-arg ,@(arg-names))
	   (declare (type segment ,segment-arg) ,@(arg-types))
	   ,@(ecase (c:backend-byte-order c:*target-backend*)
	       (:little-endian (nreverse forms))
	       (:big-endian forms))
	   ',name)))))

(defun grovel-lambda-list (lambda-list vop-var)
  (let ((segment-name (car lambda-list))
	(vop-var (or vop-var (gensym "VOP-"))))
    (ext:collect ((new-lambda-list))
      (new-lambda-list segment-name)
      (new-lambda-list vop-var)
      (labels
	  ((grovel (state lambda-list)
	     (when lambda-list
	       (let ((param (car lambda-list)))
		 (cond
		  ((member param lambda-list-keywords)
		   (new-lambda-list param)
		   (grovel param (cdr lambda-list)))
		  (t
		   (ecase state
		     ((nil)
		      (new-lambda-list param)
		      `(cons ,param ,(grovel state (cdr lambda-list))))
		     (&optional
		      (multiple-value-bind
			  (name default supplied-p)
			  (if (consp param)
			      (values (first param)
				      (second param)
				      (or (third param)
					  (gensym "SUPPLIED-P-")))
			      (values param nil (gensym "SUPPLIED-P-")))
			(new-lambda-list (list name default supplied-p))
			`(and ,supplied-p
			      (cons ,(if (consp name)
					 (second name)
					 name)
				    ,(grovel state (cdr lambda-list))))))
		     (&key
		      (multiple-value-bind
			  (name default supplied-p)
			  (if (consp param)
			      (values (first param)
				      (second param)
				      (or (third param)
					  (gensym "SUPPLIED-P-")))
			      (values param nil (gensym "SUPPLIED-P-")))
			(new-lambda-list (list name default supplied-p))
			(multiple-value-bind
			    (key var)
			    (if (consp name)
				(values (first name) (second name))
				(values (intern (symbol-name name) :keyword)
					name))
			  `(append (and ,supplied-p (list ',key ,var))
				   ,(grovel state (cdr lambda-list))))))
		     (&rest
		      (new-lambda-list param)
		      (grovel state (cdr lambda-list))
		      param))))))))
	(let ((reconstructor (grovel nil (cdr lambda-list))))
	  (values (new-lambda-list)
		  segment-name
		  vop-var
		  reconstructor))))))

(defun extract-nths (index glue list-of-lists-of-lists)
  (mapcar #'(lambda (list-of-lists)
	      (cons glue
		    (mapcar #'(lambda (list)
				(nth index list))
			    list-of-lists)))
	  list-of-lists-of-lists))

;;; DEFINE-INSTRUCTION -- interface.
;;; 
(defmacro define-instruction (name lambda-list &rest options)
  (let* ((sym-name (symbol-name name))
	 (defun-name (ext:symbolicate sym-name "-INST-EMITTER"))
	 (vop-var nil)
	 (postits (gensym "POSTITS-"))
	 (emitter nil)
	 (decls nil)
	 (attributes nil)
	 (reads nil)
	 (writes nil)
	 (delay nil)
	 (pinned nil)
	 (pdefs nil))
    (dolist (option-spec options)
      (multiple-value-bind
	  (option args)
	  (if (consp option-spec)
	      (values (car option-spec) (cdr option-spec))
	      (values option-spec nil))
	(case option
	  (:emitter
	   (when emitter
	     (error "Can only specify one emitter per instruction."))
	   (setf emitter args))
	  (:declare
	   (setf decls (append decls args)))
	  (:attributes
	   (setf attributes (append attributes args)))
	  (:reads
	   (when (or reads (cdr args))
	     (error
	      "Can only specify one reads expression per instruction."))
	   (setf reads (car args)))
	  (:writes
	   (when (or writes (cdr args))
	     (error
	      "Can only specify one writes expression per instruction."))
	   (setf writes (car args)))
	  (:delay
	   (when delay
	     (error "Can only specify delay once per instruction."))
	   (setf delay args))
	  (:pinned
	   (setf pinned t))
	  (:vop-var
	   (if vop-var
	       (error "Can only specify :vop-var once.")
	       (setf vop-var (car args))))
	  (:printer
	   (push
	    (eval
	     `(list
	       (multiple-value-list
		,(disassem:gen-printer-def-forms-def-form name
							  (cdr option-spec)))))
	    pdefs))
	  (:printer-list
	   ;; same as :printer, but is evaled first, and is a list of printers
	   (push
	    (eval
	     `(eval
	       `(list ,@(mapcar #'(lambda (printer)
				    `(multiple-value-list
				      ,(disassem:gen-printer-def-forms-def-form
					',name printer nil)))
				,(cadr option-spec)))))
	    pdefs))
	  (t
	   (error "Unknown option: ~S" option)))))
    (setf pdefs (nreverse pdefs))
    (multiple-value-bind
	(new-lambda-list segment-name vop-name arg-reconstructor)
	(grovel-lambda-list lambda-list vop-var)
      (push `(let ((hook (segment-inst-hook ,segment-name)))
	       (when hook
		 (funcall hook ,segment-name ,vop-name ,sym-name
			  ,arg-reconstructor)))
	    emitter)
      (push `(dolist (postit ,postits)
	       (emit-back-patch ,segment-name 0 postit))
	    emitter)
      (when (assem-params-scheduler-p
	     (c:backend-assembler-params c:*target-backend*))
	(if pinned
	    (setf emitter
		  `((when (segment-run-scheduler ,segment-name)
		      (schedule-pending-instructions ,segment-name))
		    ,@emitter))
	    (let ((flet-name
		   (gensym (concatenate 'string "EMIT-" sym-name "-INST-"))))
	      (setf emitter `((flet ((,flet-name (,segment-name)
				       ,@emitter))
				(if (segment-run-scheduler ,segment-name)
				    (queue-inst ,segment-name
						(make-instruction
						 #',flet-name
						 (instruction-attributes
						  ,@attributes)
						 (progn ,@delay))
						(canonicalize-locations
						 ,reads)
						(canonicalize-locations
						 ,writes))
				    (,flet-name ,segment-name))))))))
      `(progn
	 (defun ,defun-name ,new-lambda-list
	   ,@(when decls
	       `((declare ,@decls)))
	   (let ((,postits (segment-postits ,segment-name)))
	     (setf (segment-postits ,segment-name) nil)
	     (symbol-macrolet
		 ((*current-segment*
		   (macrolet ((lose ()
				(error "Can't use INST without an ASSEMBLE ~
					inside emitters.")))
		     (lose))))
	       ,@emitter))
	   (ext:undefined-value))
	 (eval-when (compile load eval)
	   (%define-instruction ,sym-name ',defun-name))
	 ,@(extract-nths 1 'progn pdefs)
	 (disassem:install-inst-flavors
	  ',name
	  (append ,@(extract-nths 0 'list pdefs)))))))

;;; DEFINE-INSTRUCTION-MACRO -- interface.
;;;
(defmacro define-instruction-macro (name lambda-list &body body)
  (let ((whole (gensym "WHOLE-"))
	(env (gensym "ENV-")))
    (multiple-value-bind
	(body local-defs)
	(lisp::parse-defmacro lambda-list whole body name 'instruction-macro
			      :environment env)
      `(eval-when (compile load eval)
	 (%define-instruction ,(symbol-name name)
			      #'(lambda (,whole ,env)
				  ,@local-defs
				  (block ,name
				    ,body)))))))

(defun %define-instruction (name defun)
  (setf (gethash name
		 (assem-params-instructions
		  (c:backend-assembler-params c:*target-backend*)))
	defun)
  name)

