;; Boot file to increase the max alignment value from 3 to 4.
;;
;; This takes care of everything for the bootstrap, but when new-assem
;; is loaded. you'll get a warning.  Select the CLOBBER-IT restart to
;; continue.

(in-package :new-assem)

(handler-bind
    ((error (lambda (c)
	      (declare (ignore c))
	      (invoke-restart 'continue))))
  (defconstant max-alignment 4))

(ext:without-package-locks
(deftype alignment ()
  `(integer 0 ,max-alignment))
)

(ext:without-package-locks
(handler-bind ((error (lambda (c)
			(declare (ignore c))
			(invoke-restart 'kernel::clobber-it))))
  (defstruct (segment
	      (:print-function %print-segment)
	      (:constructor make-segment (&key name run-scheduler inst-hook)))
    ;;
    ;; The name of this segment.  Only using in trace files.
    (name "Unnamed" :type simple-base-string)
    ;;
    ;; Whether or not run the scheduler.  Note: if the instruction defintions
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
    ;; ``Number'' for last instruction queued.  Used only to supply insts
    ;; with unique sset-element-number's.
    (inst-number 0 :type index)
    ;;
    ;; Simple-Vectors mapping locations to the instruction that reads them and
    ;; instructions that write them.
    (readers (make-array (assem-params-max-locations
			  (c:backend-assembler-params c:*backend*))
			 :initial-element nil)
     :type simple-vector)
    (writers (make-array (assem-params-max-locations
			  (c:backend-assembler-params c:*backend*))
			 :initial-element nil)
     :type simple-vector)
    ;;
    ;; The number of additional cycles before the next control transfer, or NIL
    ;; if a control transfer hasn't been queued.  When a delayed branch is
    ;; queued, this slot is set to the delay count.
    (branch-countdown nil :type (or null (and fixnum unsigned-byte)))
    ;;
    ;; *** These two slots are used both by the queuing noise and the
    ;; scheduling noise.
    ;;
    ;; All the instructions that are pending and don't have any unresolved
    ;; dependents.  We don't list branches here even if they would otherwise
    ;; qualify.  They are listed above.
    ;;
    (emittable-insts-sset (make-sset) :type sset)
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
    (delayed nil :type list)
    ;;
    ;; The emittable insts again, except this time as a list sorted by depth.
    (emittable-insts-queue nil :type list)
    ;;
    ;; Whether or not to collect dynamic statistics.  This is just the same as
    ;; *collect-dynamic-statistics* but is faster to reference.
    (collect-dynamic-statistics nil))))

