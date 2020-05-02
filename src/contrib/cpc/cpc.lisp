;;; cpc.lisp -- access the hardware performance counters on Solaris
;;
;; Author: Eric Marsden  <emarsden@laas.fr>
;; Time-stamp: <2004-01-03 emarsden>
;; Version: 0.5
;; Copying: This code is in the public domain.
;;
;;
;; This file provides access from CMUCL to the CPU Performance
;; Counters Library provided with Solaris 8. These routines allow you
;; to obtain measurements on certain low level performance measures,
;; such as the number of instructions and processor cycles required to
;; execute a certain bit of code, the number of cycles lost due to
;; incorrectly predicted branches, etc. Only tested on UltraSPARC, but
;; may also work on Solaris x86.
;;
;; Note that the user is responsible for sampling the performance
;; counters sufficiently frequently for them not to wrap. For more
;; information see libcpc(3), cputrack(1) and the SPARC manuals.
;;
;; See also the Perfmon tool
;;
;;    <URL:http://www.cps.msu.edu/~enbody/perfmon.html>
;;
;; and
;;
;;   <URL:http://developers.sun.com/tools/cc/articles/pcounters.html>
;;   <URL:http://developers.sun.com/solaris/articles/optimizing_apps.html>
;;   <URL:http://www.sun.com/microelectronics/shade/>
;;   <URL:http://www.ece.utexas.edu/projects/ece/lca/courses/tools.html>
;;
;;
;; Here is the meaning of the event specifier flags for UltraSPARC-II
;; (see the SPARC Architecture Manual for more information):
;;
;; +------------------------------------------------------------------+
;; |Cycle_cnt         |Accumulated cycles                             |
;; |Instr_cnt         |The number of instructions completed           |
;; |Dispatch0_IC_miss |Cycles I-buffer empty from I-Cache miss        |
;; |Dispatch0_mispred |Cycles I-buffer empty from branch misprediction|
;; |Dispatch0_storeBuf|Cycles store buffer full                       |
;; |Dispatch0_FP_use  |Cycles stalled waiting for fp dependency       |
;; |Load_use          |Cycles stalled waiting for load                |
;; |Load_use_RAW      |Cycles stalled on some weird internal condition|
;; |IC_ref            |I-Cache references                             |
;; |IC_hit            |I-Cache hits                                   |
;; |DC_rd             |D-Cache read references                        |
;; |DC_rd_hit         |D-Cache read hits                              |
;; |DC_wr             |D-Cache write references                       |
;; |DC_wr_hit         |D-Cache write hits                             |
;; |EC_ref            |E-Cache references                             |
;; |EC_hit            |E-Cache hits                                   |
;; |EC_write_hit_RDO  |See User's guide                               |
;; |EC_wb             |E-Cache misses that do writebacks              |
;; |EC_snoop_inv      |E-Cache invalidates                            |
;; |EC_snoop_cb       |E-Cache snoop copy-backs                       |
;; |EC_rd_hit         |E-Cache read hits from D-Cache misses          |
;; |EC_ic_hit         |E-Cache read hits from I-Cache misses          |
;; +------------------------------------------------------------------+
;;
;; Some of these event specifiers only work on one of the performance
;; counters (eg on pic1 but not pic0); see cputrack -h for details.


;; from cpc_bind_event(3):
;;
;; Sometimes, even the overhead of performing a system call will be too
;; disruptive to the events being measured. Once a call to
;; cpc_bind_event() has been issued, it is possible to directly access
;; the performance hardware registers from within the application. If the
;; performance counter context is active, then the counters will count on
;; behalf of the current LWP .
;; 
;; rd %pic, %rN        ! All UltraSPARC
;; wr %rN, %pic        ! (ditto, but see text)




;; TODO: add ability to flush the caches, in order to be able to
;; compare cold and warm execution times and CPI values. Convert some
;; of the examples provided with perfmon (matrix multiplication for
;; example).
;;
;; Use the ability of the UltraSPARC-III to signal a trap on overflow
;; of a performance counter.



;; The type of statistics that are useful to obtain include
;;
;;  - cycles per instruction (CPI)
;;  - decomposition into memory loads and stores
;;  - the separate contribution of memory latency to total CPI
;;    (depends on the program access pattern and on the memory
;;    hierarchy, establishes an upper bound on the possible improvement
;;    that could be acheived by using prefetching)
;;  - number of prefetches issued but never referenced (measures
;;    accuracy of address prediction algorithm)
;;  - the number of cache misses per 1000 instructions (with
;;    decomposition into i-cache, d-cache etc).
;;  - the number of cycles lost due to data and control hazards
;;
;; Benchmarks for C++ workloads on UltraSPARC:
;; <URL:http://www.ece.utexas.edu/~radhakri/hipc98/>
;;
;;   http://www.ciar.org/~ttk/public/WRL-TR-93.6.ps
;;   http://www.ciar.org/~ttk/public/ilp-limits.WRL-TN-15.ps
;;   http://www.ciar.org/~ttk/public/ilp-limits.WRL-TN-15.pdf



(defpackage :cpu-performance-counters
  (:nicknames :cpc)
  (:use :common-lisp
        :alien
        :c-call)
  (:export #:cpc-init
           #:cpc-string-to-event
           #:cpc-bind-event
           #:cpc-take-sample
           #:cpc-trace
           #:cpc-free))
(in-package :cpc)

(declaim (optimize (speed 3) (safety 1) (debug 0)))


(eval-when (:load-toplevel :execute)
  (sys::load-object-file "/usr/lib/libcpc.so"))


;; /usr/include/sys/time.h
(def-alien-type hrtime_t (alien:integer 64))
(def-alien-routine "gethrtime" hrtime_t)
(def-alien-routine "gethrvtime" hrtime_t)


;; /usr/include/sys/cpc_event.h
(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-alien-type cpc_event_t
    (struct _cpc_event
            (ce_cpuver int)
            (ce_hrt hrtime_t)
            (ce_tick (alien:unsigned 64))
            (ce_pic0 (alien:unsigned 64))
            (ce_pic1 (alien:unsigned 64))
            (ce_pcr (alien:unsigned 64))))

  (def-alien-type p_cpc_event_t (* cpc_event_t)))

(defparameter +cpu-version+ 0)

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +CPC_BIND_LWP_INHERIT+ #x1)
  (defconstant +CPC_BIND_EMT_OVF+ #x2)
  (defconstant +CPC_ULTRA1+ 1000)
  (defconstant +CPC_ULTRA2+ 1001)
  (defconstant +CPC_ULTRA3+ 1002)
  (defconstant +CPC_ULTRA3_PLUS+ 1003)
  (defconstant +CPC_ULTRA3_I+ 1004)
  (defconstant +CPC_PENTIUM+ 2000)
  (defconstant +CPC_PENTIUM_MMX+ 2001)
  (defconstant +CPC_PENTIUM_PRO+ 2002)
  (defconstant +CPC_PENTIUM_PRO_MMX+ 2003))

;; /usr/include/libcpc.h
(def-alien-routine ("cpc_version" %cpc_version) unsigned-int (ver unsigned-int))
(def-alien-routine ("cpc_getcpuver" %cpc_getcpuver) int)
(def-alien-routine ("cpc_getusage" %cpc_getusage) c-string
  (cpuver int))
(def-alien-routine ("cpc_strtoevent" %cpc_strtoevent) int
  (cpuver int)
  (spec c-string)
  (event (* cpc_event_t)))
(def-alien-routine ("cpc_eventtostr" %cpc_eventtostr) c-string
  (event (* cpc_event_t)))
(def-alien-routine ("cpc_access" %cpc_access) int)
(def-alien-routine ("cpc_bind_event" %cpc_bind_event) int
  (event (* cpc_event_t))
  (flags int))
(def-alien-routine ("cpc_take_sample" %cpc_take_sample) int
  (event (* cpc_event_t)))
(def-alien-routine ("cpc_event_accum" %cpc_event_accum) void
  (accum (* cpc_event_t))
  (event (* cpc_event_t)))
(def-alien-routine ("cpc_event_diff" %cpc_event_diff) void
  (diff (* cpc_event_t))
  (after (* cpc_event_t))
  (before (* cpc_event_t)))  
(def-alien-routine ("cpc_count_usr_events" %cpc_count_usr_events) int
  (enable int))
(def-alien-routine ("cpc_count_sys_events" %cpc_count_sys_events) int
  (enable int))
(def-alien-routine ("cpc_rele" %cpc_rele) int)




(define-condition cpc-condition (condition) ())
(define-condition cpc-error (cpc-condition) ())
(define-condition cpc-unavailable (cpc-error) ())
(define-condition cpc-noaccess (cpc-error) ())
(define-condition cpc-no-such-event (cpc-error) ())


(defun cpc-get-cpu-version ()
  (unless (zerop (%cpc_access))
    (error 'cpc-noaccess))
  (let ((res (%cpc_getcpuver)))
    (when (eql -1 res) (error 'cpc-unavailable))
    res))

(defun cpc-init ()
  (setf +cpu-version+ (cpc-get-cpu-version))
  (case +cpu-version+
    (#.+cpc_ultra1+  (pushnew :cpc-ultrasparc-1 *features*))
    (#.+cpc_ultra2+  (pushnew :cpc-ultrasparc-2 *features*))
    (#.+cpc_ultra3+  (pushnew :cpc-ultrasparc-3 *features*))
    (#.+cpc_ultra3_i+ (pushnew :cpc-ultrasparc-3i *features*))
    (#.+cpc_pentium+ (pushnew :cpc-pentium *features*)))
  +cpu-version+)


;; the caller should call CPC-FREE on the returned value
(defun cpc-string-to-event (spec)
  (let ((event (make-alien cpc_event_t)))
    (unless (zerop (%cpc_strtoevent +cpu-version+ spec event))
      (error 'cpc-no-such-event))
    event))

(defun cpc-bind-event (event)
  (unless (zerop (%cpc_bind_event event 0))
    (error 'cpc-error)))

;; this side-effects EVENT
(defun cpc-take-sample (event)
  (unless (zerop (%cpc_take_sample event))
    (error 'cpc-error)))

(defun cpc-event-diff (diff before after)
  (%cpc_event_diff diff before after))

(defun cpc-free (&rest events)
  (dolist (e events)
    (when (alien::alien-pointer-type-p e)
      (free-alien e))))


;;; adapted from code by Derek L Davies <ddavies@world.std.com> posted
;;; to cmucl-help@cons.org on 2000-07-13.

;;; Munged code from CMUCL's multiprocessing START-SIGALRM-YIELD
;;; function.  Might be dangerous: needs to be heavily tested.
;;;
;;; Examples:
;;;
;;; (with-timeout (5 (progn (format t "Timed out!~%") 'time-is-up))
;;;   (do ((i 0 (+ i 1))) ((> i 3) 'done)
;;;     (sleep 3) (format t "hi~%")))
;;; => TIME-IS-UP
;;;
;;; (with-timeout (30 (progn (format t "Timed out!~%") 'time-is-up))
;;;   (do ((i 0 (+ i 1))) ((> i 3) 'done)
;;;    (sleep 3) (format t "hi~%")))
;;; => DONE


(defvar *nested-with-timeout* nil)

(defmacro with-timeout ((timeout-symbol &rest if-timedout) &rest body)
  (let ((catch-tag (gensym))
        (secs (gensym))
        (usecs (gensym)))
    `(unwind-protect
	 (catch ',catch-tag
	   (if *nested-with-timeout*
	       (progn
		 (format *debug-io* "Nested with-timeout running under CMUCL.")
		 (setq *nested-with-timeout* nil))
	       (progn
		 (prog2
		     (progn
		       (setq *nested-with-timeout* t)
		       ;; Disable the gencgc pointer filter to improve interrupt safety.
		       #+gencgc
		       (setf (alien:extern-alien "enable_pointer_filter" alien:unsigned) 0)
		       (flet ((sigalrm-handler (signal code scp)
				(declare (ignore signal code scp))
				(when (<= lisp::*free-interrupt-context-index* 1)
				  (progn
				    (setq *nested-with-timeout* nil)
				    (throw ',catch-tag (progn ,@if-timedout))))))
			 (sys:enable-interrupt :sigalrm #'sigalrm-handler))
		       (multiple-value-bind (,secs ,usecs)
			   (floor (floor (* 1000000.0 ,timeout-symbol)) 1000000)
			 (unix:unix-setitimer :real 0 0 ,secs ,usecs)))
		     (progn ,@body)
		   ;; This resets the timer.  If we fail to do this we'll get a throw after
		   ;; we've exited the catch.
		   (unix:unix-setitimer :real 0 0 0 0)
		   (setq *nested-with-timeout* nil))))))))


;; Run FUNCTION with ARGS for MAX-SECONDS seconds, and return multiple
;; values delta-pic0 and delta-pic1.
(defun cpc-trace (event-spec function &key (max-seconds 10))
  (let ((event (cpc-string-to-event event-spec))
         delta-pic0 delta-pic1)
    (when (zerop +cpu-version+) (cpc-init))
    (with-alien ((before cpc_event_t)
                 (after cpc_event_t)
                 (diff cpc_event_t))
          (cpc-bind-event event)
          (cpc-take-sample (addr before))
          (with-timeout (max-seconds 'timeout)
            (funcall function))
          (cpc-take-sample (addr after))
          (cpc-event-diff (addr diff) (addr after) (addr before))
          (setf delta-pic0 (alien:slot diff 'ce_pic0))
          (setf delta-pic1 (alien:slot diff 'ce_pic1)))
       (cpc-free event)
       (values delta-pic0 delta-pic1)))


(defvar *pic0* 0)
(defvar *pic1* 0)
(defvar *old-cpc-event* (alien:make-alien cpc_event_t))

(defun cpc-sigalrm-handler (sig code scp)
  (declare (ignore sig code scp)
           (optimize speed))
  (let ((delta-pic0 0)
        (delta-pic1 0))
    (with-alien ((now cpc_event_t)
                 (diff cpc_event_t))
      (cpc-take-sample (addr now))
      (cpc-event-diff (addr diff) (addr now) *old-cpc-event*)
      (setf delta-pic0 (alien:slot diff 'ce_pic0))
      (setf delta-pic1 (alien:slot diff 'ce_pic1))
      ;; copy from now to *old-cpc-event*
      (dotimes (byte (alien:alien-size cpc_event_t :bytes))
        (setf (sys:sap-ref-8 (alien:alien-sap *old-cpc-event*) byte)
              (sys:sap-ref-8 (alien:alien-sap now) byte))))
    (incf *pic0* delta-pic0)
    (incf *pic1* delta-pic1)))

;; different version, that polls the counters every 3 seconds
(defun cpc-trace/poll (event-spec function &key (max-seconds 7))
  (let ((event (cpc-string-to-event event-spec))
        (elapsed-seconds 0))
    (assert (<= 3 max-seconds))
    (dotimes (byte (alien:alien-size cpc_event_t :bytes))
      (setf (sys:sap-ref-8 (alien:alien-sap *old-cpc-event*) byte) 0))
    ;; don't set to 0 to avoid division by zero
    (setf *pic0* 1
          *pic1* 1)
    (cpc-bind-event event)
    (sys:enable-interrupt :sigvtalrm #'cpc-sigalrm-handler)
    (unix:unix-setitimer :virtual 3 0 3 0)
    (cpc-take-sample *old-cpc-event*)
    (if max-seconds
        (with-timeout (max-seconds 'timeout)
          (funcall function))
        (funcall function))
    ;; should add an UNWIND-PROTECT
    (unix:unix-setitimer :virtual 0 0 0 0)
    (sys:ignore-interrupt :sigvtalrm)
    (funcall #'cpc-sigalrm-handler 0 0 0)
    (cpc-free event))
  (values *pic0* *pic1*))



;; some sample uses of the library to calculate various statistics

;; FIXME rewrite these in terms of a wrapper function that adds them to a list of available 
;; monitoring functions

(defun calculate-cpi (function)
  (cpc-init)
  (multiple-value-bind (pic0 pic1)
      (funcall #'cpc-trace/poll "pic0=Instr_cnt,pic1=Cycle_cnt" function)
    (float (/ pic1 pic0))))

(defun calculate-icache-miss (function)
  (cpc-init)
  (ecase +cpu-version+
    ((#.+cpc_ultra1+ #.+cpc_ultra2+)
     (multiple-value-bind (pic0 pic1)
         (funcall #'cpc-trace/poll "pic0=IC_ref,pic1=IC_hit" function)
       (float (/ (- pic0 pic1) pic0))))
    ((#.+cpc_ultra3+ #.+cpc_ultra3_plus+ #.+cpc_ultra3_i+)
     (multiple-value-bind (pic0 pic1)
         (funcall #'cpc-trace/poll "pic0=IC_ref,pic1=IC_miss" function)
       (float (/ pic1 pic0))))))

(defun calculate-ecache-miss (function)
  (cpc-init)
  (ecase +cpu-version+
    ((#.+cpc_ultra1+ #.+cpc_ultra2+)
     (multiple-value-bind (pic0 pic1)
         (funcall #'cpc-trace/poll "pic0=EC_ref,pic1=EC_hit" function)
       (float (/ (- pic0 pic1) pic0))))
    ((#.+cpc_ultra3+ #.+cpc_ultra3_plus+ #.+cpc_ultra3_i+)
     (multiple-value-bind (pic0 pic1)
         (funcall #'cpc-trace/poll "pic0=EC_ref,pic1=EC_misses" function)
       (float (/ pic1 pic0))))))

;; Counts the proportion of cycles stalled due to the event that no
;; instructions are issued because I-queue is empty from instruction
;; cache miss. This count includes L2-cache miss processing if a
;; L2-cache miss also occurs.
(defun calculate-instruction-stall (function)
  (cpc-init)
  (multiple-value-bind (pic0 pic1)
      (funcall #'cpc-trace/poll "pic0=Dispatch0_IC_miss,pic1=Cycle_cnt" function)
    (float (/ pic0 pic1))))

;; Counts the proportion of cycles stalled due to the event that no
;; instructions are issued because I-queue is empty due to branch
;; misprediction.
(defun calculate-mispredict-stall (function)
  (cpc-init)
  (multiple-value-bind (pic0 pic1)
      (funcall #'cpc-trace/poll "pic0=Cycle_cnt,pic1=Dispatch0_mispred" function)
    (float (/ pic1 pic0))))

(defun calculate-load-stall (function)
  (cpc-init)
  (ecase +cpu-version+
    ((#.+cpc_ultra1+ #.+cpc_ultra2+)
     (multiple-value-bind (pic0 pic1)
         (funcall #'cpc-trace/poll "pic0=Load_use,pic1=Cycle_cnt" function)
       (float (/ pic0 pic1))))
    ;; Rstall_IU_use is the number of cycles stalled because the
    ;; processor was waiting for an integer value to be generated.
    ;; There is also a corresponding Rstall_FP_use counter, which we ignore here. 
    ((#.+cpc_ultra3+ #.+cpc_ultra3_plus+ #.+cpc_ultra3_i+)
     (multiple-value-bind (pic0 pic1)
         (funcall #'cpc-trace/poll "pic0=Rstall_IU_use,pic1=Cycle_cnt" function)
       (float (/ pic0 pic1))))))


;; FIXME on US-III want to know Re_DC_miss for L1 & L2 cache misses


;; mostly uninteresting: this is always very close to 0
(defun calculate-stob-stall (function)
  (cpc-init)
  (case +cpu-version+
    ((#.+cpc_ultra1+ #.+cpc_ultra2+)
     (multiple-value-bind (pic0 pic1)
         (funcall #'cpc-trace/poll "pic0=Dispatch0_storeBuf,pic1=Cycle_cnt" function)
       (float (/ pic0 pic1))))))


;; approximately 90 cycles are required to handle a TLB miss on an UltraSPARC III processor
(defun calculate-DTLB-miss (function)
  (cpc-init)
  (ecase +cpu-version+
    ((#.+cpc_ultra3+ #.+cpc_ultra3_plus+ #.+cpc_ultra3_i+)
     (multiple-value-bind (pic0 pic1)
         (funcall #'cpc-trace/poll "pic0=DTLB_miss,pic1=Cycle_cnt" function)
       (float (/ pic0 pic1))))))


;; D-Cache Miss Rate = 0.000058   <- L1 cache miss
;; L2 miss rate = 0.024860   <- L2 cache miss
;; FP Stall rate = 0.286592    <- float pipeline stalls
;; DTLB miss rate = 0.299275    <- TLB misses rate (30%)
;; FLOPS = 2157.783478  <-  float operations per second
;; elapsed time = 85.420990 secs  <- total time to run
;; 
;;
;; pic0=DC_rd,pic1=DC_wr_miss
;; pic0=DC_wr,pic1=DC_wr_miss
;; pic1=Rstall_FP_use,pic0=Cycle_cnt
;; FM_pipe_completion,pic0=FA_pipe_completion (FLOPS)


;; === for UltraSPARC II ==
;;
;;         event0: Cycle_cnt Instr_cnt Dispatch0_IC_miss IC_ref DC_rd DC_wr 
;;                 EC_ref EC_snoop_inv Dispatch0_storeBuf Load_use 
;;                 EC_write_hit_RDO EC_rd_hit 
;; 
;;         event1: Cycle_cnt Instr_cnt Dispatch0_mispred EC_wb EC_snoop_cb 
;;                 Dispatch0_FP_use IC_hit DC_rd_hit DC_wr_hit Load_use_RAW 
;;                 EC_hit EC_ic_hit 
;;
;; === UltraSPARC III events ==
;;
;;       event0: Cycle_cnt Instr_cnt Dispatch0_IC_miss IC_ref DC_rd DC_wr 
;;                 EC_ref EC_snoop_inv Dispatch0_br_target Dispatch0_2nd_br 
;;                 Rstall_storeQ Rstall_IU_use EC_write_hit_RTO EC_rd_miss 
;;                 PC_port0_rd SI_snoop SI_ciq_flow SI_owned SW_count_0 
;;                 IU_Stat_Br_miss_taken IU_Stat_Br_count_taken 
;;                 Dispatch_rs_mispred FA_pipe_completion MC_reads_0 
;;                 MC_reads_1 MC_reads_2 MC_reads_3 MC_stalls_0 MC_stalls_2 
;; 
;;         event1: Cycle_cnt Instr_cnt Dispatch0_mispred EC_wb EC_snoop_cb 
;;                 IC_miss_cancelled Re_endian_miss Re_FPU_bypass Re_DC_miss 
;;                 Re_EC_miss IC_miss DC_rd_miss DC_wr_miss Rstall_FP_use 
;;                 EC_misses EC_ic_miss Re_PC_miss ITLB_miss DTLB_miss WC_miss 
;;                 WC_snoop_cb WC_scrubbed WC_wb_wo_read PC_soft_hit 
;;                 PC_snoop_inv PC_hard_hit PC_port1_rd SW_count_1 
;;                 IU_Stat_Br_miss_untaken IU_Stat_Br_count_untaken 
;;                 PC_MS_misses MC_writes_0 MC_writes_1 MC_writes_2 
;;                 MC_writes_3 MC_stalls_1 MC_stalls_3 Re_RAW_miss 
;;                 FM_pipe_completion 
;; 

;; EOF
