(in-package #:sacla-lisp-unit)
(define-test sacla-must-condition.1 (:tag :sacla)
 (assert-true (eq (signal "test signal") nil)))
(define-test sacla-must-condition.2 (:tag :sacla)
 (assert-true
  (eq
   (signal 'simple-error :format-control "simple-error" :format-arguments nil)
   nil)))
(define-test sacla-must-condition.3 (:tag :sacla)
 (assert-true
  (eq
   (signal 'simple-warning
           :format-control "simple-warning"
           :format-arguments nil)
   nil)))
(define-test sacla-must-condition.4 (:tag :sacla)
 (assert-true
  (handler-case (signal "test simple-condition")
    (simple-condition nil t)
    (condition nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.5 (:tag :sacla)
 (assert-true
  (handler-case
      (signal 'simple-warning
              :format-control "simple warning"
              :format-arguments nil)
    (simple-warning nil t)
    (warning nil nil)
    (condition nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.6 (:tag :sacla)
 (assert-true
  (handler-case (signal 'type-error :datum nil :expected-type 'vector)
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.7 (:tag :sacla)
 (assert-true
  (let ((*break-on-signals* 'arithmetic-error))
    (handler-case (signal 'type-error :datum nil :expected-type 'vector)
      (type-error nil t)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-condition.8 (:tag :sacla)
 (assert-true
  (handler-case (error "simple-error test")
    (simple-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.9 (:tag :sacla)
 (assert-true
  (handler-case (error 'type-error :datum nil :expected-type 'vector)
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.10 (:tag :sacla)
 (assert-true
  (handler-case (error 'no-such-error!!)
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.11 (:tag :sacla)
 (assert-true
  (handler-case
      (error 'simple-condition :format-control "simple-condition test")
    (simple-condition nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.12 (:tag :sacla)
 (assert-true
  (handler-case (error 'simple-warning :format-control "simple-warning test")
    (simple-warning nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.13 (:tag :sacla)
 (assert-true
  (handler-case (cerror "Continue." "error test")
    (simple-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.14 (:tag :sacla)
 (assert-true
  (handler-case
      (cerror "Continue." 'type-error :datum nil :expected-type 'vector)
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.15 (:tag :sacla)
 (assert-true
  (handler-bind
      ((simple-error
        #'(lambda (condition)
            (declare (ignore condition))
            (invoke-restart 'continue))))
    (eq (cerror "Continue." "error test") nil))))
(define-test sacla-must-condition.16 (:tag :sacla)
 (assert-true
  (handler-bind
      ((type-error
        #'(lambda (condition)
            (declare (ignore condition))
            (invoke-restart 'continue))))
    (eq (cerror "Continue." 'type-error :datum nil :expected-type 'vector)
        nil))))
(define-test sacla-must-condition.17 (:tag :sacla)
 (assert-true
  (let ((*error-output* (make-string-output-stream)))
    (and (eq (warn "I warn you!") nil)
         (get-output-stream-string *error-output*)))))
(define-test sacla-must-condition.18 (:tag :sacla)
 (assert-true
  (handler-bind
      ((warning
        #'(lambda (condition)
            (declare (ignore condition))
            (invoke-restart 'muffle-warning))))
    (eq (warn "I warn you!") nil))))
(define-test sacla-must-condition.19 (:tag :sacla)
 (assert-true
  (let ((*error-output* (make-string-output-stream)))
    (handler-bind
        ((warning
          #'(lambda (condition)
              (declare (ignore condition))
              (invoke-restart 'muffle-warning))))
      (and (eq (warn "I warn you!") nil)
           (string= (get-output-stream-string *error-output*) ""))))))
(define-test sacla-must-condition.20 (:tag :sacla)
 (assert-true
  (block tag
    (handler-case
        (warn 'simple-error :format-control "boom!" :format-arguments nil)
      (type-error nil t)
      (simple-error nil nil)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-condition.21 (:tag :sacla)
 (assert-true
  (block tag
    (handler-case
        (warn 'simple-condition :format-control "boom!" :format-arguments nil)
      (type-error nil t)
      (simple-condition nil nil)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-condition.22 (:tag :sacla)
 (assert-true
  (block tag
    (let ((condition
           (make-condition 'simple-condition
                           :format-control "boom!"
                           :format-arguments nil)))
      (handler-case (warn condition)
        (type-error nil t)
        (simple-condition nil nil)
        (error nil nil)
        (:no-error (&rest rest) (declare (ignore rest)) nil))))))
(define-test sacla-must-condition.23 (:tag :sacla)
 (assert-true
  (block tag
    (let ((condition
           (make-condition 'simple-error
                           :format-control "boom!"
                           :format-arguments nil)))
      (handler-case (warn condition)
        (type-error nil t)
        (simple-error nil nil)
        (error nil nil)
        (:no-error (&rest rest) (declare (ignore rest)) nil))))))
(define-test sacla-must-condition.24 (:tag :sacla)
 (assert-true
  (block tag
    (let ((condition
           (make-condition 'simple-warning
                           :format-control "boom!"
                           :format-arguments nil)))
      (handler-case (warn condition)
        (type-error nil nil)
        (simple-warning nil t)
        (error nil nil)
        (:no-error (&rest rest) (declare (ignore rest)) nil))))))
(define-test sacla-must-condition.25 (:tag :sacla)
 (assert-true
  (block tag
    (let ((condition
           (make-condition 'simple-warning
                           :format-control "boom!"
                           :format-arguments nil)))
      (handler-case
          (warn condition :format-control "boom!" :format-arguments nil)
        (type-error nil t)
        (simple-warning nil nil)
        (error nil nil)
        (:no-error (&rest rest) (declare (ignore rest)) nil))))))
(define-test sacla-must-condition.26 (:tag :sacla)
 (assert-true (null (handler-bind nil))))
(define-test sacla-must-condition.27 (:tag :sacla)
 (assert-true
  (handler-bind nil
    t)))
(define-test sacla-must-condition.28 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (handler-bind nil
      1
      2
      3
      (values 4 5 6)))
   '(4 5 6))))
(define-test sacla-must-condition.29 (:tag :sacla)
 (assert-true
  (eq 'handled
      (block tag
        (handler-bind
            ((type-error
              #'(lambda (c) (declare (ignore c)) (return-from tag 'handled))))
          (error 'type-error :datum nil :expected-type 'vector))))))
(define-test sacla-must-condition.30 (:tag :sacla)
 (assert-true
  (eq 'handled
      (block tag
        (handler-bind
            ((error
              #'(lambda (c) (declare (ignore c)) (return-from tag 'handled))))
          (error 'type-error :datum nil :expected-type 'vector))))))
(define-test sacla-must-condition.31 (:tag :sacla)
 (assert-true
  (eq 'handled
      (block tag
        (handler-bind
            ((condition
              #'(lambda (c) (declare (ignore c)) (return-from tag 'handled))))
          (error 'type-error :datum nil :expected-type 'vector))))))
(define-test sacla-must-condition.32 (:tag :sacla)
 (assert-true
  (eq 'outer-handler
      (block tag
        (handler-bind
            ((type-error
              #'(lambda (c)
                  (declare (ignore c))
                  (return-from tag 'outer-handler))))
          (handler-bind
              ((type-error #'(lambda (c) (error c)))
               (type-error
                #'(lambda (c)
                    (declare (ignore c))
                    (return-from tag 'inner-handler))))
            (error 'type-error :datum nil :expected-type 'vector)))))))
(define-test sacla-must-condition.33 (:tag :sacla)
 (assert-true
  (eq 'outer-handler
      (block tag
        (handler-bind
            ((error
              #'(lambda (c)
                  (declare (ignore c))
                  (return-from tag 'outer-handler))))
          (handler-bind
              ((type-error #'(lambda (c) (error c)))
               (type-error
                #'(lambda (c)
                    (declare (ignore c))
                    (return-from tag 'inner-handler))))
            (error 'type-error :datum nil :expected-type 'vector)))))))
(define-test sacla-must-condition.34 (:tag :sacla)
 (assert-true
  (eq 'left-handler
      (block tag
        (handler-bind
            ((type-error
              #'(lambda (c)
                  (declare (ignore c))
                  (return-from tag 'left-handler)))
             (type-error
              #'(lambda (c)
                  (declare (ignore c))
                  (return-from tag 'right-handler))))
          (error 'type-error :datum nil :expected-type 'vector))))))
(define-test sacla-must-condition.35 (:tag :sacla)
 (assert-true
  (eq 'left-handler
      (block tag
        (handler-bind
            ((error
              #'(lambda (c)
                  (declare (ignore c))
                  (return-from tag 'left-handler)))
             (type-error
              #'(lambda (c)
                  (declare (ignore c))
                  (return-from tag 'right-handler))))
          (error 'type-error :datum nil :expected-type 'vector))))))
(define-test sacla-must-condition.36 (:tag :sacla)
 (assert-true
  (eq 'left-handler
      (block tag
        (handler-bind
            ((type-error
              #'(lambda (c)
                  (declare (ignore c))
                  (return-from tag 'left-handler)))
             (error
              #'(lambda (c)
                  (declare (ignore c))
                  (return-from tag 'right-handler))))
          (error 'type-error :datum nil :expected-type 'vector))))))
(define-test sacla-must-condition.37 (:tag :sacla)
 (assert-true
  (let ((handler-declined nil))
    (and
     (eq
      (handler-bind
          ((type-error
            #'(lambda (c) (declare (ignore c)) (setq handler-declined t))))
        (signal 'type-error :datum nil :expected-type 'vector))
      nil)
     handler-declined))))
(define-test sacla-must-condition.38 (:tag :sacla)
 (assert-true
  (let ((handler-declined nil))
    (and
     (eq
      (handler-bind
          ((type-error
            #'(lambda (c)
                (declare (ignore c))
                (push 'outer handler-declined))))
        (handler-bind
            ((type-error
              #'(lambda (c)
                  (declare (ignore c))
                  (push 'inner handler-declined))))
          (signal 'type-error :datum nil :expected-type 'vector)))
      nil)
     (equal handler-declined '(outer inner))))))
(define-test sacla-must-condition.39 (:tag :sacla)
 (assert-true
  (let ((handler-declined nil))
    (and
     (eq
      (handler-bind
          ((type-error
            #'(lambda (c)
                (declare (ignore c))
                (push 'outer-left-handler handler-declined)))
           (type-error
            #'(lambda (c)
                (declare (ignore c))
                (push 'outer-right-handler handler-declined))))
        (handler-bind
            ((type-error
              #'(lambda (c)
                  (declare (ignore c))
                  (push 'inner-left-handler handler-declined)))
             (type-error
              #'(lambda (c)
                  (declare (ignore c))
                  (push 'inner-right-handler handler-declined))))
          (signal 'type-error :datum nil :expected-type 'vector)))
      nil)
     (equal handler-declined
            '(outer-right-handler outer-left-handler inner-right-handler
              inner-left-handler))))))
(define-test sacla-must-condition.40 (:tag :sacla)
 (assert-true
  (let ((handler-declined nil))
    (and
     (eq
      (handler-bind
          ((type-error
            #'(lambda (c)
                (declare (ignore c))
                (push 'outer-left-handler handler-declined)))
           (type-error
            #'(lambda (c)
                (declare (ignore c))
                (push 'outer-right-handler handler-declined))))
        (handler-bind
            ((type-error
              #'(lambda (c)
                  (declare (ignore c))
                  (push 'inner-left-handler handler-declined)))
             (type-error
              #'(lambda (c)
                  (signal c)
                  (push 'inner-right-handler handler-declined))))
          (signal 'type-error :datum nil :expected-type 'vector)))
      nil)
     (equal handler-declined
            '(outer-right-handler outer-left-handler inner-right-handler
              outer-right-handler outer-left-handler inner-left-handler))))))
(define-test sacla-must-condition.41 (:tag :sacla)
 (assert-true
  (let ((*dynamic-var* nil))
    (declare (special *dynamic-var*))
    (block tag
      (handler-bind
          ((type-error
            #'(lambda (c)
                (declare (ignore c))
                (return-from tag *dynamic-var*))))
        (let ((*dynamic-var* t))
          (declare (special *dynamic-var*))
          (signal 'type-error :datum nil :expected-type 'vector)))))))
(define-test sacla-must-condition.42 (:tag :sacla)
 (assert-true
  (let ((declined nil))
    (and
     (eq nil
         (handler-bind
             ((simple-condition
               #'(lambda (c) (declare (ignore c)) (push 'specific declined))))
           (handler-bind
               ((condition
                 #'(lambda (c) (declare (ignore c)) (push 'general declined))))
             (signal "error"))))
     (equal declined '(specific general))))))
(define-test sacla-must-condition.43 (:tag :sacla)
 (assert-true
  (block tag
    (handler-bind ((error #'(lambda (c) (return-from tag (typep c 'error)))))
      (error "error")))))
(define-test sacla-must-condition.44 (:tag :sacla)
 (assert-true
  (eq 'ok
      (block tag
        (handler-bind
            ((error #'(lambda (c) (declare (ignore c)) (return-from tag 'ok))))
          (handler-bind
              ((error #'(lambda (c) (declare (ignore c)) (error "error3"))))
            (handler-bind
                ((error #'(lambda (c) (declare (ignore c)) (error "error2"))))
              (error "error"))))))))
(define-test sacla-must-condition.45 (:tag :sacla)
 (assert-true
  (eq 'ok
      (block tag
        (handler-bind
            ((error
              #'(lambda (c)
                  (declare (ignore c))
                  (handler-bind
                      ((error
                        #'(lambda (c)
                            (declare (ignore c))
                            (handler-bind
                                ((error
                                  #'(lambda (c)
                                      (declare (ignore c))
                                      (return-from tag 'ok))))
                              (error "error2")))))
                    (error "error1")))))
          (error "error0"))))))
(define-test sacla-must-condition.46 (:tag :sacla)
 (assert-true (handler-case t)))
(define-test sacla-must-condition.47 (:tag :sacla)
 (assert-true
  (handler-case nil
    (:no-error (&rest rest) (declare (ignore rest)) t))))
(define-test sacla-must-condition.48 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (handler-case (values 0 1 2 3 4))) '(0 1 2 3 4))))
(define-test sacla-must-condition.49 (:tag :sacla)
 (assert-true
  (equal
   (handler-case (values 0 1 2 3 4)
     (:no-error (&rest rest) rest))
   '(0 1 2 3 4))))
(define-test sacla-must-condition.50 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (handler-case (values 0 1 2 3 4)
      (:no-error (&rest rest) (values rest 5 6 7 8))))
   '((0 1 2 3 4) 5 6 7 8))))
(define-test sacla-must-condition.51 (:tag :sacla)
 (assert-true
  (eq t
      (handler-case t
        (type-error nil 'type-error)
        (error nil 'error)))))
(define-test sacla-must-condition.52 (:tag :sacla)
 (assert-true
  (eq 'simple-error
      (handler-case (error "error!")
        (simple-error nil 'simple-error)
        (error nil 'error)))))
(define-test sacla-must-condition.53 (:tag :sacla)
 (assert-true
  (eq 'error
      (handler-case (error "error!")
        (error nil 'error)
        (simple-error nil 'simple-error)))))
(define-test sacla-must-condition.54 (:tag :sacla)
 (assert-true
  (eq 'error
      (handler-case (error "error!")
        (error nil 'error)
        (condition nil 'condition)
        (simple-error nil 'simple-error)))))
(define-test sacla-must-condition.55 (:tag :sacla)
 (assert-true
  (eq 'condition
      (handler-case (error "error!")
        (condition nil 'condition)
        (error nil 'error)
        (simple-error nil 'simple-error)))))
(define-test sacla-must-condition.56 (:tag :sacla)
 (assert-true
  (eq 'simple-error
      (handler-case
          (signal 'simple-error :format-control "error!" :format-arguments nil)
        (simple-error nil 'simple-error)
        (error nil 'error)))))
(define-test sacla-must-condition.57 (:tag :sacla)
 (assert-true
  (eq 'simple-error-left
      (handler-case
          (signal 'simple-error :format-control "error!" :format-arguments nil)
        (simple-error nil 'simple-error-left)
        (simple-error nil 'simple-error-right)))))
(define-test sacla-must-condition.58 (:tag :sacla)
 (assert-true
  (eq 'no-one-handled
      (handler-case
          (progn
            (signal 'simple-warning
                    :format-control "warning!"
                    :format-arguments nil)
            'no-one-handled)
        (simple-error nil 'simple-error)
        (error nil 'error)))))
(define-test sacla-must-condition.59 (:tag :sacla)
 (assert-true
  (equal
   (handler-case
       (progn
         (signal 'simple-warning
                 :format-control "warning!"
                 :format-arguments nil)
         'no-one-handled)
     (:no-error (&rest rest) (cons 'no-error rest))
     (simple-error nil 'simple-error)
     (error nil 'error))
   '(no-error no-one-handled))))
(define-test sacla-must-condition.60 (:tag :sacla)
 (assert-true
  (let ((where 'out))
    (eq
     (handler-case
         (let ((where 'in))
           (declare (ignorable where))
           (error "error!"))
       (error nil where))
     'out))))
(define-test sacla-must-condition.61 (:tag :sacla)
 (assert-true
  (let ((where 'out))
    (declare (special where))
    (eq
     (handler-case
         (let ((where 'in))
           (declare (special where))
           (error "~S" where))
       (error nil where))
     'out))))
(define-test sacla-must-condition.62 (:tag :sacla)
 (assert-true
  (typep
   (handler-case (error "error!")
     (error (c) c))
   'simple-error)))
(define-test sacla-must-condition.63 (:tag :sacla)
 (assert-true
  (typep
   (handler-case (error "error!")
     (condition (c) c))
   'simple-error)))
(define-test sacla-must-condition.64 (:tag :sacla)
 (assert-true
  (typep
   (handler-case (signal "condition")
     (condition (c) c))
   'simple-condition)))
(define-test sacla-must-condition.65 (:tag :sacla)
 (assert-true
  (typep
   (handler-case (warn "warning")
     (condition (c) c))
   'simple-warning)))
(define-test sacla-must-condition.66 (:tag :sacla)
 (assert-true (null (restart-bind nil))))
(define-test sacla-must-condition.67 (:tag :sacla)
 (assert-true (restart-bind nil t)))
(define-test sacla-must-condition.68 (:tag :sacla)
 (assert-true (= (restart-bind nil 0 1 2) 2)))
(define-test sacla-must-condition.69 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (restart-bind nil 0 1 2 (values 3 4 5)))
         '(3 4 5))))
(define-test sacla-must-condition.70 (:tag :sacla)
 (assert-true
  (block tag
    (restart-bind
     ((continue
       #'(lambda (&rest rest) (declare (ignore rest)) (return-from tag t))))
     (handler-case (signal "testing simple-signal")
       (simple-condition nil (invoke-restart 'continue)))))))
(define-test sacla-must-condition.71 (:tag :sacla)
 (assert-true
  (block tag
    (handler-bind
        ((simple-condition
          #'(lambda (condition)
              (declare (ignore condition))
              (invoke-restart 'continue))))
      (restart-bind
       ((continue
         #'(lambda (&rest rest) (declare (ignore rest)) (return-from tag t))))
       (signal "testing simple-condition"))))))
(define-test sacla-must-condition.72 (:tag :sacla)
 (assert-true
  (block tag
    (restart-bind
     ((continue
       #'(lambda (&rest rest) (declare (ignore rest)) (return-from tag nil))))
     (handler-bind
         ((simple-condition
           #'(lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'continue))))
       (restart-bind
        ((continue
          #'(lambda (&rest rest) (declare (ignore rest)) (return-from tag t))))
        (signal "testing simple-condition")))))))
(define-test sacla-must-condition.73 (:tag :sacla)
 (assert-true
  (block tag
    (restart-bind
     ((continue
       #'(lambda (&rest rest) (declare (ignore rest)) (return-from tag t)))
      (continue
       #'(lambda (&rest rest) (declare (ignore rest)) (return-from tag nil))))
     (handler-case (signal "testing simple-signal")
       (simple-condition nil (invoke-restart 'continue)))))))
(define-test sacla-must-condition.74 (:tag :sacla)
 (assert-true
  (block tag
    (restart-bind
     ((continue
       #'(lambda (&rest rest) (declare (ignore rest)) (return-from tag t))
       :report-function #'(lambda (stream) (format stream "Continue"))))
     (handler-case (signal "testing simple-signal")
       (simple-condition nil (invoke-restart 'continue)))))))
(define-test sacla-must-condition.75 (:tag :sacla)
 (assert-true
  (block tag
    (restart-bind
     ((continue #'(lambda (x) (return-from tag x))
                :report-function #'(lambda (stream) (format stream "Continue"))
                :interactive-function #'(lambda () (list t))))
     (handler-case (signal "testing simple-signal")
       (simple-condition nil (invoke-restart-interactively 'continue)))))))
(define-test sacla-must-condition.76 (:tag :sacla)
 (assert-true
  (eq 'ok
      (block tag
        (restart-bind ((continue #'(lambda (x) (return-from tag x))))
                      (handler-case (signal "testing simple-signal")
                        (simple-condition nil
                         (invoke-restart 'continue 'ok))))))))
(define-test sacla-must-condition.77 (:tag :sacla)
 (assert-true
  (block tag
    (restart-bind
     ((continue #'(lambda (x) (return-from tag x))
                :report-function #'(lambda (stream) (format stream "Continue"))
                :interactive-function #'(lambda () (list t))
                :test-function (constantly t)))
     (handler-case (signal "testing simple-signal")
       (simple-condition nil (invoke-restart-interactively 'continue)))))))
(define-test sacla-must-condition.78 (:tag :sacla)
 (assert-true
  (block tag
    (restart-bind
     ((continue #'(lambda (x) (return-from tag x))
                :report-function #'(lambda (stream) (format stream "Continue"))
                :interactive-function #'(lambda () (list t))
                :test-function #'(lambda (c)
                                   (or (null c) (typep c 'simple-condition)))))
     (handler-case (signal "testing simple-signal")
       (simple-condition nil (invoke-restart-interactively 'continue)))))))
(define-test sacla-must-condition.79 (:tag :sacla)
 (assert-true
  (block tag
    (restart-bind
     ((tb-continue #'(lambda (x) (return-from tag x)) :interactive-function
       #'(lambda () (list t)) :test-function (constantly nil) :report-function
       #'(lambda (stream) (format stream "Continue"))))
     (not (find-restart 'tb-continue))))))
(define-test sacla-must-condition.80 (:tag :sacla)
 (assert-true
  (block tag
    (restart-bind
     ((tb-continue #'(lambda (x) (return-from tag x)) :interactive-function
       #'(lambda () (list t)) :test-function (constantly t) :report-function
       #'(lambda (stream) (format stream "cont."))))
     (handler-case (signal "testing simple-signal")
       (simple-condition nil (invoke-restart-interactively 'tb-continue)))))))
(define-test sacla-must-condition.81 (:tag :sacla)
 (assert-true
  (null
   (let ((*dynamic-var* nil))
     (declare (special *dynamic-var*))
     (block tag
       (restart-bind
        ((continue
          #'(lambda (x) (declare (ignore x)) (return-from tag *dynamic-var*))
          :interactive-function #'(lambda () (list t))
          :test-function (constantly t)
          :report-function #'(lambda (stream) (format stream "cont."))))
        (handler-case
            (let ((*dynamic-var* t))
              (declare (special *dynamic-var*))
              (signal "testing simple-signal"))
          (simple-condition nil (invoke-restart-interactively 'continue)))))))))
(define-test sacla-must-condition.82 (:tag :sacla)
 (assert-true
  (let ((*dynamic-var* nil))
    (declare (special *dynamic-var*))
    (block tag
      (restart-bind
       ((continue
         #'(lambda (x) (declare (ignore x)) (return-from tag *dynamic-var*))
         :interactive-function #'(lambda () (list t))
         :test-function (constantly t)
         :report-function #'(lambda (stream) (format stream "cont."))))
       (handler-bind
           ((simple-condition
             #'(lambda (c)
                 (declare (ignore c))
                 (invoke-restart-interactively 'continue))))
         (let ((*dynamic-var* t))
           (declare (special *dynamic-var*))
           (signal "testing simple-signal"))))))))
(define-test sacla-must-condition.83 (:tag :sacla)
 (assert-true
  (block tag
    (restart-bind
     ((nil
       #'(lambda (&rest rest) (declare (ignore rest)) (return-from tag t))))
     (handler-case (signal "testing simple-signal")
       (simple-condition nil (invoke-restart 'nil)))))))
(define-test sacla-must-condition.84 (:tag :sacla)
 (assert-true (restart-case t)))
(define-test sacla-must-condition.85 (:tag :sacla)
 (assert-true
  (restart-case t
    (continue (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.86 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (restart-case (values 0 1 2 3 4))) '(0 1 2 3 4))))
(define-test sacla-must-condition.87 (:tag :sacla)
 (assert-true
  (eq 'continued
      (restart-case (continue)
        (continue (&rest rest) (declare (ignore rest)) 'continued)))))
(define-test sacla-must-condition.88 (:tag :sacla)
 (assert-true
  (eq nil
      (restart-case (continue)
        (continue (&rest rest) (declare (ignore rest)))))))
(define-test sacla-must-condition.89 (:tag :sacla)
 (assert-true
  (eq 'continue-left
      (restart-case (continue)
        (continue (&rest rest) (declare (ignore rest)) 'continue-left)
        (continue (&rest rest) (declare (ignore rest)) 'continue-right)))))
(define-test sacla-must-condition.90 (:tag :sacla)
 (assert-true
  (null
   (restart-case (invoke-restart 'continue)
     (continue (&rest rest) :interactive (lambda () (list 0 1 2 3)) rest)))))
(define-test sacla-must-condition.91 (:tag :sacla)
 (assert-true
  (equal
   (restart-case (invoke-restart-interactively 'continue)
     (continue (&rest rest) :interactive (lambda () (list 0 1 2 3)) rest))
   '(0 1 2 3))))
(define-test sacla-must-condition.92 (:tag :sacla)
 (assert-true
  (equal
   (restart-case (invoke-restart-interactively 'continue)
     (continue (&rest rest) :interactive
       (lambda () (list 0 1 2 3))
       :report
       "continue"
       rest))
   '(0 1 2 3))))
(define-test sacla-must-condition.93 (:tag :sacla)
 (assert-true
  (equal
   (restart-case (invoke-restart-interactively 'continue)
     (continue (&rest rest) :interactive
       (lambda () (list 0 1 2 3))
       :report
       "continue"
       :test
       (lambda (c) (declare (ignore c)) t)
       rest))
   '(0 1 2 3))))
(define-test sacla-must-condition.94 (:tag :sacla)
 (assert-true
  (=
   (restart-case (handler-bind
                     ((error
                       #'(lambda (c)
                           (declare (ignore c))
                           (invoke-restart 'my-restart 7))))
                   (error "Foo."))
     (my-restart (&optional v) v))
   7)))
(define-test sacla-must-condition.95 (:tag :sacla)
 (assert-true
  (eq
   (handler-bind
       ((error
         #'(lambda (c)
             (declare (ignore c))
             (invoke-restart 'my-restart 'restarted))))
     (restart-case (error "Boo.")
       (my-restart (&optional v) v)))
   'restarted)))
(define-test sacla-must-condition.96 (:tag :sacla)
 (assert-true
  (eq
   (handler-bind
       ((error
         #'(lambda (c)
             (invoke-restart (find-restart 'my-restart c) 'restarted))))
     (restart-case (error "Boo.")
       (my-restart (&optional v) v)))
   'restarted)))
(define-test sacla-must-condition.97 (:tag :sacla)
 (assert-true
  (>
   (length
    (block tag
      (handler-bind
          ((error #'(lambda (c) (return-from tag (compute-restarts c)))))
        (restart-case (error "Boo.")
          (my-restart (&optional v) v)
          (my-restart (&optional v) v)))))
   1)))
(define-test sacla-must-condition.98 (:tag :sacla)
 (assert-true
  (eq 'ok
      (restart-case (invoke-restart 'nil)
        (nil (&rest rest) (declare (ignore rest)) 'ok)))))
(define-test sacla-must-condition.99 (:tag :sacla)
 (assert-true (listp (mapcar #'restart-name (compute-restarts)))))
(define-test sacla-must-condition.100 (:tag :sacla)
 (assert-true
  (listp
   (mapcar #'restart-name
           (compute-restarts
            (make-condition 'simple-error
                            :format-control "error"
                            :format-arguments nil))))))
(define-test sacla-must-condition.101 (:tag :sacla)
 (assert-true
  (restart-case (let ((list (compute-restarts)))
                  (and
                   (member 'my-restart
                           list
                           :test #'string=
                           :key #'restart-name)
                   (member 'your-restart
                           list
                           :test #'string=
                           :key #'restart-name)))
    #1=(my-restart #1#)
    #1#)))
(define-test sacla-must-condition.102 (:tag :sacla)
 (assert-true
  (restart-case (let ((list (compute-restarts)))
                  (member 'my-restart
                          (cdr
                           (member 'my-restart
                                   list
                                   :test #'string=
                                   :key #'restart-name))
                          :test #'string=
                          :key #'restart-name))
    #1=(my-restart #1#)
    #1#)))
(define-test sacla-must-condition.103 (:tag :sacla)
 (assert-true (or (find-restart 'continue) t)))
(define-test sacla-must-condition.104 (:tag :sacla)
 (assert-true
  (restart-case (find-restart 'my-restart)
    #1=(my-restart #1#))))
(define-test sacla-must-condition.105 (:tag :sacla)
 (assert-true
  (restart-case (find-restart (find-restart 'my-restart))
    #1=(my-restart #1#))))
(define-test sacla-must-condition.106 (:tag :sacla)
 (assert-true
  (let ((condition
         (make-condition 'simple-error
                         :format-control "error"
                         :format-arguments nil)))
    (block tag
      (handler-bind
          ((error
            #'(lambda (c)
                (return-from tag
                  (and (eq c condition) (find-restart 'my-restart c))))))
        (restart-case (error condition)
          #1=(my-restart #1#)))))))
(define-test sacla-must-condition.107 (:tag :sacla)
 (assert-true
  (string= "MY-RESTART"
           (block tag
             (handler-bind
                 ((error
                   #'(lambda (c)
                       (return-from tag
                         (restart-name (find-restart 'my-restart c))))))
               (restart-case (error "error!")
                 #1=(my-restart #1#)))))))
(define-test sacla-must-condition.108 (:tag :sacla)
 (assert-true
  (null
   (block tag
     (handler-bind
         ((error
           #'(lambda (c)
               (return-from tag (restart-name (find-restart 'nil c))))))
       (restart-case (error "error!")
         #1=(nil #1#)))))))
(define-test sacla-must-condition.109 (:tag :sacla)
 (assert-true
  (null
   (with-condition-restarts
       (make-condition 'simple-error
                       :format-control "error"
                       :format-arguments nil)
     nil))))
(define-test sacla-must-condition.110 (:tag :sacla)
 (assert-true
  (with-condition-restarts
      (make-condition 'simple-error
                      :format-control "error"
                      :format-arguments nil)
    nil
    t)))
(define-test sacla-must-condition.111 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (with-condition-restarts
        (make-condition 'simple-error
                        :format-control "error"
                        :format-arguments nil)
      nil
      0
      1
      2
      (values 3 4 5)))
   '(3 4 5))))
(define-test sacla-must-condition.112 (:tag :sacla)
 (assert-true
  (let ((condition
         (make-condition 'simple-error
                         :format-control "error"
                         :format-arguments nil))
        (other
         (make-condition 'simple-error
                         :format-control "error"
                         :format-arguments nil)))
    (block tag
      (handler-bind
          ((error
            #'(lambda (c)
                (return-from tag
                  (and (find-restart 'my-restart c)
                       (null
                        (with-condition-restarts other
                          (compute-restarts)
                          (find-restart 'my-restart c))))))))
        (restart-case (progn
                        3
                        2
                        1
                        'go
                        (error condition))
          #1=(my-restart #1#)))))))
(define-test sacla-must-condition.113 (:tag :sacla)
 (assert-true (null (with-simple-restart (continue "continue")))))
(define-test sacla-must-condition.114 (:tag :sacla)
 (assert-true
  (with-simple-restart (continue "continue")
    t)))
(define-test sacla-must-condition.115 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (with-simple-restart (continue "continue")
      0
      1
      (values 2 3 4)))
   '(2 3 4))))
(define-test sacla-must-condition.116 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (with-simple-restart (continue "continue")
      (continue)))
   '(nil t))))
(define-test sacla-must-condition.117 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (with-simple-restart (continue "continue")
      (handler-case (error "boo")
        (error (c) (declare (ignore c)) (invoke-restart 'continue)))))
   '(nil t))))
(define-test sacla-must-condition.118 (:tag :sacla)
 (assert-true
  (eq 'ok
      (restart-case (abort)
        (abort () 'ok)))))
(define-test sacla-must-condition.119 (:tag :sacla)
 (assert-true
  (let ((condition
         (make-condition 'simple-error
                         :format-control "error"
                         :format-arguments nil)))
    (or (find-restart 'abort condition)
        (eq 'handled
            (handler-case (abort condition)
              (control-error nil 'handled)
              (condition nil nil)))))))
(define-test sacla-must-condition.120 (:tag :sacla)
 (assert-true
  (eq 'ok
      (restart-case (muffle-warning)
        (muffle-warning () 'ok)))))
(define-test sacla-must-condition.121 (:tag :sacla)
 (assert-true
  (let ((condition
         (make-condition 'simple-warning
                         :format-control "warning"
                         :format-arguments nil)))
    (or (find-restart 'muffle-warning condition)
        (eq 'handled
            (handler-case (muffle-warning condition)
              (control-error nil 'handled)
              (condition nil nil)))))))
(define-test sacla-must-condition.122 (:tag :sacla)
 (assert-true
  (eq 'ok
      (restart-case (continue)
        (continue () 'ok)))))
(define-test sacla-must-condition.123 (:tag :sacla)
 (assert-true
  (let ((condition
         (make-condition 'simple-error
                         :format-control "error"
                         :format-arguments nil)))
    (or (find-restart 'continue condition) (null (continue condition))))))
(define-test sacla-must-condition.124 (:tag :sacla)
 (assert-true
  (eq 'ok
      (restart-case (store-value 'ok)
        (store-value (value) value)))))
(define-test sacla-must-condition.125 (:tag :sacla)
 (assert-true
  (let ((condition
         (make-condition 'simple-error
                         :format-control "error"
                         :format-arguments nil)))
    (or (find-restart 'store-value condition)
        (null (store-value t condition))))))
(define-test sacla-must-condition.126 (:tag :sacla)
 (assert-true
  (eq 'ok
      (restart-case (use-value 'ok)
        (use-value (value) value)))))
(define-test sacla-must-condition.127 (:tag :sacla)
 (assert-true
  (let ((condition
         (make-condition 'simple-error
                         :format-control "error"
                         :format-arguments nil)))
    (or (find-restart 'use-value condition) (null (use-value t condition))))))
(define-test sacla-must-condition.128 (:tag :sacla)
 (assert-true (eq (assert t) nil)))
(define-test sacla-must-condition.129 (:tag :sacla)
 (assert-true
  (handler-case (assert nil)
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.130 (:tag :sacla)
 (assert-true
  (let ((count 0))
    (and (eq (assert (incf count)) nil) (= count 1)))))
(define-test sacla-must-condition.131 (:tag :sacla)
 (assert-true
  (handler-case
      (let ((var nil))
        (assert var (var) "VAR should be true."))
    (simple-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.132 (:tag :sacla)
 (assert-true
  (let ((str (copy-seq "ABC")) (count 0))
    (and
     (eq
      (assert (char= (aref str 0) #\A)
              ((aref
                (progn
                  (incf count)
                  str)
                0)))
      nil)
     (zerop count)))))
(define-test sacla-must-condition.133 (:tag :sacla)
 (assert-true
  (let ((str (copy-seq "ABC")) (count 0))
    (and
     (eq
      (assert (and (char= (aref str 0) #\A) (char= (aref str 1) #\B))
              ((aref
                (progn
                  (incf count)
                  str)
                0)
               (aref
                (progn
                  (incf count)
                  str)
                1)))
      nil)
     (zerop count)))))
(define-test sacla-must-condition.134 (:tag :sacla)
 (assert-true
  (handler-case
      (let ((var nil))
        (assert var (var) 'type-error :expected-type 'array))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.135 (:tag :sacla)
 (assert-true
  (null
   (let ((var nil))
     (check-type var null)))))
(define-test sacla-must-condition.136 (:tag :sacla)
 (assert-true
  (null
   (let ((var '(a b c)))
     (check-type var cons)))))
(define-test sacla-must-condition.137 (:tag :sacla)
 (assert-true
  (handler-case
      (let ((var '(a b c)))
        (check-type var vector))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-condition.138 (:tag :sacla)
 (assert-true
  (eq 'handled
      (block tag
        (handler-bind
            ((type-error
              #'(lambda (c) (declare (ignore c)) (return-from tag 'handled)))
             (error #'(lambda (c) (declare (ignore c)) (return-from tag nil))))
          (let ((var '(a b c)))
            (check-type var vector)
            var))))))
(define-test sacla-must-condition.139 (:tag :sacla)
 (assert-true
  (string=
   (block tag
     (handler-bind
         ((type-error
           #'(lambda (c)
               (declare (ignore c))
               (invoke-restart 'store-value "eat this")))
          (error #'(lambda (c) (declare (ignore c)) (return-from tag nil))))
       (let ((var '(a b c)))
         (check-type var vector)
         var)))
   "eat this")))
(define-test sacla-must-condition.140 (:tag :sacla)
 (assert-true (null (ignore-errors))))
(define-test sacla-must-condition.141 (:tag :sacla)
 (assert-true (ignore-errors t)))
(define-test sacla-must-condition.142 (:tag :sacla)
 (assert-true
  (let ((result (multiple-value-list (ignore-errors (error "error")))))
    (and (null (first result)) (typep (second result) 'simple-error)))))
(define-test sacla-must-condition.143 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (ignore-errors 'a 'b 'c (values 'd 'e))) '(d e))))
(define-test sacla-must-condition.144 (:tag :sacla)
 (assert-true
  (let ((result
         (multiple-value-list
          (ignore-errors
           (signal 'simple-error
                   :format-control "error"
                   :format-arguments nil)))))
    (and (null (first result)) (typep (second result) 'simple-error)))))
(define-test sacla-must-condition.145 (:tag :sacla)
 (assert-true (eq (ignore-errors (signal "only signal") 'ok) 'ok)))
(define-test sacla-must-condition.146 (:tag :sacla)
 (assert-true
  (eq
   (block tag
     (handler-bind
         ((condition
           #'(lambda (c) (declare (ignore c)) (return-from tag 'handled))))
       (ignore-errors
        (error 'simple-condition
               :format-control "only condition"
               :format-arguments nil))))
   'handled)))
(define-test sacla-must-condition.147 (:tag :sacla)
 (assert-true
  (let ((result
         (multiple-value-list
          (ignore-errors
           (warn 'simple-error
                 :format-control "an error, not a warning"
                 :format-arguments nil)))))
    (and (null (first result)) (typep (second result) 'type-error)))))

