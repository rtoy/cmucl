;;;-*-Mode:LISP; Package:(PCL Lisp 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;
;;; The version of low for Kyoto Common Lisp (KCL)
(in-package "SI")
(export '(%structure-name
          %compiled-function-name
          %set-compiled-function-name))
(in-package 'pcl)

(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (si:address thing)))

#+akcl
(eval-when (load compile eval)
 (if (fboundp 'si::allocate-growth) (pushnew :turbo-closure *features*)))

(defmacro %svref (vector index)
  `(svref (the simple-vector ,vector) (the fixnum ,index)))

(defsetf %svref (vector index) (new-value)
  `(setf (svref (the simple-vector ,vector) (the fixnum ,index))
         ,new-value))


;;;
;;; std-instance-p
;;;
(si:define-compiler-macro std-instance-p (x)
  (once-only (x)
    `(and (si:structurep ,x)
          (eq (si:%structure-name ,x) 'std-instance))))

;;;
;;; turbo-closure patch.  See the file kcl-mods.text for details.
;;;
#-turbo-closure-env-size
(clines "
object cclosure_env_nthcdr (n,cc)
int n; object cc;
{  object env;
   if(n<0)return Cnil;
   if(type_of(cc)!=t_cclosure)return Cnil;
   env=cc->cc.cc_env;
   while(n-->0)
     {if(type_of(env)!=t_cons)return Cnil;
      env=env->c.c_cdr;}
   return env;
}")

#+turbo-closure-env-size
(clines "
object cclosure_env_nthcdr (n,cc)
int n; object cc;
{  object env,*turbo;
   if(n<0)return Cnil;
   if(type_of(cc)!=t_cclosure)return Cnil;
   if((turbo=cc->cc.cc_turbo)==NULL)
     {env=cc->cc.cc_env;
      while(n-->0)
        {if(type_of(env)!=t_cons)return Cnil;
         env=env->c.c_cdr;}
      return env;}
   else
     {if(n>=fix(*(turbo-1)))return Cnil;
      return turbo[n];}
}")

;; This is the completely safe version.
(defentry cclosure-env-nthcdr (int object) (object cclosure_env_nthcdr))
;; This is the unsafe but fast version.
(defentry %cclosure-env-nthcdr (int object) (object cclosure_env_nthcdr))

;;; #+akcl means this is an AKCL newer than 5/11/89 (structures changed)
(eval-when (compile load eval)

;;((name args-type result-type side-effect-p new-object-p c-expression) ...)
(defparameter *kcl-function-inlines*
  '(#-akcl (si:structurep (t) compiler::boolean nil nil "type_of(#0)==t_structure")
    #-akcl (si:%structure-name (t) t nil nil "(#0)->str.str_name")
    #+akcl (si:%structure-name (t) t nil nil "(#0)->str.str_def->str.str_self[0]")
    (si:%compiled-function-name (t) t nil nil "(#0)->cf.cf_name")
    (si:%set-compiled-function-name (t t) t t nil "((#0)->cf.cf_name)=(#1)")
    (cclosurep (t) compiler::boolean nil nil "type_of(#0)==t_cclosure")
    (%cclosure-env (t) t nil nil "(#0)->cc.cc_env")
    (%set-cclosure-env (t t) t t nil "((#0)->cc.cc_env)=(#1)")
    #+turbo-closure
    (%cclosure-env-nthcdr (fixnum t) t nil nil "(#1)->cc.cc_turbo[#0]")))

(defun make-function-inline (inline)
  (setf (get (car inline) 'compiler::inline-always) (list (cdr inline))))

(defmacro define-inlines ()
  `(progn
    ,@(mapcan #'(lambda (inline)
                  (let ((name (intern (format nil "~S inline" (car inline))))
                        (vars (mapcar #'(lambda (type)
                                          (declare (ignore type))
                                          (gensym))
                                      (cadr inline))))
                    `((make-function-inline ',(cons name (cdr inline)))
                      (defun ,(car inline) ,vars
                        ,@(mapcan #'(lambda (var var-type)
                                      (unless (eq var-type 't)
                                        `((declare (type ,var-type ,var)))))
                                  vars (cadr inline))
                        (,name ,@vars))
                      (make-function-inline ',inline))))
              *kcl-function-inlines*)))

(define-inlines)
)

(defsetf si:%compiled-function-name si:%set-compiled-function-name)
(defsetf %cclosure-env %set-cclosure-env)

(defun set-function-name-1 (fn new-name ignore)
  (declare (ignore ignore))
  (cond ((compiled-function-p fn)
         (setf (si:%compiled-function-name fn) new-name))
        ((and (listp fn)
              (eq (car fn) 'lambda-block))
         (setf (cadr fn) new-name))
        ((and (listp fn)
              (eq (car fn) 'lambda))
         (setf (car fn) 'lambda-block
               (cdr fn) (cons new-name (cdr fn)))))
  fn)

#+akcl (clines "#define AKCL206")

(clines "
object set_cclosure (result_cc,value_cc,available_size)
object result_cc,value_cc; int available_size;
{
  object result_env_tail,value_env_tail; int i;

  result_env_tail=result_cc->cc.cc_env;
  value_env_tail=value_cc->cc.cc_env;
  for(i=available_size;
      result_env_tail!=Cnil && i>0;
      result_env_tail=CMPcdr(result_env_tail), value_env_tail=CMPcdr(value_env_tail))
    CMPcar(result_env_tail)=CMPcar(value_env_tail), i--;
  result_cc->cc.cc_self=value_cc->cc.cc_self;
  result_cc->cc.cc_data=value_cc->cc.cc_data;
#ifndef AKCL206
  result_cc->cc.cc_start=value_cc->cc.cc_start;
  result_cc->cc.cc_size=value_cc->cc.cc_size;
#endif
  return result_cc;
}")

(defentry %set-cclosure (object object int) (object set_cclosure))

