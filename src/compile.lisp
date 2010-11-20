;; -*- coding:utf-8 -*-
;; compile.lisp - The SECD Machine in Common Lisp
;; Copyright (C) 2010 by cranebird
;; my blog: http://d.hatena.ne.jp/cranebird/ (in Japanese)
;; github:
;; Based on the lecture note by Prof. Jia-Huai You
;; (http://www.cs.ualberta.ca/~you/courses/325/Mynotes/Fun/SECD-slides.html)
;; And LispMe.

(in-package :secd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-scm-macro-character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lookup the variable VAR in the environment ENV
;; env example:
;; ( ((c . 1) (d . 2) (e . 3)) )
;; ( ((a . 1) (b . 2)) ((c . 1) (d . 2) (e . 3)))
;; (lookup 'a ( ((a . 1) (b . 2)))) => (1 . 1) =(level 1, 1st)
;; (lookup 'b ( ((a . 1) (b . 2)))) => (1 . 2) =(level 1, 2nd)
;; (lookup 'c '(((a . 1) (b . 2)) ((c . 1) (b . 2)))) => (2 .1) = (level2, 1st)
;; not found => nil

(defun lookup (var env)
  "Lookup the variable VAR in environment ENV in compile time."
  (loop :for e :in env :for level :from 1
     :if (assoc var e)
     :return (cons level (cdr (assoc var e)))))

(defun extend-env (plist env)
  "Extend environment in compile time."
  (append
   (list (loop :for idx :from 1 :for var :in plist
            :collect (cons var idx))) env))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun instruction-p (x) (keywordp x))

(defun self-evaluating-p (expr)
  "return non-nil if EXPR is self-evaluating object."
  (or (numberp expr)
      (stringp expr)
      (eql expr #t)
      (eql expr #f)))

(define-condition scheme-compile-error (simple-error)
  ((expr :initarg :expr :accessor scheme-compile-error-expr)
   (reason :initarg :reason :accessor scheme-compile-error-reason))
  (:report (lambda (condition stream)
             (format stream "SCHEME Compile Error: Fail to compile ~a~%~a"
                     (scheme-compile-error-expr condition)
                     (scheme-compile-error-reason condition)))))

(defun comp-error (expr reason &rest args)
  "raise scheme-compile-error."
  (error (make-condition 'scheme-compile-error
                         :expr expr
                         :reason (apply #'format nil reason args))))

;; resolve-define-list
;; (define x 13) => (letrec ((x 13)) 
;; ((define x 13) (define y 7)) => (letrec ((x 13)) (letrec ((y 7)) #f))
;; ((define x 13) (define y 7) (f)) => (letrec ((x 13)) (letrec ((y 7)) (f)))

(defun define->letrec (exp)
  (cond
    ((consp exp)
     (match exp
       (('define <variable> <expression>)
        `(letrec ((,<variable> ,<expression>))
                 #f))
       (t
        (reduce #'(lambda (e body)
                    (format t "e=~a body=~a~%" e body)
                    (match e
                      (('define <variable> <expression>)
                       `(letrec ((,<variable> ,<expression>))
                                (begin ,@body)))
                      (t
                       `(,e ,@body)))) exp :from-end t :initial-value nil))))
    (t
     exp)))
;; fixme
;; (define->letrec '((define y 3) (define x 1))) => (LETREC ((Y 3)) (BEGIN LETREC ((X 1)) (BEGIN))) ;; error. BEGIN LETREC is invalid.
;; (define->letrec '( (define y (lambda (n) (* n 2)))  (y 30) (y 10))) => (LETREC ((Y (LAMBDA (N) (* N 2)))) (BEGIN (Y 30) (Y 10))) ;; not clean, but ok


(defun comp (exp env c)
  "Compile an expression EXP."
  ;; (format t ";; comp :~%exp: ~a~%env: ~a~%" exp env)
  (cond
    ((self-evaluating-p exp)
     `(:LDC ,exp ,@c)) ;; BE CARE fixme
    ((null exp)
     `(:NIL ,@c))
    ;; Variable access
    ((atom exp)
     (let ((location (lookup exp env)))
       (unless location
         (comp-error exp "Variable ~a not found in env ~a." exp env))
       `(:LD ,(lookup exp env) ,@c)))
    ((consp exp)
     (let* ((fn (car exp))
            (args (cdr exp))
            (argl (length args)))
       (case fn
         ((quote)
          (comp-error exp "Not implement yet: quote"))
         ((if)
          (match exp
            (('if <test> <consequent> <alternate>)
             (let ((ct (comp <consequent> env '(:JOIN)))
                   (cf (comp <alternate> env '(:JOIN))))
               (comp <test> env `(:SEL ,ct ,cf ,@c))))
            (('if <test> <consequent>) ;; unspecified
             (let ((ct (comp <consequent> env '(:JOIN)))
                   (cf (comp '#f env '(:JOIN))))
               (comp <test> env `(:SEL ,ct ,cf ,@c))))
            (t (comp-error exp "Unexpected if form."))))
         ((and)
          (case argl
            ((0) (comp #t env c))
            ((1) (comp `(if ,(car args) #t #f) env c))
            (t
             (comp `(if ,(car args)
                        (and ,(cdr args))
                        #f) env c))))
         ((lambda)
          (match exp
            (('lambda <formals> . <body>)
             (let ((new-env (extend-env <formals> env)))
               `(:LDF ,(reduce #'(lambda (e cont)
                                   (comp e new-env cont))
                               <body>
                               :from-end t
                               :initial-value '(:RTN))
                      ,@c)))
            (t (comp-error exp "Unexpected lambda form."))))
         ((let)
          (match exp
            (('let <bindings> . <body>)
             (let ((vars (mapcar #'car <bindings>))
                   (inits (mapcar #'cadr <bindings>)))
               (comp `((lambda ,vars ,@<body>) ,@inits) env c)))
            (t (comp-error exp "Unexpected let form."))))
         ((letrec)
          (match exp
            (('letrec <bindings> . <body>)
             (let* ((vars (mapcar #'car <bindings>))
                    (inits (reverse (mapcar #'cadr <bindings>)))
                    (new-env (extend-env vars env)))
               `(:DUM 
                 :NIL
                 ,@(reduce
                    #'(lambda (init cont)
                        (comp init new-env `(:CONS ,@cont))) inits
                        :from-end t
                        :initial-value `(:LDF
                                         ,(reduce #'(lambda (e cont)
                                                      (comp e new-env cont))
                                                  <body>
                                                  :from-end t
                                                  :initial-value '(:RTN))
                                         :RAP ,@c)))))
            (t (comp-error exp "Unexpected letrec form."))))
         ((begin)
          (reduce #'(lambda (e cont)
                      (comp e env `(:POP ,@cont)))
                  (butlast args)
                  :from-end t
                  :initial-value (comp (car (last args)) env c)))
         ((set!)
          (match exp
            (('set! <variable> <expression>)
             (comp <expression> env `(:SET ,(lookup <variable> env) ,@c)))
            (t (comp-error exp "Unexpected set! form."))))
         ((call/cc)
          (match exp
            (('call/cc proc)
             (cond
               ((null c)
                (comp-error exp "Unexpected call/cc. Continuation is null."))
               ((equal c '(:RTN)) `(:LDCT (:RTN) ,@(comp proc env `(:TAP))))
               (t
                `(:LDCT ,c ,@(comp proc env `(:AP ,@c))))))
            (t (comp-error exp "Unexpected call/cc form"))))

         ;; ((equal fn 'define)
         ;;  ;;(comp-error exp "define not impl. yet.")
         ;;  ;; 1. simple definistion
         ;;  (format t ";; define ~%")
         ;;  (match exp
         ;;    (('define <variable> <expression>)
         ;;     (format t "define... ~a~%" (extend-env (list <variable>) env))
         ;;     (let ((new-env (extend-env (list <variable>) env)))
         ;;       (comp <expression> new-env  `(:SET ,(lookup <variable> new-env) ,@c)))
         ;;     )
         ;;    (t (comp-error exp "Unexpected define form.")))
         ;;  )
         
         ((apply)
          (comp-error exp "apply not impl. yet."))
         ((+)
          (case argl
            ((0) ;; (+) => 0
             (comp 0 env c))
            ((1) ;; (+ x) => x
             (comp (car args) env c))
            ((2) ;; (+ x y)
             (comp (cadr args) env (comp (car args) env `(:+ ,@c))))
            (t ;;(+ x y z) => (+ x (+ y z))
             (comp `(+ ,(car args) (+ ,@(cdr args))) env c))))
         ((*)
          (case argl
            ((0) ;; (*) => 1
             (comp 1 env c))
            ((1) ;; (* z) => z
             (comp (car args) env c))
            ((2) ;; (* z1 z2)
             (comp (cadr args) env (comp (car args) env `(:* ,@c))))
            (t ;;(* x y z) => (* x (* y z))
             (comp `(* ,(car args) (* ,@(cdr args))) env c))))
         ((-)
          (case argl
            ((0) ;; (-) => error
             (comp-error exp "(-) is invalid form."))
            ((1) ;; (- z) => -z
             (comp (- (car args)) env c))
            ((2) ;; (- z1 z2)
             (comp (cadr args) env (comp (car args) env `(:- ,@c))))
            (t ;;(- z1 z2 ...)
             (comp `(- (- ,(first args) ,(second args)) ,@(nthcdr 2 args)) env nil))))
         ((=)
          (case argl
            ((0) ;; (=) => error
             (comp-error exp "(=) is invalid form."))
            ((1) ;; (= 1) => error
             (comp-error exp "= require 2 arguments."))
            ((2)
             (comp (second args) env (comp (first args) env `(:= ,@c))))
            ((3) ;; (= x y z) => (and (= x y) (= y z))
             (comp `(if (and (= ,(car args) ,(cadr args))
                             (= ,(cadr args) ,(caddr args)))
                        #t
                        #f) env c))
            (t
             (comp `(if (and (= ,(car args) ,(cadr args))
                             (= ,(cadr args) ,@(cddr args)))
                        #t
                        #f) env c))))

         ;; primitive re-define ;; TODO
         ;; keyword ;; TODO
         (t
          (match exp
            ;; Numerical operations
            (('> z1 z2) (comp z2 env (comp z1 env `(:> ,@c))))
            (('>= z1 z2) (comp z2 env (comp z1 env `(:>= ,@c))))
            (('< z1 z2) (comp z2 env (comp z1 env `(:< ,@c))))
            (('<= z1 z2) (comp z2 env (comp z1 env `(:<= ,@c))))
            (('mod z1 z2) (comp z2 env (comp z1 env `(:mod ,@c))))
            ;; Pair and lists
            (('cons obj1 obj2) (comp obj2 env (comp obj1 env `(:CONS ,@c))))
            (('car pair) (comp pair env `(:CAR ,@c)))
            (('cdr pair) (comp pair env `(:CDR ,@c)))
            (('consp obj) (comp obj env `(:CONSP ,@c)))
            (('pair? obj) (comp obj env `(:CONSP ,@c)))
            ;; Vectors
            (('vector-length e1) (comp e1 env `(:VLEN ,@c)))
            (('vector . rest)
             `(:NIL
               ,@(loop :for e :in (reverse rest)
                    :append (comp e env '(:CONS))) :L2V ,@c))
            (('vector-ref vec n) ;; (vector-ref vec n)
             (comp vec env (comp n env `(:VREF ,@c))))
            (('vector-set! vec n obj) ;; (vector-set! vec n obj)
             (comp vec env (comp n env (comp obj env `(:VSET ,@c)))))
            ;; Input and Output
            (('write obj)
             (comp obj env `(:WRITE ,@c)))
            ;; debug
            (('debug)
             `(:DEBUG ,@c))
            (t
             (case argl
               ((0)
                `(:NIL ,@(comp fn env `(:AP ,@c))))
               (t
                `(:NIL
                  ,@(reduce #'(lambda (e cont)
                                (comp e env `(:CONS ,@cont)))
                            (reverse args)
                            :from-end t
                            :initial-value (comp fn env `(:AP ,@c)))))))
            )))
       ))))

(defun compile-pass1 (exp &optional env)
  "Compile an s-expression into a list of instructions in an environment ENV. "
  (comp exp env '(:STOP)))

(defun opt (program)
  "Optimize compiled code."
  (match program
    (()
     nil)
    ((:AP :RTN)
     `(:TAP))
    ((:RAP :RTN)
     `(:RTAP))
    ((:SEL ct cf :RTN)
     (if (and (equal (last ct) '(:JOIN)) (equal (last cf) '(:JOIN)))
         (opt `(:SELR ,(opt (append (butlast ct) '(:RTN))) 
                      ,(opt (append (butlast cf) '(:RTN)))))
         `(:SEL ,(opt ct) ,(opt cf) :RTN)))
    (t
     (if (consp program)
         (cons (opt (car program)) (opt (cdr program)))
         program))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler vector version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-code-array ()
  "make array for code vector."
  (make-array 0 :adjustable t :fill-pointer 0))

(defun append-code (code vec)
  "append CODE to VEC."
  (vector-push-extend code vec))

(defun compile-pass2 (program vec label-table)
  "Compile s-expression PROGRAM into vector VEC."
  (if (null program)
      ;; resolve label
      (loop :for i :from 0 :below (length vec)
         :for x = (aref vec i)
         :do (setf (aref vec i) (or (gethash x label-table) x)))
      (ecase (car program)
        ((:NIL :AP :RTN :CONS :CAR :CDR :RAP :DUM :+ :- :* :> :< := :JOIN :STOP) ; no label
         (destructuring-bind (op . rest) program
           (append-code op vec)
           (compile-pass2 rest vec label-table)))
        (:LDC
         (destructuring-bind (op x . rest) program
           (append-code op vec)
           (append-code x vec)
           (compile-pass2 rest vec label-table)))
        (:LD
         (destructuring-bind (op (level . n) . rest) program
           (append-code op vec)
           (append-code level vec)
           (append-code n vec)
           (compile-pass2 rest vec label-table)))
        (:SEL ;; (SEL ct cf . c) => #(SEL PC-CT PC-CF PC-CONT CT ... CF ... CONT ...)
         (destructuring-bind (op ct cf . rest) program
           (let ((ct-start (gensym))
                 (cf-start (gensym))
                 (rest-start (gensym)))
             (append-code op vec)
             (append-code ct-start vec)
             (append-code cf-start vec)
             (append-code rest-start vec)
             ;; ct
             (setf (gethash ct-start label-table) (fill-pointer vec))
             (compile-pass2 ct vec label-table)
             ;; cf
             (setf (gethash cf-start label-table) (fill-pointer vec))
             (compile-pass2 cf vec label-table)
             (setf (gethash rest-start label-table) (fill-pointer vec))
             ;; rest 
             (compile-pass2 rest vec label-table))))
        (:LDF ;; (:LDF body) => #(:LDF PC-body cont fbody)
         (destructuring-bind (op fbody . rest) program
           (let ((cont-start (gensym)))
             (append-code op vec)
             (append-code (+ 2 (fill-pointer vec)) vec) ;; fbody pos = :LDC pos + 2
             (append-code cont-start vec)
             (compile-pass2 fbody vec label-table)
             (setf (gethash cont-start label-table) (fill-pointer vec))
             (compile-pass2 rest vec label-table))))
        (t
         (format t "compile-pass2 unknown: program ~a, vec: ~a~%" program vec)))))

(defun compile-exp (exp)
  "Compile s-expression EXP into vector."
  (let ((program-list (compile-pass1 exp nil)))
    (let ((vec (make-code-array))
          (ht (make-hash-table)))
      (compile-pass2 program-list vec ht)
      (make-array (length vec)
                  :initial-contents vec))))
