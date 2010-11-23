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
  "Raise scheme-compile-error."
  (error (make-condition 'scheme-compile-error
                         :expr expr
                         :reason (apply #'format nil reason args))))

;; resolve-define-list
;; (define x 13) => (letrec ((x 13)) 
;; ((define x 13) (define y 7)) => (letrec ((x 13)) (letrec ((y 7)) #f))
;; ((define x 13) (define y 7) (f)) => (letrec ((x 13)) (letrec ((y 7)) (f)))

(defun define->letrec (seq)
  (cond
    ((null seq) nil)
    (t
     (match (car seq)
       (('define <variable> <expression>)
        (let ((rest (define->letrec (cdr seq))))
          (if (null rest)
              `(letrec ((,<variable> ,<expression>))
                       #f)
              `(letrec ((,<variable> ,<expression>))
                       ,rest))))
       (t
        (let ((rest (define->letrec (cdr seq))))
          (if (null rest)
              (car seq)
              `(begin ,(car seq) ,rest))))))))
     
;; (define->letrec '((define y 3) (define x 1))) 
;; (define->letrec '( (define y (lambda (n) (* n 2)))  (y 30) (y 10)))
;; (define->letrec '((define y 3) y))

(defun literal->cons (data)
  (if (atom data)
     data
     `(cons ,(car data) ,(literal->cons (cdr data)))))

(defun comp (exp env c)
  "Compile an expression EXP."
  ;; (format t ";; comp :~%exp: ~a~%env: ~a~%" exp env)
  (cond
    ((null exp)
     `(:NIL ,@c))
    ((self-evaluating-p exp)
     `(:LDC ,exp ,@c)) ;; BE CARE fixme
    ;; R5RS 4.1.1. Variable references
    ((atom exp)
     (let ((location (lookup exp env)))
       (unless location
         (comp-error exp "Variable ~a not found in env ~a." exp env))
       `(:LD ,(lookup exp env) ,@c)))
    (t
     (let* ((fn (car exp))
            (args (cdr exp))
            (argl (length args)))
       (case fn
         ;; R5RS 4.1.2. Literal expressions
         ((quote)
          (match exp
            (('quote data)
             (comp (literal->cons data) env c))
            (t
             (comp-error exp "Unexpected quote form."))
            )
          ;(comp-error exp "Not implement yet: quote")
          )
         ;; R5RS 4.1.4. Procedures
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
         ;; R5RS 4.1.5. Conditinals
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
         ;; R5RS 4.1.6. Assignments
         ((set!)
          (match exp
            (('set! <variable> <expression>)
             ;; be care; set! return #f
             (comp <expression> env `(:SET ,(lookup <variable> env) :LDC #f ,@c)))
            (t (comp-error exp "Unexpected set! form."))))
         ;; R5RS 4.2.1. Conditionals
         ((cond)
          (comp-error exp "Not implement yet: cond"))
         ((case)
          (comp-error exp "Not implement yet: case"))
         ((and)
          (match exp
            (('and) (comp #t env c))
            (('and <test1>) (comp `(if ,<test1> #t #f) env c))
            (t
             (comp `(if ,(car args)
                        (and ,(cdr args))
                        #f) env c))))
         ((or)
          (match exp
            (('or) (comp #f env c))
            (('or <test1>) (comp <test1> env c))
            (('or <test1> <test2>)
             (let ((sym (gensym)))
               (comp `(let ((,sym ,<test1>))
                        (if ,sym
                            ,sym ,<test2>)) env c)))
            (t
             (comp `(or ,(car args)
                        (or ,@(cdr args))) env c))))
         ;; R5RS 4.2.2. Binding constructs
         ((let)
          (match exp
            (('let <bindings> . <body>)
             (let ((vars (mapcar #'car <bindings>))
                   (inits (mapcar #'cadr <bindings>)))
               (comp `((lambda ,vars ,@<body>) ,@inits) env c)))
            (t (comp-error exp "Unexpected let form."))))
         ((let*)
          (comp-error exp "Not implement yet: let*"))
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
         ;; R5RS 4.2.3. Sequencing
         ((begin)
          (reduce #'(lambda (e cont)
                      (comp e env `(:POP ,@cont)))
                  (butlast args)
                  :from-end t
                  :initial-value (comp (car (last args)) env c)))
         ;; R5RS 4.2.4. Iteration
         ((do)
          (comp-error exp "Not implement yet: do"))
         ;; R5RS 6. Standard procedures
         ;; R5RS 6.1. Equivalence predicates
         ;; R5RS 6.2. Numbers
         ((=)
          (match exp
            (('=) (comp-error exp "(=) is invalid form."))
            (('= z) (comp-error exp "= require 2 arguments but got 1: ~a" z))
            (('= z1 z2)
             (comp z2 env (comp z1 env `(:= ,@c))))
            (('= z1 z2 z3) ;; (= x y z) => (and (= x y) (= y z))
             (comp `(if (and (= ,z1 ,z2)
                             (= ,z2 ,z3))
                        #t
                        #f) env c))
            (t
             (comp `(if (and (= ,(car args) ,(cadr args))
                             (= ,(cadr args) ,@(cddr args)))
                        #t
                        #f) env c))))
         ((<)
          (match exp
            (('<) (comp-error exp "(<) is invalid form."))
            (('< x1) (comp-error exp "(< ~a) is invalid form." x1))
            (('< x1 x2) (comp x2 env (comp x1 env `(:< ,@c))))
            (t (comp-error exp "Not implemented yet: (< x1 x2 x3)"))))
         ((>)
          (match exp
            (('>) (comp-error exp "(>) is invalid form."))
            (('> x1) (comp-error exp "(> ~a) is invalid form." x1))
            (('> x1 x2) (comp x2 env (comp x1 env `(:> ,@c))))
            (t (comp-error exp "Not implemented yet: (> x1 x2 x3)"))))
         ((<=)
          (match exp
            (('<=) (comp-error exp "(<=) is invalid form."))
            (('<= x1) (comp-error exp "(<= ~a) is invalid form." x1))
            (('<= x1 x2) (comp x2 env (comp x1 env `(:<= ,@c))))
            (t (comp-error exp "Not implemented yet: (<= x1 x2 x3)"))))
         ((>=)
          (match exp
            (('>=) (comp-error exp "(>=) is invalid form."))
            (('>= x1) (comp-error exp "(>= ~a) is invalid form." x1))
            (('>= x1 x2) (comp x2 env (comp x1 env `(:>= ,@c))))
            (t (comp-error exp "Not implemented yet: (>= x1 x2 x3)"))))
         ((zero?)
          (match exp
            (('zero? z) (comp `(= 0 ,z) env c))
            (t (comp-error exp "zero? requires 1 arguments but got ~a" argl))))
         ((positive?)
          (match exp
            (('positive? z) (comp `(> ,z 0) env c))
            (t (comp-error exp "positive? requires 1 arguments but got ~a" argl))))
         ((negative?)
          (match exp
            (('negative? z) (comp `(< ,z 0) env c))
            (t (comp-error exp "negative? requires 1 arguments but got ~a" argl))))
         ((+)
          (match exp
            (('+) (comp 0 env c))
            (('+ z1)
             (comp z1 env c))
            (('+ z1 z2) (comp z2 env (comp z1 env `(:+ ,@c))))
            (t ;;(+ z1 z2 z3) => (+ z1 (+ z2 z3))
             (comp `(+ ,(car args) (+ ,@(cdr args))) env c))))
         ((*)
          (match exp
            (('*) (comp 1 env c))
            (('* z1) (comp z1 env c))
            (('* z1 z2) (comp z2 env (comp z1 env `(:* ,@c))))
            (t ;;(* z1 z2 z3) => (* z1 (* z2 z3))
             (comp `(* ,(car args) (* ,@(cdr args))) env c))))
         ((-)
          (match exp
            (('-) (comp-error exp "(-) is invalid form."))
            (('- z) (comp (- z) env c))
            (('- z1 z2)
             (comp z2 env (comp z1 env `(:- ,@c))))
            (t ;;(- z1 z2 ...)
             (comp `(- (- ,(first args) ,(second args)) ,@(nthcdr 2 args)) env nil))))
         ((/)
          (comp-error exp "Not implement yet: /"))
         ((abs)
          (match exp
            (('abs x)
             (comp `(if (negative? ,x)
                     (- ,x)
                     ,x) env c))
            (t (comp-error "(abs requires 1 argument but got ~a" argl))))
         ;; 
         ((quotient remainder modulo gcd lcm numerator denominator)
          (comp-error exp "Not implement yet."))
         ;; R5RS 6.4. Control features
         ((procedure?)
          (comp-error exp "Not implement yet: procedure?"))
         ((apply)
          (comp-error exp "Not implement yet: apply"))
         ((map)
          (comp-error exp "Not implement yet: map"))
         ((for-each)
          (comp-error exp "Not implement yet: for-each"))
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
         ((values)
          (comp-error exp "Not implement yet: values"))
         ((call-with-values)
          (comp-error exp "Not implement yet: call-with-values"))
         ((dynamic-wind)
          (comp-error exp "Not implement yet: dynamic-wind"))
         ;; R5RS 6.5. Eval
         ((eval)
          (comp-error exp "Not implement yet: eval"))
         ((scheme-report-environment)
          (comp-error exp "Not implement yet: scheme-report-environment"))
         ((null-environment)
          (comp-error exp "Not implement yet: null-environment"))
         ((interaction-environment)
          (comp-error exp "Not implement yet: interaction-environment"))
         ;; R5RS 6.6.

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
  "Optimize compiled code if possible."
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
