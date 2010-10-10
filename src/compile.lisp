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
             (format stream "SCHEME Compile Error: Fail to compile: ~a~%~a"
                     (scheme-compile-error-expr condition)
                     (scheme-compile-error-reason condition)))))

(defun comp-error (expr reason &rest args)
  (error (make-condition 'scheme-compile-error
                         :expr expr
                         :reason (apply #'format nil reason args))))

(defun comp (exp env c)
  "Compile an expression EXP."
  (cond
    ((self-evaluating-p exp)
     `(:LDC ,exp ,@c)) ;; BE CARE fixme
    ((null exp) 
     `(:NIL ,@c))
    ;; Variable access
    ((atom exp)
     (let ((location (lookup exp env)))
       (unless location
         (comp-error exp "Variable ~a not found." exp))
       `(:LD ,(lookup exp env) ,@c)))
    ((consp exp)
     (let* ((fn (car exp))
            (args (cdr exp))
            (argl (length args)))
       (cond
         ((equal fn 'quote)
          (comp-error exp "Not implement yet: quote"))
         ((equal fn 'if)
          (match exp
            (('if <test> <consequent> <alternate>)
             (let ((ct (comp <consequent> env '(:JOIN)))
                   (cf (comp <alternate> env '(:JOIN))))
               (comp <test> env `(:SEL ,ct ,cf ,@c))))
            (t (comp-error exp "Unexpected if form."))))
         ((equal fn 'lambda)
          (match exp
            (('lambda <formals> . <body>)
             (let ((new-env (extend-env <formals> env)))
               `(:LDF ,(reduce #'(lambda (e cont)
                                   (comp e new-env cont)) <body>
                                   :from-end t :initial-value '(:RTN))
                      ,@c)))
            (t (comp-error exp "Unexpected lambda form."))))
         ((equal fn 'let)
          (match exp
            (('let <bindings> . <body>)
             (let ((vars (mapcar #'car <bindings>))
                   (inits (mapcar #'cadr <bindings>)))
               (comp `((lambda ,vars ,@<body>) ,@inits) env c)))
            (t (comp-error exp "Unexpected let form."))))
         ((equal fn 'letrec)
          (match exp
            (('letrec <bindings> . <body>) ;; BE CARE TODO check me
             (let ((vars (mapcar #'car <bindings>))
                   (inits (reverse (mapcar #'cadr <bindings>))))
               `(:DUM :NIL
                      ,@(loop :for init :in inits
                           :append (comp init (extend-env vars env) '(:CONS)))
                      :LDF
                      ,(reduce #'(lambda (e cont)
                                   (comp e (extend-env vars env) cont))
                               <body> :from-end t :initial-value '(:RTN))
                      :RAP ,@c))) ;; fixme :RAP ,@c is ok??
            (t (comp-error exp "Unexpected letrec form."))))
         ((equal fn 'begin)
          (comp-error exp "Not implement yet."))
         ((equal fn 'set!)
          (match exp
            (('set! <variable> <expression>)
             (comp <expression> env `(:SET ,(lookup <variable> env) ,@c)))
            (t (comp-error exp "Unexpected set! form."))))
         ((equal fn 'call/cc)
          (match exp
            (('call/cc proc)
             (cond
               ((null c) (comp-error exp "Unexpected call/cc. Continuation is null."))
               ((equal c '(:RTN)) `(:LDCT (:RTN) ,@(comp proc env `(:TAP))))
               (t
                `(:LDCT ,c ,@(comp proc env `(:AP ,@c))))))
            (t (comp-error exp "Unexpected call/cc form"))))
         ((equal fn 'define)
          (comp-error exp "define not impl. yet."))
         ((equal fn 'apply)
          (comp-error exp "apply not impl. yet."))
         ((equal fn '+)
          (case argl
            ((0) ;; (+) => 0
             (comp 0 env c))
            ((1) ;; (+ x) => x
             (comp (car args) env c))
            ((2) ;; (+ x y)
             (comp (cadr args) env (comp (car args) env `(:+ ,@c))))
            (t ;;(+ x y z) => (+ (+ x y) z)
             (comp-error exp "not impl yet"))))

         ;; primitive re-define
         ;; keyword
         (t
          (match exp
            ;; Numerical operations
            ;;(('+ z1 z2) (comp z2 env (comp z1 env `(:+ ,@c))))
            (('- z1 z2) (comp z2 env (comp z1 env `(:- ,@c))))
            (('* z1 z2) (comp z2 env (comp z1 env `(:* ,@c))))
            (('> z1 z2) (comp z2 env (comp z1 env `(:> ,@c))))
            (('>= z1 z2) (comp z2 env (comp z1 env `(:>= ,@c))))
            (('< z1 z2) (comp z2 env (comp z1 env `(:< ,@c))))
            (('<= z1 z2) (comp z2 env (comp z1 env `(:<= ,@c))))
            (('= z1 z2) (comp z2 env (comp z1 env `(:= ,@c))))
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
            )))))))

(defun comp-old (exp env c)
  "Compile an expression EXP."
  (cond
    ((null exp) c)
    ((self-evaluating-p exp)
     (comp-old () env `(:LDC ,exp ,@c)))
    ;; Variable access
    ((atom exp)
     (let ((location (lookup exp env)))
       (unless location
         (error "variable not found: ~a" exp))
       (comp-old () env `(:LD ,(lookup exp env) ,@c))))
    ((consp exp)
     (let* ((fn (car exp))
            (args (cdr exp))
            (argl (length args)))
       (cond
         ((equal fn 'quote)
          (error "not impl. yet: quote"))
         ((equal fn 'if)
          (match exp
            (('if <test> <consequent> <alternate>)
             (let ((ct (comp-old <consequent> env '(:JOIN)))
                   (cf (comp-old <alternate> env '(:JOIN))))
               (comp-old <test> env `(:SEL ,ct ,cf ,@c))))
            (t (error "comp if error: ~a" exp))))
         ((equal fn 'lambda)
          (match exp
            (('lambda <formals> . <body>)
             (let ((new-env (extend-env <formals> env)))
               `(:LDF ,(reduce #'(lambda (e cont)
                                   (comp-old e new-env cont)) <body> :from-end t :initial-value '(:RTN))
                      ,@c)))
            (t (error "comp lambda error"))))
         ((equal fn 'let)
          (match exp
            (('let <bindings> . <body>)
             (let ((vars (mapcar #'car <bindings>))
                   (inits (mapcar #'cadr <bindings>)))
               (format t ";; let convert cont: ~a~%" `((lambda ,vars ,@<body>) ,@inits))
               ;; (format t ";; let convert cont: ~a~%" c)
               ;; (format t ";; ~a~%" `((lambda ,vars ,@<body>) ,@inits))
               (comp-old `((lambda ,vars ,@<body>) ,@inits) env c)))
            (t (error "comp let error"))))
         ((equal fn 'letrec)
          (match exp
            (('letrec <bindings> . <body>)
             (let ((vars (mapcar #'car <bindings>))
                   (inits (reverse (mapcar #'cadr <bindings>))))
               `(:DUM :NIL
                      ,@(loop :for init :in inits
                           :append (comp-old init (extend-env vars env) '(:CONS))) ;; be care 
                      :LDF
                      ,(reduce #'(lambda (e cont)
                                   (comp-old e (extend-env vars env) cont))
                               <body> :from-end t :initial-value '(:RTN))
                      :RAP ,@c))) ;; fixme :RAP ,@c is ok??
            (t (error "comp letrec error"))))
         ((equal fn 'begin)
          (error "comp begin not impl. yet"))
         ((equal fn 'set!)
          (match exp
            (('set! <variable> <expression>)
             (comp-old <expression> env `(:SET ,(lookup <variable> env) ,@c)))
            (t (error "comp set! error"))))
         ((equal fn 'call/cc)
          (match exp
            (('call/cc proc)
             (cond
               ((null c) (error "call/cc found null!"))
               ((equal c '(:RTN)) `(:LDCT (:RTN) ,@(comp-old proc env `(:TAP))))
               (t
                (format t ";; call/cc c=~a~%" c)
                (format t ";; call/cc exp=~a~%" exp)
                (format t ";; call/cc proc=~a~%" proc)
                `(:LDCT ,c ,@(comp-old proc env `(:AP ,@c))))))
            (t (error "comp call/cc error"))))
         
         ((equal fn 'define)
          (error "comp define not impl. yet"))
         ((equal fn 'apply)
          (error "comp apply not impl. yet"))
         ;; primitive re-define
         ;; keyword
         (t
          (match exp
            ;; Numerical operations
            (('+ z1 z2) (comp-old z2 env (comp-old z1 env `(:+ ,@c))))
            (('- z1 z2) (comp-old z2 env (comp-old z1 env `(:- ,@c))))
            (('* z1 z2) (comp-old z2 env (comp-old z1 env `(:* ,@c))))
            (('> z1 z2) (comp-old z2 env (comp-old z1 env `(:> ,@c))))
            (('>= z1 z2) (comp-old z2 env (comp-old z1 env `(:>= ,@c))))
            (('< z1 z2) (comp-old z2 env (comp-old z1 env `(:< ,@c))))
            (('<= z1 z2) (comp-old z2 env (comp-old z1 env `(:<= ,@c))))
            (('= z1 z2) (comp-old z2 env (comp-old z1 env `(:= ,@c))))
            (('mod z1 z2) (comp-old z2 env (comp-old z1 env `(:mod ,@c))))
            ;; Pair and lists
            (('cons obj1 obj2) (comp-old obj2 env (comp-old obj1 env `(:CONS ,@c))))
            (('car pair) (comp-old pair env `(:CAR ,@c)))
            (('cdr pair) (comp-old pair env `(:CDR ,@c)))
            (('consp obj) (comp-old obj env `(:CONSP ,@c)))
            (('pair? obj) (comp-old obj env `(:CONSP ,@c)))
            ;; Vectors
            (('vector-length e1) (comp-old e1 env `(:VLEN ,@c)))
            (('vector . rest)
             `(:NIL ,@(loop :for e :in (reverse rest) :append (comp-old e env '(:CONS))) :L2V ,@c))
            (('vector-ref vec n) ;; (vector-ref vec n)
             (comp-old vec env (comp-old n env `(:VREF ,@c))))
            (('vector-set! vec n obj) ;; (vector-set! vec n obj)
             (comp-old vec env (comp-old n env (comp-old obj env `(:VSET ,@c)))))
            ;; Input and Output
            (('write obj)
             (comp-old obj env `(:WRITE ,@c)))
            ;; debug
            (('debug)
             (comp-old () env `(:DEBUG ,@c)))
            (t
             (if (= argl 0)
                 `(:NIL ,@(comp-old fn env `(:AP ,@c)))
                 `(:NIL
                   ,@(loop :for en :in (reverse args) :append (comp-old en env `(:CONS)))
                   ,@(comp-old fn env `(:AP ,@c)))))
            )))))))

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
