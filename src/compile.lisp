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

(defun lookup (var env)
  "Lookup the variable VAR in environment ENV in compile time."
  (loop :for e :in env :for level :from 1
     :if (assoc var e)
     :return (cons level (cdr (assoc var e)))
     :finally (error "fail to lookup ~a in ~a" var env)))

(defun extend-env (plist env)
  "Extend environment in compile time."
  (append
   (list (loop :for idx :from 1 :for var :in plist
            :collect (cons var idx))) env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun instruction-p (x) (keywordp x))

(defun comp (exp env c)
  "Compile an expression."
  (cond
    ((null exp) c)
    ((numberp exp) (comp () env `(:LDC ,exp ,@c)))
    ((eql exp #t) (comp () env `(:LDC #t ,@c)))
    ((eql exp #f) (comp () env `(:LDC #f ,@c)))
    ;; Variable references
    ((symbolp exp) (comp () env `(:LD ,(lookup exp env) ,@c)))
    ((consp exp)
     (match exp
       ;; Procedures
       (('lambda <formals> . <body>)
        ;; todo 20100922
        (let ((new-env (extend-env <formals> env)))
          `(:LDF ,(append (loop :for b :in (butlast <body>) :append (comp b new-env ()))
                          (comp (car (last <body>)) new-env '(:RTN)))
                 ,@c))

        ;; better?? 20100922
        ;; (let ((new-env (extend-env <formals> env)))
        ;;   `(:LDF ,(comp <body> new-env '(:RTN))
        ;;          ,@c))
        )
       ;; Conditionals
       (('if <test> <consequent> <alternate>)
        (let ((ct (comp <consequent> env '(:JOIN)))
              (cf (comp <alternate> env '(:JOIN))))
          (comp <test> env `(:SEL ,ct ,cf ,@c))))
       ;; Assignments
       (('set! <variable> <expression>)
        (comp <expression> env `(:SET ,(lookup <variable> env) ,@c)))
       ;; Binding Constructs
       (('let <bindings> . <body>)
        (let ((vars (mapcar #'car <bindings>))
              (inits (mapcar #'cadr <bindings>)))
          (format t ";; let convert~%")
          (format t ";; ~a~%" `((lambda ,vars ,@<body>) ,@inits))
          (comp `((lambda ,vars ,@<body>) ,@inits) env c)))
       (('letrec <bindings> . <body>)
        (let ((vars (mapcar #'car <bindings>))
              (inits (mapcar #'cadr <bindings>)))
          `(:DUM :NIL
                 ,@(loop :for init :in inits :append (comp init (extend-env vars env) '(:CONS)))
                 :LDF
                 ,(append (loop :for b :in (butlast <body>) :append (comp b (extend-env vars env) ()))
                          (comp (car (last <body>)) (extend-env vars env) '(:RTN)))
                 :RAP ,@c)))
       ;; Numerical operations
       (('+ z1 z2) (comp z2 env (comp z1 env `(:+ ,@c))))
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
        `(:NIL ,@(loop :for e :in (reverse rest) :append (comp e env '(:CONS))) :L2V ,@c))
       (('vector-ref vec n) ;; (vector-ref vec n)
        (comp vec env (comp n env `(:VREF ,@c))))
       (('vector-set! vec n obj) ;; (vector-set! vec n obj)
        (comp vec env (comp n env (comp obj env `(:VSET ,@c)))))
       ;; Control features
       (('call/cc proc)
        (cond
          ((null c) (error "call/cc found null!"))
          ((equal c '(:RTN)) `(:LDCT (:RTN) ,@(comp proc env `(:TAP))))
          (t
           (format t ";; call/cc c=~a~%" c)
           (format t ";; call/cc exp=~a~%" exp)
           `(:LDCT ,c ,@(comp proc env `(:AP ,@c))))))
       ;; (('call/cc proc)
       ;;  (cond
       ;;    ((null c) (error "call/cc found null!"))
       ;;    ((equal c '(:RTN)) `(:LDCT (:RTN) ,@(comp proc env `(:TAP))))
       ;;    (t
       ;;     (format t ";; call/cc c=~a~%" c)
       ;;     (format t ";; call/cc exp=~a~%" exp)
       ;;     `(:LDCT ,c ,@(comp proc env `(:AP ,@c))))))
       ;; Input and Output
       (('write obj)
        (comp obj env `(:WRITE ,@c)))
       (t
        `(:NIL
          ,@(loop :for en :in (reverse (cdr exp)) :append (comp en env '(:CONS)))
          ,@(comp (car exp) env '(:AP)) ,@c)
        ;; (if (atom (car exp)) ;; (e ek ...)
        ;;     `(:NIL
        ;;       ,@(loop :for en :in (reverse (cdr exp)) :append (comp en env '(:CONS)))
        ;;       ,@(comp (car exp) env '(:AP)) ,@c)
        ;;     (comp (car exp) env (cdr exp)))
        )
       ))
    ;; todo
    (t
     (error "compile-pass1 unknown expression: ~a" exp))))

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
