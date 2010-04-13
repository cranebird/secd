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
     :finally
     (error "fail to lookup ~a in ~a" var env)))

(defun extend-env (plist env)
  "Extend environment in compile time."
  (append
   (list (loop for idx from 1 for var in plist
            collect (cons var idx))) env))

;;; Compiler
(defun compile-pass1 (exp env)
  "Compile s-expression EXP in environment ENV."
  (cond
    ((null exp) nil)
    ((numberp exp) `(:LDC ,exp))
    ((eql exp #t) `(:LDC #t))
    ((eql exp #f) `(:LDC #f))
    ((symbolp exp) `(:LD ,(lookup exp env)))
    ((consp exp)
     (destructuring-bind (op . rest) exp
       (cond
         ((member op '(+ - * > < =))
          (destructuring-bind (a b) rest
            `(,@(compile-pass1 b env) ,@(compile-pass1 a env)
                ,(as-keyword op))))
         ((member op '(cons))
          (destructuring-bind (a b) rest
            `(,@(compile-pass1 b env) ,@(compile-pass1 a env)
                ,(as-keyword op))))
         ((eql op 'if)
          (destructuring-bind (e1 e2 e3) rest
            `(,@(compile-pass1 e1 env) :SEL
                (,@(compile-pass1 e2 env) :JOIN)
                (,@(compile-pass1 e3 env) :JOIN))))
         ((eql op 'lambda)
          (destructuring-bind (plist body) rest
            `(:LDF ,(append (compile-pass1 body (extend-env plist env)) '(:RTN)))))
         ((eql op 'let) ;; (let ((x 3)) body) ==  ((lambda (x) body) 3)
          (destructuring-bind (bindings body) rest
            (let ((vars (mapcar #'car bindings))
                  (inits (mapcar #'cadr bindings)))
              (compile-pass1
               `((lambda ,vars ,body) ,@inits) env))))
         ((eql op 'letrec) ;; (('letrec ((xk fk) ...) body)
          (destructuring-bind (bindings body) rest
            (let ((vars (mapcar #'car bindings))
                  (inits (reverse (mapcar #'cadr bindings))))
              `(:DUM :NIL
                ,@(loop :for init :in inits
                    :append (append (compile-pass1 init (extend-env vars env)) '(:CONS)))
                :LDF
                ,(append (compile-pass1 body (extend-env vars env)) '(:RTN))
                :RAP))))
         (t ;; (e ek ...)
          `(:NIL
            ,@(loop :for en :in (reverse rest)
                 :append (append (compile-pass1 en env) '(:CONS)))
            ,@(compile-pass1 op env) :AP)))))
    (t
     (error "compile-pass1 unknown: ~a" exp))))

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
        ((:NIL :AP :RTN :CONS :RAP :DUM :+ :- :* :> :< := :JOIN) ; no label
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
      (append-code :STOP vec)
      (make-array (length vec)
                  :initial-contents vec))))


