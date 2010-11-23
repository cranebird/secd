;; -*- coding:utf-8 -*-
;; test-secd.lisp - The SECD Machine in Common Lisp
;; Copyright (C) 2010 by cranebird
;; my blog: http://d.hatena.ne.jp/cranebird/ (in Japanese)
;; github:
;; Based on the lecture note by Prof. Jia-Huai You
;; (http://www.cs.ualberta.ca/~you/courses/325/Mynotes/Fun/SECD-slides.html)
;; And LispMe.

(in-package :secd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-scm-macro-character))

(defun test-doc ()
  "Print documentation strings."
  (let ((docs
         (loop :for sym :being :the :present-symbols :in (find-package :secd)
            :if (and (fboundp sym) (documentation sym 'function))
            :collect (cons (string sym) (documentation sym 'function)))))
    (loop :for (sym . doc) :in (sort docs #'string-lessp :key #'car)
       :do (format t "~24,,,a : ~a~%" sym doc))))

(defun test-secd-eval (exp)
  "Eval S-expression EXP."
  (let* ((c (compile-pass1 exp))
         (opt-c (opt c)))
    (when *secd-debug*
      (format t "exp: ~s~%" exp)
      (format t "code: ~%~s~%" c)
      (format t "code(optimized): ~%~s~%" opt-c))
    (secd 's0 'e0 `(,@opt-c . c0) 'd0)))

;; (use-package :sb-profile)
;; (defun profile-secd (exp)
;;   (sb-profile:reset)
;;   (sb-profile:profile secd)
;;   (test-secd-eval exp :debug nil :circle nil)
;;   (sb-profile:report))

(deftest test-integer ()
  (macrolet ((gencheck ()
               `(check ,@(loop :for i :from -100 :to 100 :collect `(= ,i (test-secd-eval ,i))))))
    (gencheck)))

(deftest test-+ ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from -100 :to 100
                       :for j :from 0 :to 200
                       :collect `(= ,(+ i j) (test-secd-eval '(+ ,i ,j)))))))
    (gencheck)))

(deftest test-++ ()
  (check
    (= 0 (test-secd-eval '(+)))
    (= 5 (test-secd-eval '(+ 5)))
    (= 7 (test-secd-eval '(+ 3 4)))
    (= 9 (test-secd-eval '(+ 2 3 4)))
    (= 10 (test-secd-eval '(+ 1 2 3 4)))
    (= 10 (test-secd-eval '(+ 1 2 (+ 1 2) 4)))
    (= 7 (test-secd-eval '(+ (+ 1 2) 4)))))

(deftest test-- ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from -100 :to 100
                       :for j :from 0 :to 200
                       :collect `(= ,(- i j) (test-secd-eval '(- ,i ,j)))))))
    (gencheck)))

(deftest test--- ()
  (check
    (= -1 (test-secd-eval '(- 3 4)))
    (= -6 (test-secd-eval '(- 3 4 5)))
    (= -3 (test-secd-eval '(- 3)))
    (= (- 2 3 4 5) (test-secd-eval '(- 2 3 4 5)))
    ))

(deftest test-+- ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from -100 :to 100
                       :for j :from 0 :to 200
                       :for k = 3
                       :collect `(= ,(+ i (- j k)) (test-secd-eval '(+ ,i (- ,j ,k))))))))
    (gencheck)))


(deftest test-positive-negative ()
  (check
    (eql #t (test-secd-eval '(positive? 3)))
    (eql #f (test-secd-eval '(positive? 0)))
    (eql #f (test-secd-eval '(positive? -3)))
    (eql #f (test-secd-eval '(negative? 3)))
    (eql #f (test-secd-eval '(negative? 0)))
    (eql #t (test-secd-eval '(negative? -3)))
    (eql #t (test-secd-eval '(positive? (- 4 1))))
    (eql #f (test-secd-eval '(positive? (- 4 5))))
    (eql #t (test-secd-eval '(negative? (- 4 5))))
    (eql #f (test-secd-eval '(negative? (- 4 3))))
    ))

(deftest test-* ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from -100 :to 100
                       :for j :from 0 :to 200
                       :collect `(= ,(* i j) (test-secd-eval '(* ,i ,j)))))))
    (gencheck)))

(deftest test-** ()
  (check
    (= 4 (test-secd-eval '(* 4)))
    (= 1 (test-secd-eval '(*)))
    (= 0 (test-secd-eval '(* 0)))
    (= 6 (test-secd-eval '(* 2 3)))    
    (= (* 2 3 4) (test-secd-eval '(* 2 3 4)))
    (= (* 2 3 4 5) (test-secd-eval '(* 2 3 4 5)))
    (= (* 1) (test-secd-eval '(* 1)))))

(deftest test-= ()
  (check
    (eql #t (test-secd-eval '(= 4 4)))
    (eql #f (test-secd-eval '(= 4 2)))
    (eql #t (test-secd-eval '(= (+ 2 2) 4)))
    (eql #t (test-secd-eval '(= (+ 3 1) 4)))
    (eql #t (test-secd-eval '(= (+ 3 1) (* 2 2))))
    (eql #t (test-secd-eval '(= 2 2 2)))
    (eql #f (test-secd-eval '(= 2 2 1)))
    (eql #t (test-secd-eval '(= 8 (* 2 2 2) (+ 3 5) (- 10 2))))
    (eql #f (test-secd-eval '(= 8 (* 2 2 2) (+ 3 5) (- 10 2) 11)))))

(deftest test-abs ()
  (check
    (eql 3 (test-secd-eval '(abs 3)))
    (eql 3 (test-secd-eval '(abs -3)))
    (eql 0 (test-secd-eval '(abs 0)))))

(deftest test-if ()
  (check
    (= 4 (test-secd-eval '(if #t 4 10)))
    (= 10 (test-secd-eval '(if #f 4 10)))
    (= 8 (test-secd-eval '(if #t 8)))
    (eql #f (test-secd-eval '(if #f 32)))))

(deftest test-sel-1 ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from 0 :to 100
                       :for j :from -100 :to 0
                       :collect `(= ,i (test-secd-eval '(if #t ,i ,j)))))))
    (gencheck)))

(deftest test-sel-2 ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from 0 :to 100
                       :for j :from -100 :to 0
                       :collect `(= ,j (test-secd-eval '(if #f ,i ,j)))))))
    (gencheck)))

(deftest test-and-1 ()
  (check
    (eql #t (test-secd-eval '(and (= 2 2) (> 2 1))))
    (eql #f (test-secd-eval '(and (= 2 2) (< 2 1))))
    (eql #t (test-secd-eval '(and)))))

(deftest test-or-1 ()
  (check
    (eql #f (test-secd-eval '(or)))
    (eql #t (test-secd-eval '(or (= 2 2) (> 2 1))))
    (eql #t (test-secd-eval '(or (= 2 2) (< 2 1))))
    (eql #f (test-secd-eval '(or #f #f #f)))))

(deftest test-basic-eval ()
  (combine-results
    (test-+)
    (test-++)
    (test--)
    (test-+-)
    (test-*)
    (test-=)
    (test-positive-negative)
    (test-abs)
    (test-if)
    (test-sel-1)
    (test-sel-2)
    (test-and-1)
    (test-or-1)
    ))

(deftest test-lambda-1 ()
  (check
    (= 0 (test-secd-eval '((lambda () 0))))
    (= 13 (test-secd-eval '((lambda () 13))))
    ))

(deftest test-lambda-2 ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from -10 :to 10
                       :collect `(= ,i (test-secd-eval '((lambda (x) ,i) ,i)))))))
    (gencheck)))

(deftest test-lambda-3 ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from -10 :to 10
                       :collect `(= ,i (test-secd-eval '((lambda (x) x) ,i)))))))
    (gencheck)))

(deftest test-lambda-4 ()
  (macrolet ((gencheck (n)
               `(check
                  ,@(loop :for i :from -10 :to 10
                       :collect `(= ,(* n i) (test-secd-eval '((lambda (x) (* ,n x)) ,i)))))))
    (gencheck 2)
    (gencheck 3)
    (gencheck 10)))

(deftest test-lambda-5 ()
  "random"
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from -100 :to 100 :for j = (random 100)
                       :collect `(= ,(+ i j) (test-secd-eval '((lambda (x y) (+ y x)) ,i ,j)))))))
    (gencheck)))

(deftest test-lambda-6 ()
  "many eval"
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from -100 :to 100
                       :collect `(= ,i (test-secd-eval '((lambda () ,i ,i ,(+ i 3) ,(* i 2) ,i))))))))
    (gencheck)))

(deftest test-lambda-7 ()
  "lambda in car"
  (check
    (= 6 (test-secd-eval '((let ((f (lambda (x) (* 2 x))))
                             f) 3)))
    (= 30 (test-secd-eval '((let ((f (lambda (x) (* 2 x)))
                                  (g (lambda (x) (* 10 x))))
                              g) 3)))
    (= 60 (test-secd-eval '((let ((f (lambda (x) (* 2 x)))
                                  (g (lambda (x) (* 10 x))))
                              (lambda (n) (f (g n)))) 3)))
    ))

(deftest test-basic-lambda ()
  (combine-results
    (test-lambda-1)
    (test-lambda-2)
    (test-lambda-3)
    (test-lambda-4)
    (test-lambda-5)
    (test-lambda-6)
    (test-lambda-7)
    ))

(deftest test-begin-1 ()
  (check
    (= 13 (test-secd-eval '(begin 13)))
    (= 14 (test-secd-eval '(begin 7 14)))
    (= 15 (test-secd-eval '(begin (+ 10 5))))
    (= 16 (test-secd-eval '(begin -1 (* 2 8))))
    (= 17 (test-secd-eval '(begin 1 3 5 7 (+ 7 (* 2 5)))))
    (= 18 (test-secd-eval '(begin (+ (- 9 1) (* 2 5)))))
    ))

(deftest test-begin-2 ()
  (check
    (= 13 (test-secd-eval '(let ((x 6))
                            (set! x 7)
                            13)))
    (= 7 (test-secd-eval '(let ((x 6))
                            (set! x 7)
                            x)))
    (= 7 (test-secd-eval '(let ((x 6)) 
                            (begin
                             (set! x 7)
                             x))))
    ))

(deftest test-letrec-1 ()
  (check
    (= 3 (test-secd-eval '(letrec ((f 3)) f)))
    (= 3 (test-secd-eval '(letrec ((f 3) (g 5)) f)))
    (= 3 (test-secd-eval '(letrec ((f 3) (g 5) (h 9)) f)))
    (= 2 (test-secd-eval '(letrec ((f 3) (g 5) (h 9))
                           (- g f))))

    (= 2 (test-secd-eval '(letrec ((f 3) (g (- 4 2)))
                           g)))
    (= 8 (test-secd-eval '(letrec ((f (- 9 1))
                                   (g (- 4 2)))
                           f)))
    ))

(deftest test-letrec ()
  (labels ((cmp (expect exp)
             (equal expect (test-secd-eval exp))))
    (check
      (cmp 3 '(letrec ((f 3))
               f))
      (cmp 5 '(letrec ((y 5))
               y))
      (cmp 3 '(letrec ((x 3) (y 5))
               x))
      (cmp 5 '(letrec ((x 3) (y 5))
               y))
      (cmp 15 '(letrec ((x 3) (y 5) (z 12))
                (+ x z)))
      (cmp 12 '(letrec ((f (lambda (x) x)))
                (f 12)))
      (cmp 4 '(letrec ((f (lambda (x)
                            x))
                       (g 4))
               (f g)))
      (cmp 4 '(letrec ((f (lambda (x)
                            g))
                       (g 4))
               (f 12)))
      (cmp 8 '(letrec ((f 8)
                       (g (lambda (x) x)))
               f))
      (cmp 12 '(letrec ((f 4)
                        (g (lambda (x) x)))
                (g 12)))
      (cmp 24 '(letrec ((f 24)
                        (g (lambda (x) x)))
                (g f)))
      (cmp 4 '(letrec ((f 4)
                       (g (lambda (x) f)))
               (g 9)))
      (cmp 3628800 '(letrec ((fact (lambda (n)
                                     (if (= n 0)
                                         1
                                         (* n (fact (- n 1)))))))
                     (fact 10)))
      (cmp 3628800 '(letrec ((fact (lambda (n res)
                                     (if (= n 0)
                                         res
                                         (fact (- n 1) (* n res))))))
                     (fact 10 1)))
      (cmp 6765 '(letrec ((fib (lambda (n)
                               (if (< n 2)
                                   n
                                   (+ (fib (- n 1)) (fib (- n 2)))))))
                (fib 20)))
                
      (cmp 55 '(letrec ((sum (lambda (term a next b)
                               (if (> a b)
                                   0
                                   (+ (term a)
                                      (sum term (next a) next b))))))
                (sum (lambda (n) n) 1 (lambda (n) (+ n 1)) 10)))
      (cmp 3025 '(letrec ((sum (lambda (term a next b)
                                 (if (> a b)
                                     0
                                     (+ (term a)
                                        (sum term (next a) next b))))))
                  (sum (lambda (n) (* n (* n n))) 1 (lambda (n) (+ n 1)) 10))))))




(deftest test-fib ()
  (labels ((cl-fib (n)
             (if (< n 2)
                 n
                 (+ (cl-fib (- n 1)) (cl-fib (- n 2))))))
    (macrolet ((gencheck ()
                 `(check
                    ,@(loop :for i :from 0 :to 20
                         :collect `(= (cl-fib ,i)
                                      (test-secd-eval 
                                       '(letrec ((fib (lambda (n)
                                                        (if (< n 2)
                                                            n
                                                            (+ (fib (- n 1)) (fib (- n 2)))))))
                                         (fib ,i))))))))
      (gencheck))))
 
(deftest test-call/cc () 
  (check
    (= 8 (test-secd-eval '((lambda (n) (call/cc (lambda (c) 8))) 13)))
    (= 15 (test-secd-eval '((lambda (n) (+ n (call/cc (lambda (c) (c 2))))) 13)))
    (= 2 (test-secd-eval '((lambda (n) (call/cc (lambda (c) (+ n (c 2))))) 13)))
    (= 9 (test-secd-eval '(* 3 (call/cc (lambda (k) (+ 1 2))))))
    (= 6 (test-secd-eval '(* 3 (call/cc (lambda (k) (+ 1 (k 2)))))))
    ))

(deftest test-call/cc-2 () ;; Kent Dybvig Programming Language SCHEME (3.3 Continuation)
  (check
    (= 20 (test-secd-eval '(call/cc
                            (lambda (k)
                              (* 5 4)))))
    (= 4 (test-secd-eval '(call/cc
                           (lambda (k)
                             (* 5 (k 4))))))
    (= 6 (test-secd-eval '(+ 2 (call/cc
                                (lambda (k)
                                  (* 5 (k 4)))))))
    (equal "HEY!" (test-secd-eval
                   '(((call/cc (lambda (k) k)) (lambda (x) x)) "HEY!")))
    (equal "hi" (test-secd-eval '(let ((x (call/cc (lambda (k) k))))
                                  (x (lambda (ignore) "hi")))))
    ))

(deftest test-call/cc-3 () ;; perter_norvig lispy2 example
  (check
    (= 35 (test-secd-eval '(call/cc (lambda (throw)
                                      (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (escape 3))))))))))
    (= 3 (test-secd-eval '(call/cc (lambda (throw)
                                     (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (throw 3))))))))))

    ))

(deftest test-call/cc-letrec ()
  (check
    (= 3628800 (test-secd-eval '(letrec ((f (lambda (n)
                                              (if (= n 0)
                                                  1
                                                  (* n (f (- n 1)))))))
                                 (f 10))))
    (= 3628800 (test-secd-eval '(letrec ((f (lambda (n)
                                              (call/cc (lambda (c)
                                                         (if (= n 0)
                                                             (c 1)
                                                             (* n (f (- n 1)))))))))
                                 (f 10))))))

(deftest test-set ()
  (check
    (= 9 (test-secd-eval '(let ((x 10))
                           (set! x 9)
                           x)))
    (= 15 (test-secd-eval '(let ((x 3)
                                 (y 7))
                            (set! x 15)
                            (set! y 9)
                            x)))
    (= 3 (test-secd-eval '(let ((x 3)
                                (y 8))
                           (let ((z (+ x y)))
                             (set! z 99)
                             x))))
    (= 4 (test-secd-eval '(let ((n 0))
                            (let ((fn (lambda ()
                                        (set! n (+ n 1))
                                        n)))
                              (fn)
                              (fn)
                              (fn)
                              (fn)))))
    ))

;; evaluate
(deftest test-basic-all ()
  (combine-results
    (test-basic-eval)
    (test-basic-lambda)
    (test-begin-1)
    (test-begin-2)
    (test-letrec-1)
    (test-letrec)
    (test-fib)
    (test-call/cc)
    (test-call/cc-2)
    (test-call/cc-letrec)
    (test-set)))
