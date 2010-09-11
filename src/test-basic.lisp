;; -*- coding:utf-8 -*-
;; test-secd.lisp - The SECD Machine in Common Lisp
;; Copyright (C) 2010 by cranebird
;; my blog: http://d.hatena.ne.jp/cranebird/ (in Japanese)
;; github:
;; Based on the lecture note by Prof. Jia-Huai You
;; (http://www.cs.ualberta.ca/~you/courses/325/Mynotes/Fun/SECD-slides.html)
;; And LispMe.

(in-package :secd)

(defun test-doc ()
  "check documentation"
  (let ((docs
         (loop :for sym :being :the :present-symbols :in (find-package :secd)
            :if (and (fboundp sym) (documentation sym 'function))
            :collect (cons (string sym) (documentation sym 'function)))))
    (loop :for (sym . doc) :in (sort docs #'string-lessp :key #'car)
       :do (format t "~24,,,a : ~a~%" sym doc))))

(defun test-secd-eval (exp &key (debug nil) (circle t) (optimize t))
  (let* ((c (compile-pass1 exp))
         (opt-c (opt c)))
    (let ((*secd-debug* debug)
          (*print-circle* circle))
      (format t "code: ~%~s~%" c)
      (format t "code(optimized): ~%~s~%" opt-c)
      (secd 's0 'e0 `(,@(if optimize opt-c c) . c0) 'd0))))

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

(deftest test-- ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from -100 :to 100
                       :for j :from 0 :to 200
                       :collect `(= ,(- i j) (test-secd-eval '(- ,i ,j)))))))
    (gencheck)))

(deftest test-+- ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from -100 :to 100
                       :for j :from 0 :to 200
                       :for k = 3
                       :collect `(= ,(+ i (- j k)) (test-secd-eval '(+ ,i (- ,j ,k))))))))
    (gencheck)))

(deftest test-* ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from -100 :to 100
                       :for j :from 0 :to 200
                       :collect `(= ,(* i j) (test-secd-eval '(* ,i ,j)))))))
    (gencheck)))

(deftest test-sel-1 ()
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from 0 :to 100
                       :for j :from -100 :to 0
                       :collect `(= ,i (test-secd-eval '(if #t ,i ,j)))))))
    (gencheck)))

(deftest test-sel-2 ()  ;; fail scheme-f valued as #t!
  (macrolet ((gencheck ()
               `(check
                  ,@(loop :for i :from 0 :to 100
                       :for j :from -100 :to 0
                       :collect `(= ,j (test-secd-eval '(if #f ,i ,j)))))))
    (gencheck)))

(deftest test-basic-eval ()
  (combine-results
    (test-+)
    (test--)
    (test-+-)
    (test-*)
    (test-sel-1)
    ;(test-sel-2)
    ))

(deftest test-lambda-1 ()
  (check
    (= 0 (test-secd-eval '((lambda () 0))))))

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

(deftest test-basic-lambda ()
  (combine-results
    (test-lambda-1)
    (test-lambda-2)
    (test-lambda-3)
    (test-lambda-4)
    (test-lambda-5)
    (test-lambda-6)
    ))

(deftest test-fib ()
  (labels ((fib (n)
             (if (< n 2)
                 n
                 (+ (fib (- n 1)) (fib (- n 2))))))
    (macrolet ((gencheck ()
                 `(check
                    ,@(loop :for i :from 0 :to 20
                         :collect `(= (fib ,i)
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

;; (deftest test-call/cc-letrec ()
;;   (check
;;     (= 3628800 (test-secd-eval '(letrec ((f (lambda (n)
;;                                          (if (= n 0)
;;                                              1
;;                                              (* n (f (- n 1)))))))
;;                             (f 10))))
;;     (= 3628800 (test-secd-eval '(letrec ((f (lambda (n)
;;                                               (call/cc (lambda (c)
;;                                                          (if (= n 0)
;;                                                              (c 1)
;;                                                              (* n (f (- n 1)))))))))
;;                                  (f 10))))))




;; evaluate
(deftest test-basic-all ()
  (combine-results
    (test-basic-eval)
    (test-basic-lambda)
    (test-fib)
    (test-call/cc)))
