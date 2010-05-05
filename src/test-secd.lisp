;; -*- coding:utf-8 -*-
;; test-secd.lisp - The SECD Machine in Common Lisp
;; Copyright (C) 2010 by cranebird
;; my blog: http://d.hatena.ne.jp/cranebird/ (in Japanese)
;; github:
;; Based on the lecture note by Prof. Jia-Huai You
;; (http://www.cs.ualberta.ca/~you/courses/325/Mynotes/Fun/SECD-slides.html)
;; And LispMe.

(in-package :secd)

(setf *print-pretty* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-doc ()
  "check documentation"
  (let ((docs
         (loop :for sym :being :the :present-symbols :in (find-package :secd)
            :if (and (fboundp sym) (documentation sym 'function))
            :collect (cons (string sym) (documentation sym 'function)))))
    (loop :for (sym . doc) :in (sort docs #'string-lessp :key #'car)
       :do (format t "~24,,,a : ~a~%" sym doc))))

(deftest test-lookup ()
  (check
    (equal '(1 . 1) (lookup 'a '(((a . 1) (b . 2)))))
    (equal '(1 . 2) (lookup 'b '(((a . 1) (b . 2)))))
    (equal '(1 . 2) (lookup 'b '(((a . 1) (b . 2)) ((c . 1) (d . 2)))))
    (equal '(2 . 1) (lookup 'c '(((a . 1) (b . 2)) ((c . 1) (d . 2)))))
    (equal '(1 . 1) (lookup 'a '(((a . 1) (b . 2)) ((a . 1) (b . 2)))))
    (equal '(3 . 1) (lookup 'd '(((a . 1) (b . 2)) ((a . 1) (b . 2)) ((d . 1)))))))

(deftest test-extend-env ()
  (let ((e (extend-env '(a b c) nil)))
    (check
      (equal '(1 . 1) (lookup 'a e))
      (equal '(1 . 2) (lookup 'b e))
      (equal '(1 . 3) (lookup 'c e)))
    (let ((e2 (extend-env '(d) e)))
      (check
        (equal '(2 . 1) (lookup 'a e2))
        (equal '(2 . 2) (lookup 'b e2))
        (equal '(2 . 3) (lookup 'c e2))
        (equal '(1 . 1) (lookup 'd e2)))
      (let ((e3 (extend-env '(e f g h) e2)))
        (check
          (equal '(3 . 1) (lookup 'a e3))
          (equal '(3 . 2) (lookup 'b e3))
          (equal '(3 . 3) (lookup 'c e3))
          (equal '(2 . 1) (lookup 'd e3))
          (equal '(1 . 1) (lookup 'e e3))
          (equal '(1 . 4) (lookup 'h e3)))))))

(deftest test-pass1 ()
  (flet ((cmp (expect exp)
           (equal expect (compile-pass1 exp nil))))
    (check
      (cmp '(:LDC 1) 1)
      (cmp '(:LDC 7 :LDC 13 :+) '(+ 13 7))
      (cmp '(:LDC 7 :LDC 13 :-) '(- 13 7))
      (cmp '(:LDC 13 :LDC 7 :LDC 3 :+ :-) '(- (+ 3 7) 13))
      (cmp '(:LDC 888 :SEL (:LDC 13 :JOIN) (:LDC 9 :JOIN)) '(if 888 13 9))
      (cmp '(:LDF (:LDC 13 :RTN)) '(lambda () 13))
      (cmp '(:LDF (:LD (1 . 1) :RTN)) '(lambda (x) x))
      (cmp '(:NIL :LDF (:LD (1 . 1) :RTN) :AP) '((lambda (x) x)))
      (cmp '(:NIL :LDF (:LD (1 . 2) :RTN) :AP) '((lambda (x y) y)))
      (cmp '(:NIL :LDF (:LD (1 . 2) :LD (1 . 1) :+ :RTN) :AP) '((lambda (x y) (+ x y)))))))

(defun pass2 (program)
  (let ((vec (make-array 0 :adjustable t :fill-pointer 0))
        (ht (make-hash-table)))
    (compile-pass2 program vec ht)
    vec))

(deftest test-pass2 ()
  (labels ((pass2 (program)
             (let ((vec (make-array 0 :adjustable t :fill-pointer 0))
                   (ht (make-hash-table)))
               (compile-pass2 program vec ht)
               vec))
           (cmp (expect program) ;; NO EXP
             (equalp expect (pass2 program))))
    (check
      (cmp #(:LDC 1) '(:LDC 1))
      (cmp #(:LDC 7 :LDC 13 :+) '(:LDC 7 :LDC 13 :+))
      (cmp #(:LDC 7 :LDC 13 :-) '(:LDC 7 :LDC 13 :-))
      (cmp #(:LDC 13 :LDC 7 :LDC 3 :+ :-) '(:LDC 13 :LDC 7 :LDC 3 :+ :-))
      (cmp #(:LDC 888 :SEL 6 9 12 :LDC 13 :JOIN :LDC 9 :JOIN) '(:LDC 888 :SEL (:LDC 13 :JOIN) (:LDC 9 :JOIN)))
      (cmp #(:LDF 3 6 :LDC 13 :RTN)  '(:LDF (:LDC 13 :RTN)))
      (cmp #(:LDF 3 7 :LD 1 1 :RTN) '(:LDF (:LD (1 . 1) :RTN)))
      (cmp #(:NIL :LDF 4 8 :LD 1 1 :RTN :AP) '(:NIL :LDF (:LD (1 . 1) :RTN) :AP))
      (cmp #(:NIL :LDF 4 8 :LD 1 2 :RTN :AP) '(:NIL :LDF (:LD (1 . 2) :RTN) :AP))
      (cmp #(:NIL :LDF 4 12 :LD 1 2 :LD 1 1 :+ :RTN :AP) '(:NIL :LDF (:LD (1 . 2) :LD (1 . 1) :+ :RTN) :AP)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-tagged-pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-immediate ()
  (check
    (= #b000 (immediate-rep 0))
    (= #b100 (immediate-rep 1))
    (= #b1100 (immediate-rep 3))
    (= #b1100 (immediate-rep 3))
    (= bool-t (immediate-rep '#t))
    (= bool-f (immediate-rep '#f))))

(deftest test-convert ()
  (check
    (eql (convert-scheme-obj :bool-t '#b111) '#t)
    (= (convert-scheme-obj :fixnum 0) #b00)
    (= (convert-scheme-obj :fixnum #b1100) 3)
    (= (convert-scheme-obj :fixnum #b10100) 5)
    (= (convert-scheme-obj :fixnum #b00000000000000000000000000000000) 0)
    (= (convert-scheme-obj :fixnum #b11111111111111111111111111111100) -1)))

(deftest test-fixnum ()
  (labels ((fn (x)
             (convert-scheme-obj :fixnum (immediate-rep x))))
    (check
      (= 0 (fn 0))
      (= 1 (fn 1))
      (= 2 (fn 2))
      (= -1 (fn -1))
      (= -2 (fn -2)))))

(deftest test-tagged-pointer ()
  (combine-results
    (test-immediate)
    (test-convert)
    (test-fixnum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-run-nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test inst :NIL
(deftest test-nil-1 ()
  (let ((vm (run-code #(:NIL :STOP))))
    (check
      (eql () (value-of (vm-car vm (sp-of vm)))))))

;; test inst :NIL
(deftest test-nil-2 ()
  (let ((vm (run-code #(:NIL :NIL :STOP))))
    (check
      (eql () (value-of (vm-car vm (sp-of vm))))
      (eql () (value-of (vm-car vm (vm-cdr vm (sp-of vm))))))))

(deftest test-nil ()
  (combine-results
    (test-nil-1)
    (test-nil-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-run-ldc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-ldc-1 ()
  (let ((vm (run-code #(:LDC 3 :STOP))))
    (check
      (eql 3 (value-of (vm-car vm (sp-of vm)))))))

(deftest test-ldc-2 ()
  (let ((vm (run-code #(:LDC 7 :LDC 0 :STOP))))
    (check
      (eql 0 (value-of (vm-car vm (sp-of vm))))
      (eql 7 (value-of (vm-car vm (vm-cdr vm (sp-of vm))))))))

(deftest test-ldc-3 ()
  (let ((vm (run-code #(:LDC 4 :LDC 0 :LDC 12 :STOP))))
    (let* ((the-car (vm-car vm (sp-of vm)))
           (the-cdr (vm-cdr vm (sp-of vm)))
           (the-cadr (vm-car vm the-cdr))
           (the-caddr (vm-car vm (vm-cdr vm the-cdr))))
      (check
        (eql 12 (value-of the-car))
        (eql 0 (value-of the-cadr))
        (eql 4 (value-of the-caddr))))))

(deftest test-ldc ()
  (combine-results
    (test-ldc-1)
    (test-ldc-2)
    (test-ldc-3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-binary-insn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-binary-insn-1 ()
  (let ((vm (run-code #(:LDC 3 :LDC 7 :+ :STOP))))
    (check
      (eql 10 (value-of (vm-car vm (sp-of vm)))))))

(deftest test-binary-insn-2 ()
  ;; (- 3 5)
  (let ((vm (run-code #(:LDC 5 :LDC 3 :- :STOP))))
    (check
      (eql -2 (value-of (vm-car vm (sp-of vm)))))))

(deftest test-binary-insn-3 ()
  ;; (+ (- 3 5) 7)
  (let ((vm (run-code #(:LDC 5 :LDC 3 :- :LDC 7 :+ :STOP))))
    (check
      (eql 5 (value-of (vm-car vm (sp-of vm)))))))

(deftest test-binary-insn-4 ()
  (let ((vm (run-code #(:LDC 3 :LDC 7 :> :STOP))))
    (check
      (eql :scheme-t (value-of (vm-car vm (sp-of vm)))))))

(deftest test-binary-insn-5 ()
  (let ((vm (run-code #(:LDC 3 :LDC 7 :< :STOP))))
    (check
      (eql :scheme-f (value-of (vm-car vm (sp-of vm)))))))

(deftest test-binary-insn ()
  (combine-results
    (test-binary-insn-1)
    (test-binary-insn-2)
    (test-binary-insn-3)
    (test-binary-insn-4)
    (test-binary-insn-5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-sel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-sel-1 ()
  (let ((vm (run '(if #t 8 11))))
    (check
      (eql 8 (value-of (vm-car vm (sp-of vm)))))))

(deftest test-sel-2 ()
  (let ((vm (run '(if #f 8 11))))
    (check
      (eql 11 (value-of (vm-car vm (sp-of vm)))))))

(deftest test-sel-3 ()
  (let ((vm (run '(if (> 999 0) 8 11))))
    (check
      (eql 8 (value-of (vm-car vm (sp-of vm)))))))

(deftest test-sel-4 ()
  (let ((vm (run '(if (< 999 0) 8 11))))
    (check
      (eql 11 (value-of (vm-car vm (sp-of vm)))))))

(deftest test-sel-5 ()
  (let ((vm (run '(if (= 999 0) 8 11))))
    (check
      (eql 11 (value-of (vm-car vm (sp-of vm)))))))

(deftest test-sel ()
  (combine-results
    (test-sel-1)
    (test-sel-2)
    (test-sel-3)
    (test-sel-4)
    (test-sel-5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (compile-exp '(lambda () 12))
;; => #(:LDF 3 6 :LDC 12 :RTN :STOP)

(deftest test-ldf-1 ()
  (let ((vm (run '((lambda () (+ 3 4))))))
    (check
      (= 7 (value-of (vm-stack-top vm))))))

(deftest test-ldf-2 ()
  (let ((vm (run '((lambda () 0)))))
    (check
      (= 0 (value-of (vm-stack-top vm))))))

(deftest test-ldf-3 ()
  (let ((vm (run '((lambda (x) 5) 10))))
    (check
      (= 5 (value-of (vm-stack-top vm))))))

(deftest test-ldf-4 ()
  (let ((vm (run '((lambda (x) (+ 3 4)) 10))))
    (check
      (= 7 (value-of (vm-stack-top vm))))))

(deftest test-ldf-5 ()
  (let ((vm (run '((lambda (x y) (+ 3 4)) 10 20))))
    (check
      (= 7 (value-of (vm-stack-top vm))))))

(deftest test-ldf-6 ()
  (let ((vm (run '((lambda (x y z) (* 3 4)) 10 (+ 20 5) 30))))
    (check
      (= 12 (value-of (vm-stack-top vm))))))

(deftest test-ldf-7 () ;; use arg
  (let ((vm (run '((lambda (x) x) 13))))
    (check
      (= 13 (value-of (vm-stack-top vm))))))

(deftest test-ldf-8 ()
  (let ((vm (run  '((lambda (x) (* 2 x)) 13))))
    (check
      (= 26 (value-of (vm-stack-top vm))))))

(deftest test-ldf-9 ()
  (let ((vm (run  '((lambda (x y) 20) 7 13))))
    (check
      (= 20 (value-of (vm-stack-top vm))))))

(deftest test-ldf-10 ()
  (let ((vm (run  '((lambda (x y) x) 7 13))))
    (check
      (= 7 (value-of (vm-stack-top vm))))))

(deftest test-ldf-11 () ;;
  (let ((vm (run  '((lambda (x y) y) 7 13))))
    (check
      (eql 13 (value-of (vm-stack-top vm))))))

(deftest test-ldf-12 ()
  (let ((vm (run  '((lambda (x y) (+ x y)) 7 13))))
    (check
      (= 20 (value-of (vm-stack-top vm))))))

(deftest test-ldf-13 ()
  (let ((vm (run  '((lambda (x y) (- x y)) 5 18))))
    (check
      (= -13 (value-of (vm-stack-top vm))))))

(deftest test-ldf-14 ()
  (let ((vm (run  '((lambda (x y) (* 2 (+ x y))) 3 5))))
    (check
      (= 16 (value-of (vm-stack-top vm))))))

(deftest test-ldf-15 ()
  (let ((vm (run  '((lambda (x y) (= x y)) 3 5))))
    (check
      (eql #f (value-of (vm-stack-top vm))))))

(deftest test-ldf ()
  (combine-results
    (test-ldf-1)
    (test-ldf-2)
    (test-ldf-3)
    (test-ldf-4)
    (test-ldf-5)
    (test-ldf-6)
    (test-ldf-7)
    (test-ldf-8)
    (test-ldf-9)
    (test-ldf-10)
    (test-ldf-11)
    (test-ldf-12)
    (test-ldf-13)
    (test-ldf-14)
    (test-ldf-15)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-basic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-basic ()
  (combine-results
    (test-pass1)
    (test-pass2)
    (test-tagged-pointer)
    (test-nil)
    (test-ldc)
    (test-binary-insn)
    (test-sel)
    (test-ldf)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-run-base-1 ()
  (labels ((cmp (expect exp)
             (equal expect (value-of (vm-stack-top (run exp))))))
    (check
      (cmp 3 3)
      (cmp 3 '(+ 1 2))
      (cmp 11 '(+ (+ 4 5) 2))
      (cmp 2 '(- 3 1)))))

(deftest test-run-base-2 ()
  (labels ((cmp (expect exp)
             (equal expect (value-of (vm-stack-top (run exp))))))
    (check
      (cmp -1 '((lambda (x y) (- x y)) 2 3))
      (cmp 4 '((lambda (z) ((lambda (x y) (+ (- x y) z)) 3 5)) 6)))))

(deftest test-run-let ()
  (labels ((cmp (expect exp)
             (equal expect (value-of (vm-stack-top (run exp))))))
    (check
      (cmp 12 '(let ((x 12)) x))
      (cmp 8 '(let ((x 3) (y 8)) y))
      (cmp 11 '(let ((x 3) (y 8))
                (+ x y)))
      (cmp 5 '(let ((x 3) (y 8) (z 1))
                (- y x))))))

(deftest test-run-lambda ()
  (labels ((cmp (expect exp)
             (equal expect (value-of (vm-stack-top (run exp))))))
    (check
      (cmp 20 '(let ((fn (lambda (x) (* 2 x))))
                (fn 10))))))

(deftest test-run-lambda-2 ()
  (labels ((cmp (expect exp)
             (equal expect (value-of (vm-stack-top (run exp))))))
    (check
      (cmp 12 '(let ((fn (lambda (x) (* 2 x))))
                (fn (fn 3))))
      (cmp 24 '(let ((fn (lambda (x) (* 2 x))))
                (fn (fn (fn 3)))))
      (cmp 11 '(let ((fn (lambda (x) (* 2 x)))
                     (gn (lambda (x) (+ 5 x))))
                (gn (fn 3))))
      (cmp 30 '(let ((fn (lambda (x) (* 2 x)))
                     (gn (lambda (x y) (+ x y))))
                (gn (fn 10) (fn 5))))
      (cmp 6 '(let ((fn (lambda (x) (* 2 x))))
                (let ((gn (lambda (x) (+ 5 x))))
                  (fn 3))))
      (cmp 8 '(let ((fn (lambda (x) (* 2 x))))
                (let ((fn (lambda (x) (+ 5 x))))
                  (fn 3)))))))

(deftest test-run-lambda-3 ()
  (labels ((cmp (expect exp)
             (equal expect (value-of (vm-stack-top (run exp))))))
    (check
      (cmp 8 '(let ((n 2))
                (let ((fn (lambda (x) (* n x))))
                  (fn 4))))
      (cmp 11 '(let ((n 2) (m 3))
                (let ((fn (lambda (x) (+ m (* n x)))))
                  (fn 4)))))))

(deftest test-run-lambda-4 ()
  (labels ((cmp (expect exp)
             (equal expect (value-of (vm-stack-top (run exp))))))
    (check
      (cmp 12 '(let ((fn (lambda (x) (* 2 x))))
                (fn (fn 3))))
      (cmp 24 '(let ((fn (lambda (x) (* 2 x))))
                (fn (fn (fn 3)))))
      (cmp 11 '(let ((fn (lambda (x) (* 2 x)))
                     (gn (lambda (x) (+ 5 x))))
                (gn (fn 3))))
      (cmp 19 '(let ((fn (lambda (x) (* 2 x))))
                (let ((gn (lambda (x y) (+ x (fn y)))))
                  (gn 13 3)))))))


;; 20100323 failed!
(deftest test-run-lambda-5 () ;; Y
  (labels ((cmp (expect exp)
             (equal expect (value-of (vm-stack-top (run exp))))))
    (check
      (cmp 3628800 '(((lambda (self)
                        (lambda (n)
                          (if (= 0 n)
                              1
                              (* n ((self self) (- n 1))))))
                      (lambda (self)
                        (lambda (n)
                          (if (= 0 n)
                              1
                              (* n ((self self) (- n 1))))))) 10))

      (cmp 21
           '(((lambda (f)
                ((lambda (g)
                   (f (lambda (arg) ((g g) arg))))
                 (lambda (g)
                   (f (lambda (arg) ((g g) arg))))))
              (lambda (f)
                (lambda (n)
                  (if (< n 2)
                      n
                      (+ (f (- n 1)) (f (- n 2))))))) 8))
      ;; be care failed. too many heap??
      ;; (cmp 6765
      ;;      '(((lambda (f)
      ;;           ((lambda (g)
      ;;              (f (lambda (arg) ((g g) arg))))
      ;;            (lambda (g)
      ;;              (f (lambda (arg) ((g g) arg))))))
      ;;         (lambda (f)
      ;;           (lambda (n)
      ;;             (if (< n 2)
      ;;                 n
      ;;                 (+ (f (- n 1)) (f (- n 2))))))) 20))


      (cmp 21
           '(let ((Y (lambda (f)
                       ((lambda (g)
                          (f (lambda (arg) ((g g) arg))))
                        (lambda (g)
                          (f (lambda (arg) ((g g) arg))))))))
             ((Y (lambda (f)
                   (lambda (n)
                     (if (< n 2)
                         n
                         (+ (f (- n 1)) (f (- n 2))))))) 8)))
      ;; failed also.
      ;; (cmp 610
      ;;      '(let ((Y (lambda (f)
      ;;                  ((lambda (g)
      ;;                     (f (lambda (arg) ((g g) arg))))
      ;;                   (lambda (g)
      ;;                     (f (lambda (arg) ((g g) arg))))))))
      ;;        ((Y (lambda (f)
      ;;              (lambda (n)
      ;;                (if (< n 2)
      ;;                    n
      ;;                    (+ (f (- n 1)) (f (- n 2))))))) 15)))
      )))


;; not yet 20100323
(deftest test-run-letrec ()
  (labels ((cmp (expect exp)
             (equal expect (value-of (vm-stack-top (run exp))))))
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

(deftest test-compile ()
  (combine-results 
    (test-pass1)
    (test-pass2)))

(deftest test-run ()
  (combine-results
    ;(test-run-base)
    (test-run-let)
    (test-run-lambda)
    (test-run-lambda-2)
    (test-run-lambda-3)
    (test-run-lambda-4) ;; TODO use many heap
    (test-run-letrec) ;; TODO use many heap
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-all ()
  (combine-results
    (test-lookup)
    (test-compile)
    (test-run)))

