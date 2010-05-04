;; -*- coding:utf-8 -*-
;; launch.lisp - The SECD Machine in Common Lisp
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
;; run
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-vm (vm)
  "run vm."
  (handler-case 
      (loop (dispatch (fetch-insn vm) vm))
    (stop-vm-condition ()
      (progn
        (format *debug-io* ";; handle stop-vm-condition~%")
        vm))))

;; (defun run-code (code)
;;   "Run compiled-code in new VM."
;;   (let ((vm (make-vm code)))
;;     (next vm)
;;     vm))

(defun run-code (code)
  "Run compiled-code in new VM."
  (let ((vm (make-vm code)))
    (run-vm vm)))

(defun run-time (exp)
  "Compile s-expression EXP and run."
  (let ((code (compile-exp exp)))
    (let ((vm (make-vm code)))
      (run-vm vm))))

;; (defun run-time (exp)
;;   "Compile s-expression EXP and run."
;;   (let ((code (compile-exp exp)))
;;     (let ((vm (make-vm code)))
;;       (time
;;        (next vm)))))

;; (defun run-prof (exp)
;;   (let ((code (compile-exp exp)))
;;     (let ((vm (make-vm code)))
;;       (with-prof
;;        (describe (next vm))))))

(defun run (exp)
  "Compile s-expression and run."
  (let ((code (compile-exp exp)))
    (format t ";; exp: ~a~%code: ~a~%" exp code)
    (run-code code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require :sb-sprof)
;; (defmacro with-prof (&body body)
;;   `(sb-sprof:with-profiling (:max-samples 10
;;                                           :report :flat
;;                                           :mode :alloc
;;                                           :report :flat)
;;      ,@body))
