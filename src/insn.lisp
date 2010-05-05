;; -*- coding:utf-8 -*-
;; insn.lisp - VM instructions
;; Copyright (C) 2010 by cranebird
;; my blog: http://d.hatena.ne.jp/cranebird/ (in Japanese)
;; github:
;; Based on the lecture note by Prof. Jia-Huai You
;; (http://www.cs.ualberta.ca/~you/courses/325/Mynotes/Fun/SECD-slides.html)
;; And LispMe.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VM instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :secd.vm)

(defmacro vm-assert (&rest rest)
  (when *debug*
      `(assert ,@rest)))

(defvar *the-vm* nil)

(defmacro cons (a b)
  `(vm-cons *the-vm* ,a ,b))

(defmacro car (a)
  `(vm-car *the-vm* ,a))

(defmacro cdr (a)
  `(vm-cdr *the-vm* ,a))

(defmacro def-insn (name (vm) &rest body)
  "define instuction."
  (let ((insn (gensym)))
    `(defmethod dispatch ((,insn (eql ,(as-keyword name))) (,vm vm))
       ;; (declare (optimize (speed 3) (safety 0)))
       ;; (when (> (execution-count-of ,vm) 1)
       ;;   (setf (ap-of ,vm) (copying ,vm))
       ;;   (swap-space ,vm))
       ,(if *debug*
            `(let ((*print-circle* t))
               (format t "; PC: ~a insn: ~a~%" (pc-of ,vm) ,insn)
               ;; (describe ,vm)
               ;; (graphviz-vm ,vm (format nil "tmp.vm.~d.dot" (execution-count-of ,vm)))
               ))
       ,(if *profile*
            `(progn
               (incf (execution-count-of ,vm))
               (incf (gethash ,(as-keyword name) (profile-of ,vm) 0))))
       ;;
       (let ((*the-vm* ,vm))
         ,@body))))

;; STOP
(def-insn STOP (vm)
  (signal 'stop-vm-condition))

;; NIL     s e (NIL.c) d        ->  (nil.s) e c d
(def-insn NIL (vm)
  (with-accessors ((sp sp-of)) vm
    (vm-assert (eql (scheme-type-of sp) :pair) (sp) "NIL: scheme-type-of sp should be :PAIR")
    (setf sp (cons empty sp))
    (vm-assert (eql (scheme-type-of sp) :pair) (sp) "NIL: scheme-type-of sp should be :PAIR")))

;; LDC     s e (LDC x.c) d      ->  (x.s) e c d
(def-insn LDC (vm)
  (with-accessors ((pc pc-of) (sp sp-of)) vm
    (let ((c (fetch-operand vm)))
      (setf sp (cons (immediate-rep c) sp))
      (incf pc))))

(defmacro def-binary-insn (name sym)
  (let ((a (gensym))
        (b (gensym))
        (sp (gensym))
        (vm (gensym)))
    `(progn
       (def-insn ,name (,vm)
         (with-accessors ((,sp sp-of)) ,vm
           (let* ((,a (car ,sp))
                  (,b (car (cdr ,sp))))
             ;; todo; not fast
             (setf ,sp
                   (cons
                    (immediate-rep
                     (,sym (value-of ,a) (value-of ,b))) ,sp))))))))

;; +       (a b.s) e (OP.c) d   ->   ((a OP b).s) e c d
;; x = 4a; y = 4b; (x + y) = (4a + 4b) = 4(a + b)
(def-insn + (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (car sp))
           (b (car (cdr sp)))
           (res (+ a b)))
      (setf sp (cons res (cdr (cdr sp)))))))

;; x = 4a; y = 4b; (x - y) = (4a - 4b) = 4(a - b)
(def-insn - (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (car sp))
           (b (car (cdr sp)))
           (res (- a b)))
      (setf sp (cons res (cdr (cdr sp)))))))

(def-binary-insn * cl:*)

;; x == y then 4x == 4y
(def-insn = (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (car sp))
           (b (car (cdr sp)))
           (res (if (= a b) bool-t bool-f)))
      (setf sp (cons res (cdr (cdr sp)))))))

(def-insn > (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (car sp))
           (b (car (cdr sp)))
           (res (if (> (value-of a) (value-of b))
                    bool-t
                    bool-f)))
      (setf sp (cons res (cdr (cdr sp)))))))

(def-insn < (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (car sp))
           (b (car (cdr sp)))
           (res (if (< (value-of a) (value-of b))
                    bool-t
                    bool-f)))
      (setf sp (cons res (cdr (cdr sp)))))))

;; CONS
(def-insn CONS (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (car sp))
           (b (car (cdr sp))))
      (setf sp (cons (cons a b) (cdr (cdr sp)))))))

(def-insn CAR (vm)
  (with-accessors ((sp sp-of)) vm
    (setf sp (car sp))))

(def-insn CDR (vm)
  (with-accessors ((sp sp-of)) vm
    (vm-assert (eql (scheme-type-of sp) :pair) (sp) "CDR: scheme-type-of sp should be :PAIR")
    (let* ((a (car sp))
           (x (cdr a)))
      (vm-assert (eql (scheme-type-of a) :pair) (a) "CDR: scheme-type-of a should be :PAIR")
      (setf sp (cons x (cdr sp)))
      (vm-assert (eql (scheme-type-of sp) :pair) (sp) "CDR: scheme-type-of sp should be :PAIR"))))

;; SEL CT CF CONT
;; SEL     (x.s) e (SEL ct cf.c) d   ->  s e c' (c.d)
;;                 where c' = ct if x is T, and cf if x is F
(def-insn SEL (vm)
  (with-accessors ((sp sp-of) (pc pc-of) (code get-code) (dump dump-of)) vm
    (let* ((x (car sp))
           (ct (code-ref code pc))
           (cf (code-ref code (1+ pc)))
           (cont (code-ref code (+ 2 pc))))
      (setf sp (cdr sp)
            pc (if (eql (value-of x) '#t)
                   ct
                   cf)
            dump (cons (immediate-rep cont) dump)))))

;; JOIN    s e (JOIN.c) (cr.d)  ->  s e cr d
(def-insn JOIN (vm)
  (with-accessors ((dump dump-of) (pc set-pc)) vm
    (let ((cr (car dump)))
      (setf pc (value-of cr))
      (setf dump (cdr dump)))))

;;(defun locate (level j env)
;;  "Return i th variable in j level in environment ENV in runtime."
;;  (nth (- j 1) (nth (- level 1) env)))

(defgeneric locate (vm level n)
  (:documentation "auxiliary function"))

(defmethod locate ((vm vm) level n)
  (with-accessors ((env env-of)) vm
    (let ((*the-vm* vm))
      (let ((env* (loop :repeat level
                     :for e = env :then (cdr e)
                     :finally (return (car e)))))
        (loop :repeat n
           :for var = env* :then (cdr var)
           :finally (return (car var)))))))

;; LD      s e (LD (i.j).c) d   ->  (locate((i.j),e).s) e c d
(def-insn LD (vm)
  (with-accessors ((sp sp-of) (pc pc-of) (code get-code)) vm
    (let ((level (code-ref code pc))
          (n (code-ref code (1+ pc)))
          (oldpc (get-pc vm)))
      (setf sp (cons (locate vm level n) sp)
            pc (+ 2 oldpc)))))

;; LDF     s e (LDF f.c) d            ->  ((f.e).s) e c d
(def-insn LDF (vm)
  (with-accessors ((sp sp-of) (env get-env) (pc pc-of) (code get-code)) vm
    (let ((f (fetch-operand vm)) ;; PC
          (c (code-ref code (+ 1 pc))))
      (setf sp (cons (cons (immediate-rep f) env) sp)
            pc c))))

;; AP      ((f.e') v.s) e (AP.c) d    ->  NIL (v.e') f (s e c.d)
(def-insn AP (vm)
  (with-accessors ((env env-of) (pc pc-of) (dump dump-of) (sp sp-of) (code get-code)) vm
    (let* ((c pc) ;;
           (closure (car sp)) ;; (f.e')
           (fbody-pc (value-of (car closure))) ;; f
           (fenv (cdr closure)) ;; e'
           (v (car (cdr sp))) ;; v
           (s (cdr (cdr sp))) ;; s
           (oldenv env))
      (setf sp (cons empty empty)
            env (cons v fenv)
            pc fbody-pc
            dump (cons s (cons oldenv (cons (immediate-rep c) dump)))))))

;; RTN     (x.z) e' (RTN.q) (s e c.d) ->  (x.s) e c d
(def-insn RTN (vm)
  (with-accessors ((sp sp-of) (env set-env) (pc pc-of) (dump dump-of)) vm
    (let* ((x (car sp))
           (s (car dump))
           (e (car (cdr dump)))
           (c (car (cdr (cdr dump))))
           (d (cdr (cdr (cdr dump)))))
      (setf sp (cons x s)
            env e
            pc (value-of c)
            dump d))))

;; DUM
(def-insn DUM (vm)
  (with-accessors ((env env-of)) vm
    (setf env (cons (gensym) env))))

;; RAP     ((f.(W.e)) v.s) (W.e) (RAP.c) d  ->  nil rplaca((W.e),v) f (s e c.d)
(def-insn RAP (vm)
  (with-accessors ((sp sp-of) (env set-env) (pc pc-of) (dump dump-of)) vm
    ;; TODO
    ))

;; (def-insn RAP (vm)
;;   (with-accessors ((sp sp-of) (env set-env) (pc pc-of) (dump dump-of)) vm
;;     ;; todo consider how to recover closure from vm-stack
;;     (destructuring-bind ((f . WW) v . s) stack
;;       (let ((c pc))
;;         (setf (car WW) v) ;; make circular-list
;;         (setf stack :NIL)
;;         (setf env WW)
;;         (setf pc f)
;;         (setf dump (append (list s (cdr WW) c) dump))
;;         ))))


