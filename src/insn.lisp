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


(in-package :secd)

(defmacro def-insn (name (vm) &rest body)
  "define instuction."
  (let ((insn (gensym)))
    `(defmethod dispatch ((,insn (eql ,(as-keyword name))) (,vm vm))
       ,(if *debug*
            `(let ((*print-circle* t))
               (format t "; PC: ~a insn: ~a~%" (pc-of ,vm) ,insn)
               (describe ,vm)
               ;; (graphviz-vm ,vm (format nil "tmp.vm.~d.dot" (execution-count-of ,vm)))
               ))
       ,(if *profile*
            `(progn
               (incf (execution-count-of ,vm))
               (incf (gethash ,(as-keyword name) (profile-of ,vm) 0))))
       ,@body)))

;; NIL     s e (NIL.c) d        ->  (nil.s) e c d
;; (def-insn NIL (vm)
;;   (s (cons () s))
;;   (e e)
;;   (c c)
;;   (d d))

(def-insn NIL (vm)
  (with-accessors ((sp sp-of)) vm
    (setf sp (vm-cons vm empty sp)) ;; cons
    (next vm)))

;; STOP
(def-insn STOP (vm)
  vm)

;; LDC
(def-insn LDC (vm)
  (with-accessors ((pc pc-of) (sp sp-of)) vm
    (let ((c (fetch-operand vm)))
      ;(assert (integerp c))
      (setf sp (vm-cons vm (immediate-rep c) sp))
      (incf pc)
      (next vm))))

(defmacro def-binary-insn (name sym)
  (let ((a (gensym))
        (b (gensym))
        (sp (gensym))
        (vm (gensym)))
    `(progn
       (def-insn ,name (,vm)
         (with-accessors ((,sp sp-of)) ,vm
           (let* ((,a (vm-stack-pop ,vm))
                  (,b (vm-stack-pop ,vm)))
             ;; todo; not fast
             (setf ,sp
                   (vm-cons ,vm
                            (immediate-rep
                             (,sym (scheme-value-of ,a) (scheme-value-of ,b))) ,sp)))
           (next ,vm))))))

;; x = 4a; y = 4b; (x + y) = (4a + 4b) = 4(a + b)
(def-insn + (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (vm-stack-pop vm))
           (b (vm-stack-pop vm))
           (res (+ a b)))
      (setf sp (vm-cons vm res sp))
      (next vm))))

;; x = 4a; y = 4b; (x - y) = (4a - 4b) = 4(a - b)
(def-insn - (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (vm-stack-pop vm))
           (b (vm-stack-pop vm))
           (res (- a b)))
      (setf sp (vm-cons vm res sp))
      (next vm))))

(def-binary-insn * cl:*)

;; x == y then 4x == 4y
(def-insn = (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (vm-stack-pop vm))
           (b (vm-stack-pop vm))
           (res (= a b)))
      (setf sp (vm-cons vm (if res
                               bool-t
                               bool-f)
                        sp))
      (next vm))))

(def-insn > (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (vm-stack-pop vm))
           (b (vm-stack-pop vm))
           (res (> (scheme-value-of a) (scheme-value-of b))))
      (setf sp (vm-cons vm (if res
                               bool-t
                               bool-f)
                        sp))
      (next vm))))

(def-insn < (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (vm-stack-pop vm))
           (b (vm-stack-pop vm))
           (res (< (scheme-value-of a) (scheme-value-of b))))
      (setf sp (vm-cons vm (if res
                               bool-t
                               bool-f)
                        sp))
      (next vm))))

;; CONS
(def-insn CONS (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (vm-stack-pop vm))
           (b (vm-stack-pop vm)))
      (setf sp (vm-cons vm (vm-cons vm a b) sp))
      (next vm))))

;; SEL CT CF CONT
(def-insn SEL (vm)
  (with-accessors ((pc pc-of) (code get-code) (dump dump-of)) vm
    (let* ((x (vm-stack-pop vm))
           (ct (code-ref code pc))
           (cf (code-ref code (1+ pc)))
           (cont (code-ref code (+ 2 pc))))
      (setf pc (if (eql (scheme-value-of x) '#t)
                   ct
                   cf)
            dump (vm-cons vm (immediate-rep cont) dump))
      (next vm))))

;; JOIN
(def-insn JOIN (vm)
  (with-accessors ((pc set-pc)) vm
    (let ((cr (vm-dump-pop vm)))
      (setf pc (scheme-value-of cr))
      (next vm))))

;;(defun locate (level j env)
;;  "Return i th variable in j level in environment ENV in runtime."
;;  (nth (- j 1) (nth (- level 1) env)))

(defgeneric locate (vm level n)
  (:documentation "auxiliary function"))

(defmethod locate ((vm vm) level n)
  (with-accessors ((env env-of)) vm
    (let ((env* (loop :repeat level
                   :for e = env :then (vm-cdr vm e)
                   :finally (return (vm-car vm e)))))
      (loop :repeat n
         :for var = env* :then (vm-cdr vm var)
         :finally (return (vm-car vm var))))))

;; LD      s e (LD (i.j).c) d   ->  (locate((i.j),e).s) e c d
(def-insn LD (vm)
  (with-accessors ((sp sp-of) (pc pc-of) (code get-code)) vm
    (let ((level (code-ref code pc))
          (n (code-ref code (1+ pc)))
          (oldpc (get-pc vm)))
      (setf sp (vm-cons vm (locate vm level n) sp)
            pc (+ 2 oldpc))
      (next vm))))

;; LDF     s e (LDF f.c) d            ->  ((f.e).s) e c d
(def-insn LDF (vm)
  (with-accessors ((sp sp-of) (env get-env) (pc pc-of) (code get-code)) vm
    (let ((f (fetch-operand vm)) ;; PC
          (c (code-ref code (+ 1 pc))))
      (setf sp (vm-cons vm (vm-cons vm (immediate-rep f) env) sp)
            pc c)
      (next vm))))

;; AP      ((f.e') v.s) e (AP.c) d    ->  NIL (v.e') f (s e c.d)
(def-insn AP (vm)
  (with-accessors ((env env-of) (pc pc-of) (dump dump-of) (sp sp-of) (code get-code)) vm
    (let* ((c pc) ;;
           (closure (vm-car vm sp)) ;; (f.e')
           (fbody-pc (scheme-value-of (vm-car vm closure))) ;; f
           (fenv (vm-cdr vm closure)) ;; e'
           (v (vm-car vm (vm-cdr vm sp))) ;; v
           (s (vm-cdr vm (vm-cdr vm sp))) ;; s
           (oldenv env))
      (setf sp (vm-cons vm empty empty)
            env (vm-cons vm v fenv)
            pc fbody-pc
            dump (vm-cons vm s (vm-cons vm oldenv (vm-cons vm (immediate-rep c) dump))))
      (next vm))))

;; RTN     (x.z) e' (RTN.q) (s e c.d) ->  (x.s) e c d
(def-insn RTN (vm)
  (with-accessors ((sp sp-of) (env set-env) (pc pc-of) (dump dump-of)) vm
    (let* ((x (vm-stack-pop vm))
           (s (vm-car vm dump))
           (e (vm-car vm (vm-cdr vm dump)))
           (c (vm-car vm (vm-cdr vm (vm-cdr vm dump))))
           (d (vm-cdr vm (vm-cdr vm (vm-cdr vm dump)))))
      (setf sp (vm-cons vm x s)
            env e
            pc (scheme-value-of c)
            dump d)
      (next vm))))

;; DUM
(def-insn DUM (vm)
  (with-accessors ((env env-of)) vm
    (setf env (vm-cons vm (gensym) env))
    (next vm)))

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
;;         (next vm)))))


