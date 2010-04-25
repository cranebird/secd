;; -*- coding:utf-8 -*-
;; vm.lisp - The SECD Machine in Common Lisp
;; Copyright (C) 2010 by cranebird
;; my blog: http://d.hatena.ne.jp/cranebird/ (in Japanese)
;; github:
;; Based on the lecture note by Prof. Jia-Huai You
;; (http://www.cs.ualberta.ca/~you/courses/325/Mynotes/Fun/SECD-slides.html)
;; And LispMe.

(in-package :secd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-scm-macro-character)
  ;(defparameter *debug* t)
  (defparameter *debug* nil)
  )

;; tagged pointer

;(defparameter *memory-size* 64000)
(defparameter *memory-size* (* 10000000 4)) ;; SCM_VM_STACK_SIZE in words = 10000
;(defparameter *memory-size* 64000)
;(defparameter *memory-size* 320)

(defun make-memory (size)
  "make memory array"
  (make-array size :element-type '(unsigned-byte 8)))

(defun address (mem idx)
  (aref mem idx))

(defsetf address (mem idx) (new)
  `(setf (aref ,mem ,idx) ,new))

(defconstant fxshift 2)
(defconstant bool-f #b0101111)
(defconstant bool-t #b1101111)
(defconstant bool-mask #b10111111)
(defconstant empty #b00111111)
(defconstant wordsize 4)

(defconstant tag-fixnum #b00)
(defconstant tag-pair #b001)
(defconstant tag-vector #b010)

(defun scheme-type-of (val)
  "return scheme type of value VAL or return nil if unknown."
  (if (eql (ldb (byte 2 0) val) tag-fixnum)
      :fixnum
      (let ((tag3 (ldb (byte 3 0) val)))
        (cond
          ((eql tag3 tag-pair)
           :pair)
          ((eql tag3 tag-vector)
           :vector)
          ((eql (ldb (byte 8 0) val) empty)
           :empty)
          ((eql (ldb (byte 7 0) val) bool-f)
           :bool-f)
          ((eql (ldb (byte 7 0) val) bool-t)
           :bool-t)))))

;; Fnnnnn## 
;; immediate-value
;; 00000000 = 0
;; 00000100 = 1
;; 00001100 = 3
;; 11111100 = -1
;; 11110100 = -3
;; (format t "~32,'0,,b" (immediate-rep 3))

(defun as-32b (x &optional stream)
  (format stream "~32,'0,,b" x))

(defun convert-to-scheme-value (type val)
  (ecase type
    ((:fixnum)
     (let ((u 0))
       ;; ;(setf (ldb (byte 31 0) u) (ldb (byte 31 2) val))
       ;; ;(ldb (byte 32 0) val)
       ;; (setf (ldb (byte 31 0) u) (ldb (byte 31 2) val))
       ;; (format t ";; (ldb (byte 1 31) val) :~a~%" (ldb (byte 1 31) val))
       ;; (setf (ldb (byte 1 31) u) (ldb (byte 1 31) val))
       ;; (ldb (byte 32 0) u)
       
       (setf (ldb (byte 29 0) u) (ldb (byte 29 2) val))
       (if (logbitp 31 val)
           (let ((z (+ (lognot val) 1)))
             (- (loop :for i :from 2 :to 29 :for x :from 0
                   :sum (if (logbitp i z)
                           (expt 2 x)
                           0))))
           u)))
    ((:bool-f) #f)
    ((:bool-t) #t)
    ((:empty) ())
    ((:pair :vector)
     (let ((v val))
       (setf (ldb (byte 3 0) v) #b000)
       v))))

(defun scheme-value-of (val)
  "return value of scheme object OBJ and type. val is tagged value
Error if invalid type."
  (let ((type (scheme-type-of val)))
    (values
     (convert-to-scheme-value type val)
     type)))

(defun immediate-rep (x)
  "return immediate representaion of X."
  (cond
    ((and (typep x 'fixnum) (< x (expt 2 29)))
     (let ((u 0)) ;; little endian?? ;; TODO
       (setf (ldb (byte 2 0) u) #b00)
       (setf (ldb (byte 29 2) u) x)
       (if (< x 0)
           (setf (ldb (byte 1 31) u) 1))
       u))
    ((null x) empty)
    ((eql x '#t) bool-t)
    ((eql x '#f) bool-f)
    (t (error "Unknown immediate-rep: ~a" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass vm ()
  ((memory
    :accessor memory-of
    :reader get-memory
    :writer (setf set-memory)
    :initform nil
    :initarg :memory)
   (ap
    :accessor ap-of
    :reader get-ap
    :writer (setf set-ap)
    :initform 0
    :type fixnum
    :documentation "allocation pointer")
   (sp
    :accessor sp-of
    :reader get-sp
    :writer (setf set-sp)
    :initform 0
    :type fixnum
    :documentation "stack pointer")
   (env
    :accessor env-of
    :reader get-env
    :writer (setf set-env)
    :initform 0
    :type fixnum
    :initarg :env
    :documentation "env pointer")
   (pc
    :accessor pc-of
    :reader get-pc
    :writer (setf set-pc)
    :initform 0
    :type fixnum
    :documentation "Program Pointer")
   (code
    :reader get-code
    :initform nil
    :initarg :code
    :documentation "code vector")
   (dump
    :accessor dump-of
    :reader get-dump
    :writer (setf set-dump)
    :initform 0
    :type fixnum
    :documentation "dump stack")
   (execution-count
    :accessor execution-count-of
    :reader get-execution-count
    :writer (setf set-execution-count)
    :initform 0
    :type fixnum
    :documentation "instruction execution count")
   (profile
    :accessor profile-of
    :reader get-profile
    :writer (setf set-profile)
    :initform (make-hash-table)
    :documentation "instruction => executed count hash-table"))
  (:documentation "The scheme virtual machine class"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cons cell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric vm-cons (vm a b)
  (:documentation "make new cell on VM."))

;; CONS CELL
;; | CAR | 1 word
;; | CDR | 1 word
(defmethod vm-cons ((vm vm) a b)
  (with-accessors ((mem memory-of) (ap ap-of)) vm
    (let ((ap0 ap))
      (setf (address mem ap) (ldb (byte 8 0) a))
      (setf (address mem (+ 1 ap)) (ldb (byte 8 8) a))
      (setf (address mem (+ 2 ap)) (ldb (byte 8 16) a))
      (setf (address mem (+ 3 ap)) (ldb (byte 8 24) a))
      (setf (address mem (+ 4 ap)) (ldb (byte 8 0) b))
      (setf (address mem (+ 5 ap)) (ldb (byte 8 8) b))
      (setf (address mem (+ 6 ap)) (ldb (byte 8 16) b))
      (setf (address mem (+ 7 ap)) (ldb (byte 8 24) b))
      (incf ap 8)
      (setf (ldb (byte 3 0) ap0) tag-pair)
      ap0)))

(defgeneric vm-car (vm addr)
  (:documentation "car on VM."))

(defmethod vm-car ((vm vm) val)
  (with-accessors ((m memory-of)) vm
    (let ((v 0)
          (sv (scheme-value-of val)))
      (setf (ldb (byte 8 0) v) (address m (+ 0 sv)))
      (setf (ldb (byte 8 8) v) (address m (+ 1 sv)))
      (setf (ldb (byte 8 16) v) (address m (+ 2 sv)))
      (setf (ldb (byte 8 24) v) (address m (+ 3 sv)))
      v)))

(defgeneric vm-cdr (vm addr)
  (:documentation "car on VM."))

(defmethod vm-cdr ((vm vm) val)
  (with-accessors ((m memory-of)) vm
    (let ((v 0)
          (sv (scheme-value-of val)))
      (setf (ldb (byte 8 0) v) (address m (+ 4 sv)))
      (setf (ldb (byte 8 8) v) (address m (+ 5 sv)))
      (setf (ldb (byte 8 16) v) (address m (+ 6 sv)))
      (setf (ldb (byte 8 24) v) (address m (+ 7 sv)))
      v)))

(defvar *the-vm* nil)
(defmacro with-vm ((vm) &body body)
  `(let ((*the-vm* ,vm))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-vm (code)
  "Make vm instance."
  (let ((vm (make-instance 'vm :code code)))
    (with-accessors ((mem memory-of) (ap ap-of) (sp set-sp) (env set-env) (dump set-dump)) vm
      (setf mem (make-memory *memory-size*))
      (setf (address mem 0) (immediate-rep ()))
      (incf ap 8)
      (setf sp 1
            env 1
            dump 1))
    vm))

(defmethod print-object ((vm vm) stream)
  (print-unreadable-object (vm stream)
    (with-accessors ((env get-env) (code get-code) (dump get-dump) (mem get-memory)
                     (sp sp-of) (ap ap-of)) vm
      (format stream "VM ap: ~a sp: ~a stack-top(value): ~a(~a)" ap sp
              (vm-stack-top vm)
              (scheme-value-of (vm-stack-top vm))))))

(defmethod describe-object ((vm vm) stream)
  (with-accessors ( ;;(stack get-stack)
                   (pc get-pc) (code get-code) (count get-execution-count)
                   (profile get-profile)) vm
    (format stream "Profile: number of execution: ~a~%" count)
    (maphash (lambda (key val)
               (format stream "~a: ~a~%" key val)) profile)
    ;; (format stream "VM ap: ~a~%" (ap-of vm))
    ;; (format stream "VM sp: ~a~%" (sp-of vm))
    ;; (format stream "VM stack top: ~a~%" (scheme-value-of (vm-stack-top vm)))
     (with-accessors ((mem memory-of) (ap ap-of)) vm
       (loop :for i :from ap :downto 0 :by wordsize
          :do
            (format stream "~x : ~a ~%" i (load-word mem i))))
    ;;(format stream "todo desc~%")
    ))

;; (defmethod dump ((vm vm))
;;   (with-accessors ((mem memory-of) (ap ap-of)) vm
;;     (format t "stack -> ~8,'0,,x;~%" (scheme-value-of (sp-of vm)))
;;     (format t "env -> ~8,'0,,x;~%" (scheme-value-of (env-of vm)))
;;     (format t "dump -> ~8,'0,,x;~%" (scheme-value-of (dump-of vm)))

;;     (loop :for addr :from ap :downto 0 :by wordsize
;;        :do
;;        (let ((word (load-word mem addr)))
;;          (if (scheme-type-of word) 
;;              (multiple-value-bind (val type) (scheme-value-of word)
;;                (format t "~8,'0,,x| ~a| ~a~%" addr type val))
;;              (format t "~8,'0,,x| ~a| ~a~%" addr "unknown" "unknown")
;;              )))))

(defmethod graphviz-object ((vm vm) stream)
  (format stream "digraph structs {~%")
  (format stream "rankdir=LR~%")
  (format stream "node [shape=record];~%")
  (format stream "mem [label=\"");
  ;; memory box
  (with-accessors ((mem memory-of) (ap ap-of)) vm
    (loop :for addr :from ap :downto 0 :by wordsize
       :do
       (multiple-value-bind (val type) (scheme-value-of (load-word mem addr))
         (format stream "{<p~8,'0,,x> ~8,'0,,x| ~a| ~a}" addr addr type val)
         (unless (= addr 0)
           (format stream " | ")))))
  (format stream "\"];~%")
  ;; link
  (with-accessors ((mem memory-of) (ap ap-of)) vm
    (loop :for addr :from ap :downto 0 :by wordsize
       :do
       (multiple-value-bind (val type) (scheme-value-of (load-word mem addr))
         (when (eql type :pair)
           (format stream "mem:p~8,'0,,x:w -> mem:p~8,'0,,x:w~%"
                   addr val)))))
  ;; SECD pointer
  (format stream "stack -> mem:p~8,'0,,x:w;~%" (scheme-value-of (sp-of vm)))
  (format stream "env -> mem:p~8,'0,,x:w;~%" (scheme-value-of (env-of vm)))
  (format stream "dump -> mem:p~8,'0,,x:w;~%" (scheme-value-of (dump-of vm)))
  (format stream "ap -> mem:p~8,'0,,x:w;~%" (ap-of vm))
  (format stream "}~%")
  )

(defun graphviz-vm (vm)
  (with-output-to-string (o)
    (graphviz-object vm o)
    o))

(defgeneric dispatch (insn vm)
  (:documentation "Dispatch VM instruction."))

(defmethod dispatch (insn vm)
  (format t ";base case: ~a~%" insn)
  (describe vm))

(defun code-ref (code idx)
  "refer code element of index IDX"
  ;(aref code idx)
  (svref code idx))

(defgeneric fetch-insn (vm)
  (:documentation "fetch instruction and increment PC of vm."))

(defmethod fetch-insn ((vm vm))
  (with-accessors ((pc pc-of) (code get-code)) vm
    (let ((c (code-ref code pc)))
      (incf pc)
      c)))

(defgeneric fetch-operand (vm)
  (:documentation "Fetch operand."))

(defmethod fetch-operand ((vm vm))
  (with-accessors ((pc get-pc) (code get-code)) vm
    (code-ref code pc)))

(defgeneric next (vm)
  (:documentation "Fetch instruction and dispatch."))

(defmethod next ((vm vm))
  (dispatch (fetch-insn vm) vm))

;; (defmethod next ((vm vm))
;;   (let ((c (fetch-insn vm)))
;;     (if c
;;         (dispatch c vm)
;;         (format t ";; end of code? ~a~%" vm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro vm-stack-pop (vm)
  (let ((top (gensym))
        (sp (gensym)))
    `(with-accessors ((,sp sp-of)) ,vm
       (let ((,top (vm-car ,vm ,sp)))
         (setf ,sp (vm-cdr ,vm ,sp))
         ,top))))

(defgeneric vm-stack-top (vm)
  (:documentation "car of stack."))

(defmethod vm-stack-top ((vm vm))
  (with-accessors ((sp sp-of)) vm
    (vm-car vm sp)))

(defmacro vm-dump-pop (vm)
  (let ((top (gensym))
        (dump (gensym)))
    `(with-accessors ((,dump dump-of)) ,vm
       (let ((,top (vm-car ,vm ,dump)))
         (setf ,dump (vm-cdr ,vm ,dump))
         ,top))))

(defun load-word (m p)
  (let ((v 0)) ;; little endian
    (setf (ldb (byte 8 0) v) (address m (+ 0 p)))
    (setf (ldb (byte 8 8) v) (address m (+ 1 p)))
    (setf (ldb (byte 8 16) v) (address m (+ 2 p)))
    (setf (ldb (byte 8 24) v) (address m (+ 3 p)))
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-insn (name (vm) &rest body)
  "define instuction."
  (let ((insn (gensym)))
    `(defmethod dispatch ((,insn (eql ,(as-keyword name))) (,vm vm))
       ,(if *debug*
            `(let ((*print-circle* t))
               (format t "; insn: ~a~%" ,insn)
               ;;(format t "; Memory: ~a Stack: ~a~%" (memory-of ,vm) (sp-of ,vm))
               (format t "; PC: ~a~%" (pc-of ,vm)))
            ;`(declare (ignore ,insn))
            )
       ,(if *debug*
            `(progn
               (incf (execution-count-of ,vm))
               (incf (gethash ,(as-keyword name) (profile-of ,vm) 0))))
       ,@body)))

;; NIL
(def-insn NIL (vm)
  (with-accessors ((sp sp-of)) vm
    (setf sp (vm-cons vm (immediate-rep ()) sp))) ;; cons
  (next vm))

;; STOP
(def-insn STOP (vm)
  vm)

;; LDC
(def-insn LDC (vm)
  (with-accessors ((pc pc-of) (sp sp-of)) vm
    (let ((c (fetch-operand vm)))
      ;(assert (integerp c))
      (setf sp (vm-cons vm (immediate-rep c) sp)) ;; cons
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
                               (immediate-rep '#t)
                               (immediate-rep '#f))
                        sp))
      (next vm))))

(def-insn > (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (vm-stack-pop vm))
           (b (vm-stack-pop vm))
           (res (> (scheme-value-of a) (scheme-value-of b))))
      (setf sp (vm-cons vm (if res
                               (immediate-rep '#t)
                               (immediate-rep '#f))
                        sp))
      (next vm))))

(def-insn < (vm)
  (with-accessors ((sp sp-of)) vm
    (let* ((a (vm-stack-pop vm))
           (b (vm-stack-pop vm))
           (res (< (scheme-value-of a) (scheme-value-of b))))
      (setf sp (vm-cons vm (if res
                               (immediate-rep '#t)
                               (immediate-rep '#f))
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
                   cf))
      (setf dump (vm-cons vm (immediate-rep cont) dump))
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

(defmethod locate ((vm vm) level n)
  (with-accessors ((env env-of)) vm
    (let ((env* (loop :repeat level
                   :for e = env :then (vm-cdr vm e)
                   :finally (return (vm-car vm e)))))
      (loop :repeat n
         :for var = env* :then (vm-cdr vm var)
         :finally (return (vm-car vm var))))))

;; LD
(def-insn LD (vm)
  (with-accessors ((sp sp-of) (env get-env) (pc pc-of) (code get-code)) vm
    (let ((level (code-ref code pc))
          (n (code-ref code (1+ pc)))
          (oldpc (get-pc vm)))
      (setf sp (vm-cons vm (locate vm level n) sp))
      (setf pc (+ 2 oldpc))
      (next vm))))

;; LDF     s e (LDF f.c) d            ->  ((f.e).s) e c d
(def-insn LDF (vm)
  (with-accessors ((sp sp-of) (e get-env) (pc pc-of) (code get-code)) vm
    (let ((f (fetch-operand vm)) ;; PC
          (c (code-ref code (+ 1 pc))))
      (setf sp (vm-cons vm (vm-cons vm (immediate-rep f) e) sp))
      (setf pc c)
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
           (env-old env))
      (setf sp (vm-cons vm (immediate-rep ()) (immediate-rep ()))) ;; () be care
      (setf env (vm-cons vm v fenv))
      (setf pc fbody-pc)
      ;;(setf dump (vm-cons vm s (vm-cons vm env-old (vm-cons vm c dump))))
      (setf dump (vm-cons vm s (vm-cons vm env-old (vm-cons vm (immediate-rep c) dump))))
      (next vm))))

;; RTN     (x.z) e' (RTN.q) (s e c.d) ->  (x.s) e c d
(def-insn RTN (vm)
  (with-accessors ((stack sp-of) (env set-env) (pc pc-of) (dump dump-of)) vm
    (let* ((x (vm-stack-pop vm))
           (s (vm-car vm dump))
           (e (vm-car vm (vm-cdr vm dump)))
           (c (vm-car vm (vm-cdr vm (vm-cdr vm dump))))
           (d (vm-cdr vm (vm-cdr vm (vm-cdr vm dump)))))
      (setf stack (vm-cons vm x s))
      (setf env e)
      ;;(setf pc c)
      (setf pc (scheme-value-of c))
      (setf dump d)
      (next vm))))

;; DUM
(def-insn DUM (vm)
  (with-accessors ((env env-of)) vm
    (setf env (vm-cons vm (gensym) env))
    (next vm)))

;; RAP     ((f.(W.e)) v.s) (W.e) (RAP.c) d  ->  nil rplaca((W.e),v) f (s e c.d)
(def-insn RAP (vm)
  (with-accessors ((sp sp-of) (env set-env) (pc pc-of) (dump dump-of)) vm
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

