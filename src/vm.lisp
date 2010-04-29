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
  ;;(defparameter *debug* t)
  (defparameter *debug* nil)
  (defparameter *profile* t) 
  )

;; tagged pointer

;(defparameter *memory-size* 64000)
(defparameter *memory-size* (* 50000 4)) ;; SCM_VM_STACK_SIZE in words = 10000
;(defparameter *memory-size* 64000)
;(defparameter *memory-size* 320)

(defun make-memory (size)
  "make memory array"
  (make-array size :element-type '(unsigned-byte 8)))

(defun address (mem idx)
  "low level accesser to the memory MEM"
  (aref mem idx))

(defsetf address (mem idx) (new)
  `(setf (aref ,mem ,idx) ,new))

;; (defconstant fxshift 2)

(defconstant bool-f #b0101111) ;; immediate rep
(defconstant bool-t #b1101111) ;; immediate rep
(defconstant empty #b00111111) ;; immediate rep

;;(defconstant bool-mask #b10111111)



(defconstant wordsize 4)

(defconstant tag-fixnum #b00)
(defconstant tag-pair #b001)
(defconstant tag-vector #b010)

(defun scheme-type-of (word)
  "return scheme type of word WORD or return nil if unknown."
  (cond
    ((eql (ldb (byte 2 0) word) tag-fixnum) :fixnum)
    (t
     (let ((tag3 (ldb (byte 3 0) word)))
       (cond
         ((eql tag3 tag-pair) :pair)
         ((eql tag3 tag-vector) :vector)
         ((eql (ldb (byte 8 0) word) empty) :empty)
         ((eql (ldb (byte 7 0) word) bool-f) :bool-f)
         ((eql (ldb (byte 7 0) word) bool-t) :bool-t))))))
       
;; Fnnnnn## 
;; immediate-value
;; 00000000 = 0
;; 00000100 = 1
;; 00001100 = 3
;; 11111100 = -1
;; 11110100 = -3

(defun as-32b (x &optional stream)
  (format stream "~32,'0,,b" x))

(defun convert-to-scheme-value (type val)
  "convert word to scheme value."
  (ecase type
    ((:fixnum)
     (if (logbitp 31 val) ;; minus value
         (loop :with z = (1+ (lognot val))
            :for i :from 2 :to 29
            :for x :from 0
            :sum (if (logbitp i z)
                     (expt 2 x)
                     0) :into tot
            :finally
            (return (- tot)))
         (let ((u 0))
           (setf (ldb (byte 29 0) u) (ldb (byte 29 2) val))
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
    (values (convert-to-scheme-value type val) type)))

(defun immediate-rep (x)
  "return immediate representaion of X."
  (cond
    ((and (typep x 'fixnum) (< x (expt 2 29)))
     (let ((u 0)) ;; little endian
       (setf (ldb (byte 2 0) u) #b00)
       (setf (ldb (byte 29 2) u) x)
       (when (< x 0) ;; minus
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
    :accessor code-of
    :writer (setf set-code)
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
;; CONS CELL
;; | CAR | 1 word
;; | CDR | 1 word

(defgeneric vm-cons (vm a b)
  (:documentation "make new cell on VM."))

(defun read-word (mem addr0)
  "read word on memory."
  (loop :with word = 0
     :for addr :from addr0 :below (+ addr0 wordsize)
     :for pos :from 0 :by 8
     :do (setf (ldb (byte 8 pos) word) (address mem addr))
     :finally (return word)))

(defun write-word (mem addr0 word)
  "write word on memory."
  (loop :for addr :from addr0 :below (+ addr0 wordsize)
     :for pos :from 0 :by 8
     :do (setf (address mem addr) (ldb (byte 8 pos) word))))

(defmethod vm-cons ((vm vm) a b)
  (with-accessors ((mem memory-of) (ap ap-of)) vm
    (let ((ap0 ap))
      (write-word mem ap a)
      (write-word mem (+ ap wordsize) b)
      (incf ap (* 2 wordsize))
      (setf (ldb (byte 3 0) ap0) tag-pair)
      ap0)))

(defgeneric vm-car (vm addr)
  (:documentation "car on VM."))

(defmethod vm-car ((vm vm) word)
  (with-accessors ((mem memory-of)) vm
    (let ((addr (scheme-value-of word)))
      (read-word mem addr))))

(defgeneric vm-cdr (vm addr)
  (:documentation "car on VM."))

(defmethod vm-cdr ((vm vm) word)
  (with-accessors ((mem memory-of)) vm
    (let ((addr (scheme-value-of word)))
      (read-word mem (+ addr wordsize)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-vm0 ()
  "Make vm instance and initialize."
  (let ((vm (make-instance 'vm)))
    (with-accessors ((mem memory-of) (ap ap-of) (sp set-sp) (env set-env) (dump set-dump)) vm
      (setf mem (make-memory *memory-size*))
      (setf (address mem 0) (immediate-rep ()))
      (incf ap 8)
      (setf sp 1
            env 1
            dump 1))
    vm))

(defun make-vm (code)
  "Make vm instance."
  (let ((vm (make-vm0)))
    (setf (code-of vm) code)
    vm))

(defmethod print-object ((vm vm) stream)
  (print-unreadable-object (vm stream)
    (with-accessors ((env get-env) (code get-code) (dump get-dump) (mem get-memory)
                     (sp sp-of) (ap ap-of)) vm
      (format stream "VM ap: ~a sp: ~a stack-top(raw): ~a(~a)" ap sp
              (scheme-value-of (vm-stack-top vm))
              (vm-stack-top vm)))))

(defmethod describe-object ((vm vm) stream)
  (with-accessors ((pc get-pc) (code get-code) (count get-execution-count)
                   (mem memory-of) (ap ap-of)
                   (profile get-profile)) vm
    (format stream "====Profile=:~% number of execution: ~a~%" count)
    (maphash (lambda (key val) (format stream "~8,,,@a: ~a~%" key val)) profile)
    (format stream "====Heap====:~%")
    (loop :for i :from ap :downto 0 :by wordsize
       :do (format stream "~8,'0,,x: ~4,,,@a ~%" i (read-word mem i)))))

;; VM method

(defgeneric dispatch (insn vm)
  (:documentation "Dispatch VM instruction."))

(defmethod dispatch (insn vm)
  (format t ";; base case: ~a~%" insn)
  (describe vm))

(defun code-ref (code idx)
  "refer code element of index IDX"
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
;; SECD operation
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

