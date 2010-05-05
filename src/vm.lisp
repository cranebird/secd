;; -*- coding:utf-8 -*-
;; vm.lisp - The SECD Machine in Common Lisp
;; Copyright (C) 2010 by cranebird
;; my blog: http://d.hatena.ne.jp/cranebird/ (in Japanese)
;; github:
;; Based on the lecture note by Prof. Jia-Huai You
;; (http://www.cs.ualberta.ca/~you/courses/325/Mynotes/Fun/SECD-slides.html)
;; And LispMe.

(in-package :secd.vm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-scm-macro-character)
  ;;(defparameter *debug* t)
  (defparameter *debug* nil)
  (defparameter *profile* nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory and tagged pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *memory-size* 10000) ;; SCM_VM_STACK_SIZE in words = 10000

(defun make-memory (size)
  "make memory array"
  (make-array size :element-type '(signed-byte 32)))

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

;; CL value === immediate-rep ==> Scheme value
;; CL value <== convert-scheme-obj == Scheme value

(define-condition unknown-immediate-rep-error (type-error)
  ((text :initarg :text :reader text)))

(defun convert-scheme-obj (type obj)
  "convert scheme object OBJ to Common Lisp value."
  (ecase type
    ((:fixnum)
     (if (logbitp 31 obj) ;; minus value
         (loop :with z = (1+ (lognot obj))
            :for i :from 2 :to 29
            :for x :from 0
            :sum (if (logbitp i z)
                     (expt 2 x)
                     0) :into tot
            :finally
            (return (- tot)))
         (let ((u 0))
           (setf (ldb (byte 29 0) u) (ldb (byte 29 2) obj))
           u)))
    ((:bool-f) #f)
    ((:bool-t) #t)
    ((:empty) ())
    ((:pair :vector)
     (let ((v obj))
       (setf (ldb (byte 3 0) v) #b000)
       v))))

(defun value-of (obj)
  "return value of scheme object OBJ and type. OBJ is tagged.
Error if invalid type."
  (let ((type (scheme-type-of obj)))
    (values (convert-scheme-obj type obj) type)))

(defun immediate-rep (x)
  "return immediate representaion of X."
  (cond
    ((and (typep x 'fixnum) (< x (expt 2 29)))
     (let ((u 0)) ;; little endian
       (setf (ldb (byte 2 0) u) tag-fixnum)
       (setf (ldb (byte 29 2) u) x)
       (when (< x 0) ;; minus
         (setf (ldb (byte 1 31) u) 1))
       u))
    ((null x) empty)
    ((eql x '#t) bool-t)
    ((eql x '#f) bool-f)
    (t (error 'unknown-immediate-rep-error :text x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass vm ()
  (
   ;; (memory
   ;;  :accessor memory-of
   ;;  :reader get-memory
   ;;  :writer (setf set-memory)
   ;;  :initform (make-memory *memory-size*)
   ;;  :initarg :memory)
   (memory-a
    :accessor memory-a
    :initform (make-memory *memory-size*)
    :initarg :memory)
   (memory-b
    :accessor memory-b
    :initform (make-memory *memory-size*))
   (from-space
    :accessor from-space-of
    :initform :a)
   (to-space
    :accessor to-space-of
    :initform :b)
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

(defmethod memory-of ((vm vm))
  (if (eql :a (from-space-of vm))
      (memory-a vm)
      (memory-b vm)))

(defmethod get-memory ((vm vm)) ;; from-space
  (if (eql :a (from-space-of vm))
      (memory-a vm)
      (memory-b vm)))

(defmethod from-memory ((vm vm))
  (if (eql :a (from-space-of vm))
      (memory-a vm)
      (memory-b vm)))

(defmethod to-memory ((vm vm))
  (if (eql :a (from-space-of vm))
      (memory-b vm)
      (memory-a vm)))

(defmethod swap-space ((vm vm))
  (format t "*************swap!~%")
  (rotatef (from-space-of vm)
           (to-space-of vm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cons cell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONS CELL
;; | CAR | 1 word
;; | CDR | 1 word

(defgeneric read-word (mem addr0)
  (:documentation "read word on memory."))

;; (defmethod read-word (mem addr0)
;;   (loop :with word = 0
;;      :for addr :from addr0 :below (+ addr0 wordsize)
;;      :for pos :from 0 :by 8
;;      :do (setf (ldb (byte 8 pos) word) (address mem addr))
;;      :finally (return word)))

(defmethod read-word (mem addr0)
  (address mem addr0))

(defgeneric write-word (mem addr0 word)
  (:documentation "write word on memory."))

;; (defmethod write-word (mem addr0 word)
;;  (loop :for addr :from addr0 :below (+ addr0 wordsize)
;;     :for pos :from 0 :by 8
;;     :do (setf (address mem addr) (ldb (byte 8 pos) word))))

(defmethod write-word (mem addr0 word)
  (setf (address mem addr0) word))

(defgeneric vm-cons (vm a b)
  (:documentation "make new cell on VM."))

(defmethod copying ((vm vm))
  (write-word (to-memory vm) 0 empty)
  (write-word (to-memory vm) (+ 0 wordsize) 0)
  (let ((to-ap (* 2 wordsize)))
    ;; NOT WORK TODO; 20100503
    (let ((newsp to-ap))
      (setq to-ap (copying-iter vm (value-of (sp-of vm)) to-ap)) ;; copy car
      (setq to-ap (copying-iter vm (+ wordsize (value-of (sp-of vm))) to-ap)) ;; copy cdr
      (setf (sp-of vm) (add-tag-pair newsp)))
    
    (let ((newenv to-ap))
      (setq to-ap (copying-iter vm (value-of (env-of vm)) to-ap))
      (setq to-ap (copying-iter vm (+ wordsize (value-of (env-of vm))) to-ap))
      (setf (env-of vm) (add-tag-pair newenv)))

    (let ((newdump to-ap))
      (setq to-ap (copying-iter vm (value-of (dump-of vm)) to-ap))
      (setq to-ap (copying-iter vm (+ wordsize (value-of (dump-of vm))) to-ap))
      (setf (dump-of vm) (add-tag-pair newdump)))
    to-ap
    )
  )

(defmethod copying-iter ((vm vm) addr to-ap0)
  (let* ((to-ap to-ap0)
         (val (read-word (from-memory vm) addr)))
    (write-word (to-memory vm) to-ap val)
    (incf to-ap wordsize)

    (when (eql (scheme-type-of val) :pair)
      ;(format t ";; recursive copying addr=~a:~%" addr)
      (setq to-ap (copying-iter vm (value-of val) to-ap))
      (setq to-ap (copying-iter vm (+ wordsize (value-of val)) to-ap)))

    to-ap))

(defun add-tag-pair (x)
  (let ((y x))
    (setf (ldb (byte 3 0) y) tag-pair)
    y))

(define-condition gc-condition (condition)
  ())

(define-condition allocation-fail-error (error)
  ())

(defmethod vm-cons ((vm vm) a b)
  (with-accessors ((ap ap-of)) vm
    (let ((ap0 ap))
      (let ((len (length (memory-of vm))))
        (when (>= (+ ap (* 2 wordsize)) len)
          (signal 'gc-condition) ;; cause gc
          (format t ";; after gc condition signaled~%")
          (when (>= (+ ap (* 2 wordsize)) len)
            (error 'allocation-fail-error))))
      (write-word (memory-of vm) ap a)
      (write-word (memory-of vm) (+ ap wordsize) b)
      (incf ap (* 2 wordsize))

      (setf (ldb (byte 3 0) ap0) tag-pair)
      ap0)))

(defgeneric vm-car (vm addr)
  (:documentation "car on VM."))

(defmethod vm-car ((vm vm) word)
  (let ((addr (value-of word)))
    (read-word (memory-of vm) addr)))

(defgeneric vm-cdr (vm addr)
  (:documentation "car on VM."))

(defmethod vm-cdr ((vm vm) word)
  (let ((addr (value-of word)))
    (read-word (memory-of vm) (+ addr wordsize))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-vm0 ()
  "Make vm instance and initialize."
  (let ((vm (make-instance 'vm)))
    (with-accessors ((sp set-sp) (env set-env) (dump set-dump)) vm
      (setf sp empty ;; TODO pointer to empty ; (let ((x 0)) (setf (ldb (byte 3 0) x) tag-pair) x) == 1
            env empty
            dump empty))
    vm))

(defun make-vm (code)
  "Make vm instance."
  (let ((vm (make-vm0)))
    (setf (code-of vm) code)
    vm))

(defmethod print-object ((vm vm) stream)
  (print-unreadable-object (vm stream)
    (with-accessors ((env get-env) (code get-code) (dump get-dump)
                     (sp get-sp) (ap get-ap)) vm
      (format stream "VM ap: ~a sp: ~x(~x) " ap sp (value-of sp))
      ;; (format stream "VM ap: ~a sp: ~x(~x) stack-top(raw): ~a(~a)" ap sp (value-of sp)
      ;;         (value-of (vm-car vm sp))
      ;;         (vm-car vm sp))
      )))

(defun display-memory (vm mem addr &optional (stream t))
  (let ((arrow (loop :for p :in (list (sp-of vm)
                                      (env-of vm)
                                      (dump-of vm))
                  :for s :in '("sp" "env" "dump")
                  if (eql addr (value-of p))
                  collect s)))
    (let ((word (read-word mem addr)))
      (format stream "~32,'0,,b:(~4,,,a) ~8,,,@a | ~a ~a~%"
              addr addr word (value-of word)
              (if arrow
                  (format nil "<-~{~a~^ ~}" arrow)
                  "")))))

(defmethod describe-from-space ((vm vm) &optional (stream t))
  (format stream "====From Space(~a)====:~%" (from-space-of vm))
  (loop :for addr :from 0 :to (+ (get-ap vm) wordsize) :by wordsize
       :do (display-memory vm (from-memory vm) addr stream)))

(defmethod describe-to-space ((vm vm) &optional (stream t))
  (format stream "====To Space(~a)====:~%" (to-space-of vm))
  ;; (format stream "~%~a~%" (to-memory vm))
  (loop :for addr :from 0 :below (length (to-memory vm)) :by wordsize
       :do (display-memory vm (to-memory vm) addr stream)))

(defmethod describe-object ((vm vm) stream)
  (with-accessors ((pc get-pc) (code get-code) (count get-execution-count)
                   (ap get-ap) (sp get-sp) (profile get-profile)) vm
    (format stream "====Profile=:~% number of execution: ~a~%" count)
    (maphash (lambda (key val) (format stream "~8,,,@a: ~a~%" key val)) profile)
    (describe-from-space vm stream)
    ;;(describe-to-space vm stream)
    ;; S
    (format stream "====Stack===:~%")
    ;; print LIST TODO
    ))

;; (defun display-list (addr top-p &optional stream)
;;   (when top-p
;;     (format stream "~a" #\())
;;   (display-obj (vm-

;; (define (display-pair)
;;   '(code (obj top) ()
;;      (if top
;;          ($display-char #\())
;;      ($display-obj (car obj))
;;      (if (pair? (cdr obj))
;;          (begin
;;            ($display-char #\Space)
;;            ($display-pair (cdr obj) #f))
;;          (if (not (null? (cdr obj)))
;;              (begin
;;                ($display-char #\Space)
;;                ($display-char #\.)
;;                ($display-char #\Space)
;;                ($display-obj (cdr obj)))))
;;      (if top
;;          ($display-char #\)))))


;; VM method

;; Conditions
(define-condition stop-vm-condition (condition)
  ())

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric vm-stack-top (vm)
  (:documentation "car of stack."))

(defmethod vm-stack-top ((vm vm))
  (with-accessors ((sp sp-of)) vm
    (vm-car vm sp)))



