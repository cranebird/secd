;;; Utility

(in-package :secd.util)

(defun set-scm-macro-character ()
  "set #f and #t"
  (set-dispatch-macro-character #\# #\t
                                #'(lambda (stream subchar arg)
                                    (declare (ignore stream subchar arg))
                                    :scheme-t))
  (set-dispatch-macro-character #\# #\f
                                #'(lambda (stream subchar arg)
                                    (declare (ignore stream subchar arg))
                                    :scheme-f)))

;; ANSI CL Macros

(defun single? (x)
  (and (consp x) (null (cdr x))))

;; "Practical Common Lisp" Macros
(defun as-keyword (sym) (intern (string sym) :keyword))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for n :in names :collect `(,n (gensym)))
     ,@body))

;; "Let Over Lambda" Macros

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

