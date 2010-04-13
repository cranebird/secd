;;; Utility
;; from "Practical Common Lisp"
(in-package :secd)

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

(defun as-keyword (sym) (intern (string sym) :keyword))