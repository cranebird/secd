(in-package :secd.interp)

(defun instruction-p (x) (keywordp x))

(defun rest->binding (rest)
  (if rest
      (progn
        (assert (and (consp rest) (eql (car rest) 'where)) (rest) "where  x = ...")
        (destructuring-bind (where var sym-eq init-form) rest
          (declare (ignore where))
          (declare (ignore sym-eq))
          `((,var ,init-form))))
      nil))

(defun pattern->cons (pattern)
  "(a . b) => (cons a b)"
  (if (consp pattern)
      `(cons ,(pattern->cons (car pattern))
             ,(pattern->cons (cdr pattern)))
      pattern))

;; (a . s) => 2
;; (a b . s) => 3
;; (s) => 1
;; ((a . b) . s)
(defun pattern-length (pattern)
  ""
  (cond
    ((consp pattern)
     (+ (pattern-length (car pattern))
        (pattern-length (cdr pattern))))
    ((null pattern)
     0)
    ((atom pattern)
     1)))

(defun collect-syms (lst)
  (remove-if #'instruction-p (remove-duplicates (flatten lst))))

(defun rest->syms (rest)
  (collect-syms (rest->binding rest)))

(defun ignorable-sym (state1 state2)
  (loop :for x :in (collect-syms state1)
     :if (not (member x (intersection (collect-syms state1) (collect-syms state2))))
     :collect x))

(defun validate-states (rule)
  (flet ((check (init trans)
                (let ((diff (set-difference (collect-syms trans) (collect-syms init))))
                  (unless (null diff)
                    (error "transformed state contain unknown symbol: ~s~%rule: ~s~%" diff rule)))))
    (match rule
      ((s0 e0 c0 d0 :_ s1 e1 c1 d1 'where var '= init-form)
       (declare (ignore init-form))
       (check (list s0 e0 c0 d0 var) (list s1 e1 c1 d1)))
      ((s0 e0 c0 d0 :_ s1 e1 c1 d1)
       (check (list s0 e0 c0 d0) (list s1 e1 c1 d1)))
      (t
       (error "rule is not expected form: ~s" rule)))))

(defun validate-rules (rules)
  (loop :for rule :in rules :do (validate-states rule)))

(defun locate (m n e) ;; for Common Lisp interpreter
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum m n ))
  (nth (the fixnum (1- n)) (nth (the fixnum (1- m)) e)))

(defmacro def-secd-machine% (name doc &rest body)
  (let ((s (gensym "S ")) (e (gensym "E ")) (c (gensym "C ")) (d (gensym "D ")))
    `(progn
       (defun ,name (,s ,e ,c ,d)
         ,doc
         (tagbody
          :loop
            ;; (format t ";; ~a~%" (list ,s ,e ,c ,d))
            (match (list ,s ,e ,c ,d)
              ,@(loop :for rule :in body
                   :collect
                   (destructuring-bind (s0 e0 c0 d0 arrow s1 e1 c1 d1 &rest rest) rule
                     (declare (ignore arrow))
                     `((,s0 ,e0 ,c0 ,d0)
                       (let ,(rest->binding rest)
                         ;; psetq is better ?
                         (psetq ,s ,(pattern->cons s1)
                                ,e ,(pattern->cons e1)
                                ,c ,c1
                                ,d ,(pattern->cons d1))
                         (go :loop))))))
            (values 'stop ,s))))))

(defun describe-secd (rules)
  rules
  )

(defmacro def-secd-machine (name doc &rest rules)
  (let ((s (gensym "S ")) (e (gensym "E ")) (c (gensym "C ")) (d (gensym "D ")))
    (validate-rules rules)
    `(progn
       (defun ,name (,s ,e ,c ,d)
         ,doc
         (declare (optimize (speed 3)))
         (tagbody
          :loop
            ;; (format t ";; ~s~%" (list ,s ,e ,c ,d))
            (match (list ,s ,e ,c ,d)
              ,@(loop :for rule :in rules
                   :collect
                   (match rule
                     ((s0 e0 c0 d0 :_ s1 e1 c1 d1 'where var '= init-form)
                      `((,s0 ,e0 ,c0 ,d0)
                        (let ((,var ,init-form))
                          (psetq ,s ,(pattern->cons s1)
                                 ,e ,(pattern->cons e1)
                                 ,c ,c1
                                 ,d ,(pattern->cons d1))
                          (go :loop))))
                     ((s0 e0 c0 d0 :_ s1 e1 c1 d1)
                      `((,s0 ,e0 ,c0 ,d0)
                        (psetq ,s ,(pattern->cons s1)
                               ,e ,(pattern->cons e1)
                               ,c ,c1
                               ,d ,(pattern->cons d1))
                        (go :loop))))))
            (values 'stop ,s))
         (list ,s ,e ,c ,d)))))



