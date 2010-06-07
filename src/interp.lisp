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

(defun locate (m n e) ;; for Common Lisp interpreter
  (declare (optimize speed))
  (declare (type fixnum m))
  (declare (type fixnum n))
  (nth (- n 1) (nth (- m 1) e)))

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
  )
     

(defmacro def-secd-machine (name doc &rest body)
  (let ((s (gensym "S ")) (e (gensym "E ")) (c (gensym "C ")) (d (gensym "D ")))
    `(progn
       (defun ,name (,s ,e ,c ,d)
         ,doc
         (tagbody
          :loop
            (format t ";; ~s~%" (list ,s ,e ,c ,d))
            (match (list ,s ,e ,c ,d)
              ,@(loop :for rule :in body
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
            (values 'stop ,s))))))



