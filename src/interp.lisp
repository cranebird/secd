(in-package :secd.interp)

(defun instruction-p (x) (keywordp x))

(defun pattern->cons (pattern)
  "(a . b) => (cons a b)"
  (if (consp pattern)
      `(cons ,(pattern->cons (car pattern))
             ,(pattern->cons (cdr pattern)))
      pattern))

(defun collect-syms (lst)
  "collect not instruction."
  (remove-if #'instruction-p (remove-duplicates (flatten lst))))

(defun ignorable-sym (state1 state2)
  (loop :for x :in (collect-syms state1)
     :if (not (member x (intersection (collect-syms state1) (collect-syms state2))))
     :collect x))

(defun gather-instructions (rules)
  (let ((instructions
         (loop :for rule :in rules
            :append (remove-if-not #'instruction-p (remove-duplicates (flatten rule))))))
    (format t ";; instructions: ~s~%" instructions)
    instructions))

(defun validate-rule (rule)
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
  (let ((ht (make-hash-table :test #'equal)))
    (loop :for rule :in rules
       :do
       (validate-rule rule)
       (multiple-value-bind (val present-p)
           (gethash rule ht)
         (if present-p
             (error "multiple rule found: ~s" val)
             (setf (gethash rule ht) rule))))))

(defun locate (m n e) ;; for Common Lisp interpreter
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum m n ))
  (nth (the fixnum (1- n)) (nth (the fixnum (1- m)) e)))

;; (defun locate (m n e) ;; for Common Lisp interpreter
;;   (nth (1- n) (nth (1- m) e)))

(defun make-vector (l)
  (apply #'vector l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun max-rule-width (rules)
  (let ((states
         (loop :for rule :in rules
            :collect
            (match rule
              ((s0 e0 c0 d0 :_ s1 e1 c1 d1 'where var '= init-form)
               (list s0 e0 c0 d0 s1 e1 c1 d1))
              ((s0 e0 c0 d0 :_ s1 e1 c1 d1)
               (list s0 e0 c0 d0 s1 e1 c1 d1))))))
    (loop :for (s0 e0 c0 d0 s1 e1 c1 d1) :in states
       :with margin = 3
       :maximize (length (mkstr s0)) :into s0w
       :maximize (length (mkstr e0)) :into e0w
       :maximize (length (mkstr c0)) :into c0w
       :maximize (length (mkstr d0)) :into d0w
       :maximize (length (mkstr s1)) :into s1w
       :maximize (length (mkstr e1)) :into e1w
       :maximize (length (mkstr c1)) :into c1w
       :maximize (length (mkstr d1)) :into d1w
       :finally (return (mapcar (lambda (n) (+ margin n)) (list s0w e0w c0w d0w s1w e1w c1w d1w))))))

(defun describe-secd (rules)
  "describe secd rules"
  (with-output-to-string (out)
    (destructuring-bind  (s0w e0w c0w d0w s1w e1w c1w d1w)
        (max-rule-width rules)
      (let ((w (+ (apply #'+ (list s0w e0w c0w d0w s1w e1w c1w d1w))
                  (length "-> "))))
        (flet ((hr () (loop :repeat w :do (format out "-") :finally (format out "~%"))))
          (format out
                  "           INITIAL STATE                                             TRANSFORMED STATE~%")
          (format out "~va~va~va~va   ~va~va~va~va~%" s0w 's e0w 'e c0w 'c d0w 'd s1w 's e1w 'e c1w 'c d1w 'd)
          (hr)
          (loop :for rule :in rules 
             :do
             (match rule
               ((s0 e0 c0 d0 :_ s1 e1 c1 d1 'where var '= init-form)
                (format out "~(~va~va~va~va-> ~va~va~va~va~)~%"
                        s0w s0 e0w e0 c0w c0 d0w d0 s1w s1 e1w e1 c1w c1 d1w d1)
                (format out "~vtwhere ~(~a = ~a~)~%"
                        (apply #'+ (list s0w e0w c0w d0w (length "-> ")))
                        var init-form))
               ((s0 e0 c0 d0 :_ s1 e1 c1 d1)
                (format out "~(~va~va~va~va-> ~va~va~va~va~)~%"
                        s0w s0 e0w e0 c0w c0 d0w d0 s1w s1 e1w e1 c1w c1 d1w d1))))
          (hr))))))


;; todo place
(defun opt (program)
  (match program
    (()
     nil)
    ((:AP :RTN)
     `(:TAP))
    ((:RAP :RTN)
     `(:RTAP))
    ((:SEL ct cf :RTN)
     (if (and (equal (last ct) '(:JOIN)) (equal (last cf) '(:JOIN)))
         (opt `(:SELR ,(opt (append (butlast ct) '(:RTN))) 
                      ,(opt (append (butlast cf) '(:RTN)))))
         `(:SEL ,(opt ct) ,(opt cf) :RTN)))
    (t
     (if (consp program)
         (cons (opt (car program)) (opt (cdr program)))
         program))))

(defun superinstruction (program)
  )

(defgeneric run-secd (secd exp)
  (:documentation "generic launcher."))

;; match-n ver
 (defun secd (s e c d)
   (match-n (s e c d)
            ((s e (:LDC n . c) d) (list :ldc n))
            ((s e c d) 'base)))

(defmacro def-secd-machine (name doc &rest rules)
  "define secd machine."
  (let ((s (gensym "S ")) (e (gensym "E ")) (c (gensym "C ")) (d (gensym "D "))
        (s0 (gensym))
        (e0 (gensym))
        (c0 (gensym))
        (d0 (gensym))
        (exp (gensym "exp ")))
    ;;(validate-rules rules)
    ;;(gather-instructions rules)
    `(progn
       (defparameter ,name (describe-secd ',rules))
       (defun ,name (,s ,e ,c ,d)
         ,doc
         (declare (optimize (speed 3) (safety 0)))
         (tagbody
          :loop
            ;; (format t ";; ~s ~s ~s ~s ~%" ,s ,e ,c ,d)
            (match-n (,s ,e ,c ,d)
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
                               (go :loop)))))))
         (values (list ,s ,e ,c ,d) 'end))
       (defmethod run-secd ((secd (eql ,(as-keyword name))) ,exp)
         (let ((,c (compile-pass1 ,exp ())))
           (format t ";; code: ~s~%" ,c)
           (,name ',s ',e ,c ',d)))
       ',name)))

;; one list ver.
(defmacro def-secd-machine-one-list (name doc &rest rules)
  "define secd machine."
  (let ((s (gensym "S ")) (e (gensym "E ")) (c (gensym "C ")) (d (gensym "D "))
        (secd (gensym "SECD"))
        (exp (gensym "exp ")))
    (validate-rules rules)
    (gather-instructions rules)
    `(progn
       (defparameter ,name (describe-secd ',rules))
       (defun ,name (,s ,e ,c ,d)
         ,doc
         (declare (optimize (speed 3) (safety 0)))
         (let ((,secd (list ,s ,e ,c ,d)))
           (tagbody
            :loop
              (let ((*print-circle* t))
                (format t ";; ~s~%" ,secd))
              (match ,secd
                ,@(loop :for rule :in rules
                     :collect
                     (match rule
                       ((s0 e0 c0 d0 :_ s1 e1 c1 d1 'where var '= init-form)
                        `((,s0 ,e0 ,c0 ,d0)
                          (let ((,var ,init-form))
                            (psetf (nth 0 ,secd) ,(pattern->cons s1)
                                   (nth 1 ,secd) ,(pattern->cons e1)
                                   (nth 2 ,secd) ,c1
                                   (nth 3 ,secd) ,(pattern->cons d1))
                            (go :loop))))
                       ((s0 e0 c0 d0 :_ s1 e1 c1 d1)
                        `((,s0 ,e0 ,c0 ,d0)
                          (psetf (nth 0 ,secd) ,(pattern->cons s1)
                                 (nth 1 ,secd) ,(pattern->cons e1)
                                 (nth 2 ,secd) ,c1
                                 (nth 3 ,secd) ,(pattern->cons d1))
                          (go :loop)))))))
           (values ,secd 'end)))
       (defmethod run-secd ((secd (eql ,(as-keyword name))) ,exp)
         (let ((,c (compile-pass1 ,exp ())))
           (format t ";; code: ~s~%" ,c)
           (,name ',s ',e ,c ',d)))
       ',name)))

;; no list ver. care multiple rule for :AP
;;; 20100627 bug?
(defmacro def-secd-machine-%% (name doc &rest rules)
  "define secd machine."
  (let ((s (gensym "S ")) (e (gensym "E ")) (c (gensym "C ")) (d (gensym "D "))
        (exp (gensym "exp ")))
    (validate-rules rules)
    (gather-instructions rules)
    `(progn
       (defparameter ,name (describe-secd ',rules))
       (defun ,name (,s ,e ,c ,d)
         ,doc
         (declare (optimize (speed 3) (safety 0)))
         (let ((*print-circle* nil))
           (format t "~(~80s~40s~120s~60s~)~%" ,s ,e ,c ,d))
         (tagbody
          :loop
            (let ((*print-circle* nil))
              (format t "~80s~40s~120s~60s~%" ,s ,e ,c ,d))
            (match (list ,s ,c)
              ,@(loop :for rule :in rules
                   :collect
                   (match rule
                     ((s0 e0 c0 d0 :_ s1 e1 c1 d1 'where var '= init-form)
                      `((,s0 ,c0)
                        (match ,s
                          (,s0
                           (match ,e
                             (,e0
                              (match ,d
                                (,d0
                                 (let ((,var ,init-form))
                                   (psetq ,s ,(pattern->cons s1)
                                          ,e ,(pattern->cons e1)
                                          ,c ,c1
                                          ,d ,(pattern->cons d1))
                                   (go :loop))))))))))
                     ((s0 e0 c0 d0 :_ s1 e1 c1 d1)
                      `((,s0 ,c0)
                        (match ,s
                          (,s0
                           (match ,e
                             (,e0
                              (match ,d
                                (,d0
                                 (psetq ,s ,(pattern->cons s1)
                                        ,e ,(pattern->cons e1)
                                        ,c ,c1
                                        ,d ,(pattern->cons d1))
                                 (go :loop)))))))))))))
         (values (list ,s ,e ,c ,d) 'end))
       (defmethod run-secd ((secd (eql ,(as-keyword name))) ,exp)
         (let ((,c (compile-pass1 ,exp ())))
           (format t ";; code: ~s~%" ,c)
           (,name ',s ',e ,c ',d)))
       ',name)))

(defmacro def-secd-machine% (name doc &rest rules)
  "define secd machine."
  (let ((s (gensym "S ")) (e (gensym "E ")) (c (gensym "C ")) (d (gensym "D "))
        (exp (gensym "exp ")))
    (validate-rules rules)
    (gather-instructions rules)
    `(progn
       (defparameter ,name (describe-secd ',rules))
       (defun ,name (,s ,e ,c ,d)
         ,doc
         (declare (optimize (speed 3) (safety 0)))
         (let ((*print-circle* nil))
           (format t "~(~80s~40s~120s~60s~)~%" ,s ,e ,c ,d))
         (tagbody
          :loop
            (let ((*print-circle* nil))
              (format t "~80s~40s~120s~60s~%" ,s ,e ,c ,d))
            (match ,c
              ,@(loop :for rule :in rules
                   :collect
                   (match rule
                     ((s0 e0 c0 d0 :_ s1 e1 c1 d1 'where var '= init-form)
                      `(,c0
                        (match ,s
                          (,s0
                           (match ,e
                             (,e0
                              (match ,d
                                (,d0
                                 (let ((,var ,init-form))
                                   (psetq ,s ,(pattern->cons s1)
                                          ,e ,(pattern->cons e1)
                                          ,c ,c1
                                          ,d ,(pattern->cons d1))
                                   (go :loop))))))))))
                     ((s0 e0 c0 d0 :_ s1 e1 c1 d1)
                      `(,c0
                        (match ,s
                          (,s0
                           (match ,e
                             (,e0
                              (match ,d
                                (,d0
                                 (psetq ,s ,(pattern->cons s1)
                                        ,e ,(pattern->cons e1)
                                        ,c ,c1
                                        ,d ,(pattern->cons d1))
                                 (go :loop)))))))))))))
         (values (list ,s ,e ,c ,d) 'end))
       (defmethod run-secd ((secd (eql ,(as-keyword name))) ,exp)
         (let ((,c (compile-pass1 ,exp ())))
           (format t ";; code: ~s~%" ,c)
           (,name ',s ',e ,c ',d)))
       ',name)))