(in-package :secd.interp)

;; PAIP 9.2 Compiling One Language into Another

;; ( s e (:NIL . c) d                         -> (nil . s) e c d )
;; ( s e (:LDC x . c) d                       -> (x . s) e c d )
;; ( (a b . s) e (:+ . c) d                   -> (x . s) e c d where x = (+ a b) )

(defstruct rule states lhs rhs var init-form)

(defun collect-syms (lst)
  "collect not instruction."
  (remove-if #'instruction-p (remove-duplicates (flatten lst))))

(defun valid-rule (rule)
  "validate a rule structure."
  (let ((lhs-syms (collect-syms (if (rule-var rule)
                                    (append (rule-lhs rule) (rule-var rule))
                                    (rule-lhs rule))))
        (rhs-syms (collect-syms (rule-rhs rule))))
    (null (set-difference rhs-syms lhs-syms))))

(defun validate-rules (rules)
  "validate list of rule structure."
  (and (every #'valid-rule rules)
       (loop :for rule :in rules
          :for lhs = (rule-lhs rule)
          :if (member lhs lhss :test #'equal)
          :collect lhs :into duplicate-lhs
          :else
          :collect lhs :into lhss
          :finally
          (if (null duplicate-lhs)
              (return (values t rules))
              (return (values nil duplicate-lhs))))))

(defun parse-rules (states rules)
  "Parse rules into list of rule structure.
rule: left-hand-side -> right-hand-side or left-hand-side -> right-hand-side where var = init-form"
  (loop :for rule :in rules
     :for pos-arrow = (position '-> rule :test #'equal)
     :for pos-where = (position 'where rule :test #'equal)
     :for lhs = (subseq rule 0 pos-arrow)
     :for rhs = (subseq rule (1+ pos-arrow) (+ (1+ pos-arrow) (length lhs)))
     :for var = (if pos-where
                    (nth (1+ pos-where) rule))
     :for init-form = (if pos-where
                          (nth (+ 3 pos-where) rule))
     :for rule-obj = (make-rule :states states :lhs lhs :rhs rhs :var var :init-form init-form)
     :collect rule-obj :into definitions
     :finally
     (multiple-value-bind (success result)
         (validate-rules definitions)
       (if success
           (return result)
           (error "found invalid rule: ~a" result)))))

(defun rule->match-n-pattern (rule)
  `(,(rule-lhs rule)))

(defun rule->match-n-body (rule)
  (labels ((state->cons (state)
             (if (consp state)
                 `(cons ,(state->cons (car state))
                        ,(state->cons (cdr state)))
                 state)))
    (let ((body (loop :for state :in (rule-states rule)
                   :for rhs-state :in (rule-rhs rule)
                   :collect `(setq ,state ,(state->cons rhs-state)))))
      (if (rule-var rule)
          `(let ((,(rule-var rule) ,(rule-init-form rule)))
             (progn ,@body))
          `(progn ,@body)))))

;; (defun rule->match-n-clause (rule &optional cont)
;;   "Convert a rule to match-n clauses."
;;   `(,@(rule->match-n-pattern rule)
;;      ,(rule->match-n-body rule)))

(defun rules->match-n (rules &optional cont)
  "Convert rules to match-n"
  (let ((states (rule-states (car rules))))
    `(match-n ,states
       ,@(loop :for rule :in rules
            :for pattern = (rule->match-n-pattern rule)
            :for body = (rule->match-n-body rule)
            :collect
            `(,@pattern
              (progn
                ,body
                ,cont))))))

(defun pattern->cons (pattern)
  "(a . b) => (cons a b)"
  (if (consp pattern)
      `(cons ,(pattern->cons (car pattern))
             ,(pattern->cons (cdr pattern)))
      pattern))


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

(declaim (inline locate))
(defun locate (m n e) ;; for Common Lisp interpreter
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum m n ))
  (nth (the fixnum (1- n)) (nth (the fixnum (1- m)) e)))

(declaim (inline locate2))
(defun locate2 (m n e)
  "faster locate"
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (declare (type fixnum m n ))
  (nth n (nth m e)))

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
;;    ((:LD (m . n))
;;     `(:LD2 (,(1- m) . ,(1- n)))) ;; not work.
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


(defgeneric run (secd exp)
  (:documentation "generic launcher."))

(defun secd-trans (s e c d s0 e0 c0 d0 s1 e1 c1 d1)
  (let ((setq-body
         (loop :for v :in (list s e c d)
            :for x0 :in (list s0 e0 c0 d0)
            :for x1 :in (list s1 e1 c1 d1)
            :unless (eql x0 x1)
            :append
            `(,v ,(pattern->cons x1)))))
    `(setq ,@setq-body)))

(defmacro defsecd (name (&rest states)
                   doc &rest definitions)
  "define secd machine."
  (let* ((syms (loop :for s :in states :collect (gensym (mkstr s))))
         (rules (parse-rules syms definitions)))
    `(progn
       (defun ,name ,syms
         ,doc
         (declare (optimize (debug 0) (speed 3) (safety 0)))
         (tagbody :loop
            ,(rules->match-n rules '(go :loop)))
         (values (list ,@syms) 'end))
       ',name)))

;; match-n ver.
(defmacro def-secd-machine (name doc &rest rules)
  "define secd machine."
  (let ((s (gensym "S ")) (e (gensym "E ")) (c (gensym "C ")) (d (gensym "D "))
        (exp (gensym "exp ")))
    ;;(validate-rules rules)
    ;;(gather-instructions rules)
    `(progn
       (defparameter ,name (describe-secd ',rules))
       (defun ,name (,s ,e ,c ,d)
         ,doc
         (declare (optimize (debug 0) (speed 3) (safety 0)))
         (tagbody
          :loop
            (format t ";; ~s ~s ~s ~s ~%" ,s ,e ,c ,d)
            (match-n (,c ,s ,e ,d)
              ,@(loop :for rule :in rules
                   :collect
                   (match rule
                     ((s0 e0 c0 d0 :_ s1 e1 c1 d1 'where var '= init-form)
                      `((,c0 ,s0 ,e0 ,d0)
                        (let ((,var ,init-form))
                          ,(secd-trans s e c d s0 e0 c0 d0 s1 e1 c1 d1)
                          (go :loop))))
                     ((s0 e0 c0 d0 :_ s1 e1 c1 d1)
                      `((,c0 ,s0 ,e0 ,d0)
                        ,(secd-trans s e c d s0 e0 c0 d0 s1 e1 c1 d1)
                        (go :loop)))))))
         (values (list ,s ,e ,c ,d) 'end))
       (defmethod run ((secd (eql ,(as-keyword name))) ,exp)
         (let ((,c (compile-pass1 ,exp ())))
           (format t ";; code: ~s~%" ,c)
           (,name ',s ',e ,c ',d)))
       ',name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stack
(defun pattern->stack (pattern)
  "(a . b) => "
  (if (consp pattern)
      `(cons ,(pattern->cons (car pattern))
             ,(pattern->cons (cdr pattern)))
      pattern))

;;   ( s e (:LDC x . c) d                          -> (x . s) e c d )
(defun secd-trans-stack (s e c d s0 e0 c0 d0 s1 e1 c1 d1)
  (let ((body
         (loop :for v :in (list s e c d)
            :for x0 :in (list s0 e0 c0 d0)
            :for x1 :in (list s1 e1 c1 d1)
            :unless (eql x0 x1)
            :append
            `(,v ,(pattern->stack x1)))))
    `(progn ,@body)))

(defmacro def-secd-stack-machine (name doc &rest rules)
  "define secd stack machine."
  (let ((s (gensym "S ")) (e (gensym "E ")) (c (gensym "C ")) (d (gensym "D "))
        (exp (gensym "exp ")))
    ;;(validate-rules rules)
    ;;(gather-instructions rules)
    `(progn
       (defparameter ,name (describe-secd ',rules))
       (defun ,name (,s ,e ,c ,d)
         ,doc
         (declare (optimize (debug 0) (speed 3) (safety 0)))
         (tagbody
          :loop
            (match-n (,c ,s ,e ,d)
              ,@(loop :for rule :in rules
                   :collect
                   (match rule
                     ((s0 e0 c0 d0 :_ s1 e1 c1 d1 'where var '= init-form)
                      `((,c0 ,s0 ,e0 ,d0)
                        (let ((,var ,init-form))
                          ,(secd-trans-stack s e c d s0 e0 c0 d0 s1 e1 c1 d1)
                          (go :loop))))
                     ((s0 e0 c0 d0 :_ s1 e1 c1 d1)
                      `((,c0 ,s0 ,e0 ,d0)
                        ,(secd-trans-stack s e c d s0 e0 c0 d0 s1 e1 c1 d1)
                        (go :loop)))))))
         (values (list ,s ,e ,c ,d) 'end))
       (defmethod run ((secd (eql ,(as-keyword name))) ,exp)
         (let ((,c (compile-pass1 ,exp ())))
           (format t ";; code: ~s~%" ,c)
           (,name ',s ',e ,c ',d)))
       ',name)))
