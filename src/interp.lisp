(in-package :secd.interp)

;; PAIP 9.2 Compiling One Language into Another
(defstruct rule states lhs rhs var init-form rhs-action)


(defun collect-syms (lst)
  "collect state symbols."
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

(defun compile-transition (states transition)
  "compile transition into rule structure."
  (let* ((pos-arrow (position '-> transition :test #'equal))
         (pos-where (position 'where transition :test #'equal))
         (lhs (subseq transition 0 pos-arrow))
         (rhs (subseq transition (1+ pos-arrow) (+ (1+ pos-arrow) (length lhs))))
         (var (if pos-where
                  (nth (1+ pos-where) transition)))
         (init-form (if pos-where
                        (nth (+ 3 pos-where) transition))))
    (assert (and pos-arrow (> pos-arrow 0)) (pos-arrow) "compile-transition found invalid format: ~s" transition)
    (let ((rule (make-rule :states states :lhs lhs :rhs rhs :var var :init-form init-form)))
      (labels ((state->cons (state)
             (if (consp state)
                 `(cons ,(state->cons (car state))
                        ,(state->cons (cdr state)))
                 state)))
        (let ((body (loop :for var :in (rule-states rule)
                       :for rhs-state :in (rule-rhs rule)
                       :for form = (state->cons rhs-state)
                       :unless (eql var form)
                       :append `(,var ,form))))
          (setf (rule-rhs-action rule)
                (if (rule-var rule)
                    `(let ((,(rule-var rule) ,(rule-init-form rule)))
                       (psetq ,@body))
                    `(psetq ,@body)))))
      rule)))

(defun compile-transitions (states transitions)
  "Parse specs into list of rule structure.
rule: left-hand-side -> right-hand-side or left-hand-side -> right-hand-side where var = init-form"
  (loop :for transition :in transitions
     :for rule-obj = (compile-transition states transition)
     :collect rule-obj :into definitions
     :finally
     (multiple-value-bind (success result)
         (validate-rules definitions)
       (if success
           (return result)
           (error "found invalid rule: ~a" result)))))

(defun rules->match-n (rules &optional cont base-case)
  "Convert rules to match-n"
  (let ((states (rule-states (car rules))))
    `(match-n ,states
       ,@(loop :for rule :in rules
            :for pattern = (rule-lhs rule)
            :for body = (rule-rhs-action rule)
            :collect `(,pattern (progn ,body ,cont)))
       ;; base case
       (,(loop :for s :in states :collect 't)
         (,base-case ,@states)))))

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

(defgeneric secd-eval (fn exp)
  (:documentation "generic launcher."))

;; new version.
(defmacro deftransition (name (&rest states) &rest spec)
  "define secd machine."
  (let ((transitions (cdr (assoc :transitions spec)))
        (last-value (assoc :last-value spec)))
    (let* ((states (loop :for s :in states :collect (gensym (mkstr s))))
           (rules (mapcar #'(lambda (tr)
                              (compile-transition states tr)) transitions)))
      (format t "rules = ~a~%" rules)
    `(progn
       (defun ,name (,@states)
         ;;(declare (optimize (debug 0) (speed 3) (safety 0)))
         ,(rules->match-n rules `(,name ,@states) (cadr last-value))
         )))))

;; (defmacro defsecd (name (&rest states) doc &rest definitions)
;;   "define secd machine."
;;   (let* ((syms (loop :for s :in states :collect (gensym (mkstr s))))
;;          (rules (parse-rules syms definitions)))
;;     `(progn
;;        (defun ,name ,syms
;;          ,doc
;;          ;; (declare (optimize (debug 0) (speed 3) (safety 0)))
;;          ;;(tagbody :loop
;;          ;;   ,(rules->match-n rules '(go :loop)))

;;          ;; ,(rules->match-n rules
;;          ;;                  `(progn (format t "~s~%" (list ,@syms)) (,name ,@syms)))

;;          ,(rules->match-n rules `(,name ,@syms))
;;          ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stack

(defun pattern->cons (pattern)
  "(a . b) => (cons a b)"
  (if (consp pattern)
      `(cons ,(pattern->cons (car pattern))
             ,(pattern->cons (cdr pattern)))
      pattern))

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
            ;;:unless (eql x0 x1)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compiled 
;; TEST> (test-secd-eval '((lambda () 13)))
;; 13
;; TEST> (comp '((lambda () 13)))
;; (:NIL :LDF (:LDC 13 :RTN) :AP :STOP)


(defun foobar (s e c d)
  ;; :nil
  (setq s (cons nil s))
  ;; :ldf
  (setq s
        (cons
         #'(lambda ()
             ;; :ldc 13
             (setq s (cons 13 s))
             ;; :rtn
             (psetq s (cons (car d) (cdr s))
                    e (cadr d)
                    c (caddr d)
                    d (cdddr d)))
         e))
  ;; :ap
  ;; (psetq s nil
  ;;        e (cons (cadr s) (cdar s))


  )
  
