(in-package :secd)

;; PAIP 9.2 Compiling One Language into Another
(defstruct rule states lhs rhs var init-form lhs-states rhs-action)

(defun collect-syms (lst)
  "Collect state symbols."
  (remove-if #'instruction-p (remove-duplicates (flatten lst))))

(defun valid-rule (rule)
  "Validate a rule structure."
  (let ((lhs-syms (collect-syms (if (rule-var rule)
                                    (append (rule-lhs rule) (rule-var rule))
                                    (rule-lhs rule))))
        (rhs-syms (collect-syms (rule-rhs rule))))
    (null (set-difference rhs-syms lhs-syms))))

(defun validate-rules (rules)
  "Validate list of rule structure."
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
  "Compile a transition into a rule structure."
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
      (setf (rule-lhs-states rule) (collect-syms (rule-lhs rule)))
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
                `(let ,(if (rule-var rule)
                           `((,(rule-var rule) ,(rule-init-form rule)))
                           ())
                   (when *secd-debug*
                     (let ((*print-circle* t))
                       (format t ";;;~%")
                       (format t "STATE MATCH ~a  -> ~a~%" ',(rule-lhs rule) ',(rule-rhs rule))
                       ,@(loop :for s :in (rule-lhs-states rule)
                            :collect `(format t " ~a = ~s~%" ',s ,s))
                       ,(when (rule-var rule)
                              `(format t " RHS VAR ~a = ~s~%" ',(rule-var rule) ,(rule-var rule)))))
                   (psetq ,@body)))))
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

;; for Common Lisp interpreter
(defun locate (m n e)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum m n ))
  (nth (the fixnum (1- n)) (nth (the fixnum (1- m)) e)))

(defun (setf locate) (var m n e)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum m n ))
  (setf (nth (the fixnum (1- n)) (nth (the fixnum (1- m)) e)) var))

(defun make-vector (l)
  (apply #'vector l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun max-rule-width (rules)
  (let ((states
         (loop :for rule :in rules
            :collect
            (append (rule-lhs rule) (rule-rhs rule)))))
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
  "Describe secd rules."
  (with-output-to-string (out)
    (let ((*print-pretty* nil))
      (destructuring-bind  (s0w e0w c0w d0w s1w e1w c1w d1w)
          (max-rule-width rules)
        (let ((w (+ (apply #'+ (list s0w e0w c0w d0w s1w e1w c1w d1w))
                    (length "-> "))))
          (flet ((hr () (loop :repeat w :do (format out "-") :finally (format out "~%"))))
            (format out
                    "           INITIAL STATE                                                  TRANSFORMED STATE~%")
            (format out "~va~va~va~va   ~va~va~va~va~%" s0w 's e0w 'e c0w 'c d0w 'd s1w 's e1w 'e c1w 'c d1w 'd)
            (hr)
            (loop :for rule :in rules 
               :do
               (let ((s0 (nth 0 (rule-lhs rule)))
                     (e0 (nth 1 (rule-lhs rule)))
                     (c0 (nth 2 (rule-lhs rule)))
                     (d0 (nth 3 (rule-lhs rule)))
                     (s1 (nth 0 (rule-rhs rule)))
                     (e1 (nth 1 (rule-rhs rule)))
                     (c1 (nth 2 (rule-rhs rule)))
                     (d1 (nth 3 (rule-rhs rule))))
                 (progn
                   (format out "~(~va~va~va~va-> ~va~va~va~va~)~%"
                           s0w s0 e0w e0 c0w c0 d0w d0 s1w s1 e1w e1 c1w c1 d1w d1)
                   (when (rule-init-form rule)
                     (format out "~vtwhere ~(~a = ~a~)~%"
                             (apply #'+ (list s0w e0w c0w d0w (length "-> ")))
                             (rule-var rule) (rule-init-form rule))))))
            (hr)))))))

(defparameter *secd-debug* nil)

(defmacro deftransition (name (&rest states) &rest spec)
  "Define a SECD machine."
  (let ((transitions (cdr (assoc :transitions spec)))
        (last-value (cadr (assoc :last-value spec))))
    (let* ((states (loop :for s :in states :collect (gensym (mkstr s))))
           (rules (compile-transitions states transitions))
           (desc (describe-secd rules)))
      `(progn
         (format t ";; ~a transition~%~a~%" ',name ,desc)
         (defun ,name (,@states)
           ,desc
           ;;(declare (optimize (debug 0) (speed 3) (safety 0)))
           (when *secd-debug*
             (format t "STATE:~% S = ~s~% E = ~s~% C = ~s~% D = ~s~%" ,@states))
           ,(rules->match-n rules `(,name ,@states) last-value))
         (export ',name)))))

(defun secd-eval (exp &key (debug nil) (optimize t))
  "Eval an expression."
  (let* ((c (opt (compile-pass1 exp))))
    (let ((*secd-debug* debug)
          (*print-circle* t))
      (secd 's0 'e0 `(,@c . c0) 'd0))))