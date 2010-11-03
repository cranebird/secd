(in-package :secd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PAIP 9.2 Compiling One Language into Another
(defstruct rule lhs rhs var init-form lhs-states)

(define-condition compile-rule-error (simple-error)
  ((reason :initarg :reason :accessor compile-rule-error-reason))
  (:report (lambda (condition stream)
             (format stream "Define SECD Error: ~a"
                     (compile-rule-error-reason condition)))))

(defun rule-error (reason &rest args)
  (error (make-condition 'compile-rule-error
                         :reason (apply #'format nil reason args))))

(defun collect-syms (lst)
  "Collect state symbols in list LST."
  (remove-if #'instruction-p (remove-duplicates (flatten lst))))

(defun valid-rule-p (rule)
  "Validate a rule structure."
  (let ((lhs-syms (collect-syms (if (rule-var rule)
                                    (append (rule-lhs rule) (rule-var rule))
                                    (rule-lhs rule))))
        (rhs-syms (collect-syms (rule-rhs rule))))
    (null (set-difference rhs-syms lhs-syms))))

(defun unique-rules-p (rules)
  "Return nil if rules contain duplicate rule(s)."
  (= (length rules)
     (length (remove-duplicates rules :test #'equal :key #'rule-lhs))))

(defun compile-transition (transition)
  "Compile a transition into a rule structure."
  (let* ((pos-arrow (position '-> transition :test #'equal))
         (pos-where (position 'where transition :test #'equal))
         (lhs (subseq transition 0 pos-arrow))
         (rhs (subseq transition (1+ pos-arrow)
                      (+ (1+ pos-arrow) (length lhs))))
         (var (if pos-where
                  (nth (1+ pos-where) transition)))
         (init-form (if pos-where
                        (nth (+ 3 pos-where) transition))))
    (assert (and pos-arrow (> pos-arrow 0)) (pos-arrow)
            "compile-transition found invalid format: ~s" transition)
    (make-rule :lhs lhs :rhs rhs :var var :init-form init-form
               :lhs-states (collect-syms lhs))))

(defun make-transit (states rule)
  "Generate transit function."
  (labels ((state->cons (state)
             (if (consp state)
                 `(cons ,(state->cons (car state))
                        ,(state->cons (cdr state)))
                 state)))
    (let ((body (loop :for var :in states
                   :for rhs-state :in (rule-rhs rule)
                   :for form = (state->cons rhs-state)
                   :unless (eql var form)
                   :append `(,var ,form))))
      `(let ,(if (rule-var rule)
                 `((,(rule-var rule) ,(rule-init-form rule)))
                 ())
         (when *secd-debug*
           (let ((*print-circle* t))
             (format t ";;;~%")
             (format t "STATE MATCH ~a  -> ~a~%"
                     ',(rule-lhs rule) ',(rule-rhs rule))
             ,@(loop :for s :in (rule-lhs-states rule)
                  :collect `(format t " ~a = ~s~%" ',s ,s))
             ,(when (rule-var rule)
                    `(format t " RHS VAR ~a = ~s~%"
                             ',(rule-var rule) ,(rule-var rule)))))
         (psetq ,@body)))))

(defun rules->match-n (states rules cont base-case)
  "Convert rules to match-n."
  `(match-n ,states
     ,@(loop :for rule :in rules
          :for pattern = (rule-lhs rule)
          :collect `(,pattern (progn ,(make-transit states rule) ,cont)))
     ;; base case
     (,(loop :for s :in states :collect 't)
       (,base-case ,@states))))

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
       :finally
       (return
         (mapcar (lambda (n) (+ margin n))
                 (list s0w e0w c0w d0w s1w e1w c1w d1w))))))

(defun describe-secd (rules) ;; ugly
  "Describe secd rules."
  (with-output-to-string (out)
    (let ((*print-pretty* nil))
      (destructuring-bind (s0w e0w c0w d0w s1w e1w c1w d1w)
          (max-rule-width rules)
        (let ((w (+ (apply #'+ (list s0w e0w c0w d0w s1w e1w c1w d1w))
                    (length "-> "))))
          (flet ((hr ()
                   (loop :repeat w :do (format out "-") :finally (format out "~%"))))
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
    (let* ((states (mapcar #'(lambda (x) (gensym (mkstr x))) states))
           (rules (mapcar #'compile-transition transitions))
           (desc (describe-secd rules)))
      (let ((invalid-rules (find-if-not #'valid-rule-p rules)))
        (when invalid-rules
          (rule-error "Found invalid rules: ~s" invalid-rules)))
      (unless (unique-rules-p rules)
        (rule-error "Found duplicate rules."))
      `(progn
         (format t ";; ~a transition~%~a~%" ',name ,desc)
         
         (defun ,name (,@states)
           ,desc
           ;;(declare (optimize (debug 0) (speed 3) (safety 0)))
           (when *secd-debug*
             (format t "STATE:~% S = ~s~% E = ~s~% C = ~s~% D = ~s~%" ,@states))
           ,(rules->match-n states rules `(,name ,@states) last-value))
         (export ',name)))))

;; todo
;; (defmacro secd-maker (rules)
;;   `(lambda (s e c d)
;;     ,(rules->match-n rules))
;;   )

(defun secd-eval (exp &key (debug nil) (optimize t) (show nil))
  "Eval an expression."
  (let ((c (compile-pass1 exp)))
    (when optimize
      (setq c (opt c)))
    (let ((*secd-debug* debug)
          (*print-circle* t))
      (when show
        (format t ";; code: ~s~%" c))
      (secd 's0 'e0 `(,@c . c0) 'd0))))

;; (defun secd-eval (exp &key (debug nil) (optimize t) (show nil))
;;   "Eval an expression."
;;   (let ((c (if optimize
;;                (opt (compile-pass1 exp))
;;                (compile-pass1 exp))))
;;     (let ((*secd-debug* debug)
;;           (*print-circle* t))
;;       (when show
;;         (format t ";; code: ~s~%" c))
;;       (secd 's0 'e0 `(,@c . c0) 'd0))))