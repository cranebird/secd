(in-package :secd.interp)

;; not used yet
(defun state-symbol-p (x)
  (and (symbolp x) (not (keywordp x))
       (member (char (symbol-name x) 0) '(#\s #\e #\c #\d) :test #'string-equal)))

(defun instruction-p (x) (keywordp x))

(defmacro rule->dbind (s e c d s0 e0 c0 d0 s1 e1 c1 d1)
  (with-gensyms (insn)
    `(,c0
      (destructuring-bind (,s0 ,e0 (,insn . ,c) ,d0) (list ,s ,e ,c ,d)
       '(,s1 ,e1 ,c1 ,d1)))))

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

(defun pattern->if (pattern x)
  (cond
    ((consp pattern)
      `((consp ,x) ,@(pattern->if (car pattern) `(car ,x))
              ,@(pattern->if (cdr pattern) `(cdr ,x))))
    ((null pattern)
     nil)))

(defun pattern->bind (pattern x)
  (cond
    ((atom pattern) `((,pattern ,x)))
    ((consp pattern)
     (cond
       ((null (cdr pattern))
        `(,@(pattern->bind (car pattern) `(car ,x))))
       (t
        `(,@(pattern->bind (car pattern) `(car ,x))
            ,@(pattern->bind (cdr pattern) `(cdr ,x))))))))

(defun pattern->cond (pattern body x)
  (loop :for claus :in (pattern->if pattern x)
     :collect claus :into res
     :finally (return `((and ,@res)
                        (let ,(pattern->bind pattern x)
                          ,@body)))))

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

(defmacro match (expr &rest pattern-body)
  (let ((x (gensym "x")))
  `(let ((,x ,expr))
     (cond
     ,@(loop :for (pattern . body) :in pattern-body
          :collect (pattern->cond pattern body x))))))

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

;; (defmacro def-transitions (name doc &rest body)
;;   (let ((s (gensym "S "))
;;         (e (gensym "E "))
;;         (c (gensym "C "))
;;         (d (gensym "D "))
;;         (insn (gensym "insn")))
;;     `(defun ,name (,s ,e ,c ,d)
;;        ,doc
;;        ;;(format t ";; s: ~s e: ~s c: ~s d: ~s~%" ,s ,e ,c ,d)
;;        (if (and (consp ,c) (instruction-p (car ,c)))
;;            (case (car ,c)
;;              ,@(loop :for rule :in body
;;                   :collect
;;                   (destructuring-bind (s0 e0 c0 d0 arrow s1 e1 c1 d1 &rest rest) rule
;;                     (declare (ignore arrow))
;;                     `(,(car c0)
;;                        (destructuring-bind (,s0 ,e0 ,(cons insn (cdr c0)) ,d0) (list ,s ,e ,c ,d)
;;                          (declare (ignore ,insn))
;;                          ,@(loop :for sym :in (ignorable-sym (list s0 e0 c0 d0) (list s1 e1 c1 d1 (rest->syms rest)))
;;                               :collect `(declare (ignore ,sym)))
;;                          (let ,(rest->binding rest)
;;                            (,name ,(pattern->cons s1) ,(pattern->cons e1) ,c1 ,(pattern->cons d1))))))))
;;            (values 'stop ,s)))))

;;; tagbody version.
(defmacro def-secd-machine-% (name doc &rest body)
  (let ((s (gensym "S "))
        (e (gensym "E "))
        (c (gensym "C "))
        (d (gensym "D "))
        (insn (gensym "insn")))
    `(defun ,name (,s ,e ,c ,d)
       ,doc
       ;;(format t ";; s: ~s e: ~s c: ~s d: ~s~%" ,s ,e ,c ,d)
       (tagbody
        :loop
          (if (and (consp ,c) (instruction-p (car ,c)))
              (case (car ,c)
                ,@(loop :for rule :in body
                     :collect
                     (destructuring-bind (s0 e0 c0 d0 arrow s1 e1 c1 d1 &rest rest) rule
                       (declare (ignore arrow))
                       `(,(car c0)
                          (destructuring-bind (,s0 ,e0 ,(cons insn (cdr c0)) ,d0)
                              (list ,s ,e ,c ,d)
                            (declare (ignore ,insn))
                            ,@(loop :for sym :in (ignorable-sym (list s0 e0 c0 d0) (list s1 e1 c1 d1 (rest->syms rest)))
                                 :collect `(declare (ignore ,sym)))
                            (let ,(rest->binding rest)
                              (setq ,s ,(pattern->cons s1)
                                    ,e ,(pattern->cons e1)
                                    ,c ,c1
                                    ,d ,(pattern->cons d1))
                              (go :loop)))))))))
       (values 'stop ,s))))

;; match version
(defmacro def-secd-machine (name doc &rest body)
  (let ((s (gensym "S "))
        (e (gensym "E "))
        (c (gensym "C "))
        (d (gensym "D "))
        (insn (gensym "insn")))
    `(defun ,name (,s ,e ,c ,d)
       ,doc
       (tagbody
        :loop
          ;; (format t ";; s: ~s e: ~s c: ~s d: ~s~%" ,s ,e ,c ,d)
          (if (and (consp ,c) (instruction-p (car ,c)))
              (case (car ,c)
                ,@(loop :for rule :in body
                     :collect
                     (destructuring-bind (s0 e0 c0 d0 arrow s1 e1 c1 d1 &rest rest) rule
                       (declare (ignore arrow))
                       `(,(car c0)
                          (match (list ,s ,e ,c ,d)
                            ((,s0 ,e0 ,(cons insn (cdr c0)) ,d0)
                             (declare (ignore ,insn))
                             ,@(loop :for sym :in
                                  (ignorable-sym (list s0 e0 c0 d0) (list s1 e1 c1 d1 (rest->syms rest)))
                                  :collect `(declare (ignore ,sym)))
                             (let ,(rest->binding rest)
                               ;; psetq is better ?
                               (psetq ,s ,(pattern->cons s1)
                                      ,e ,(pattern->cons e1)
                                      ,c ,c1
                                      ,d ,(pattern->cons d1))
                               (go :loop))))
                          ))))))
       (values 'stop ,s))))


(def-secd-machine secd-0 "secd machine sample."
  ;; initial state -> transformed state
  ( s e (:NIL . c) d                         -> (nil . s) e c d )
  ( s e (:LDC x . c) d                       -> (x . s) e c d )
  ( (a b . s) e (:+ . c) d                   -> (x . s) e c d where x = (+ a b) )
  ( (a b . s) e (:- . c) d                   -> (x . s) e c d where x = (- a b) )
  ( (a b . s) e (:* . c) d                   -> (x . s) e c d where x = (* a b) )
  )

;;; TODO: treat boolean 
;;; (if x cT cF) may has bug.

(def-secd-machine secd-1 "secd machine sample 2."
    ;; initial state -> transformed state
    ( s e (:NIL . c) d                         -> (nil . s) e c d )
    ( s e (:LDC x . c) d                       -> (x . s) e c d )
    ( s e (:LD (m . n) . c) d                  -> (x . s) e c d where x = (locate m n e) )
    ( s e (:LDF |c'| . c) d                    -> ((|c'| . e) . s) e c d )
    ( (x . s) e (:SEL cT cF . c) d             -> s e cX (c . d) where cX = (if x cT cF) )
    ( s e (:JOIN . c) (cr . d)                 -> s e cr d )
    ( ((|c'| . |e'|) v . s) e (:AP . c) d      -> nil (v . |e'|) |c'| (s e c . d) )
    ( (a b . s) e (:CONS . c) d                -> ((a . b) . s) e c d )
    ( ((a . b) . s) e (:CAR . c) d             -> (a . s) e c d )
    ( ((a . b) . s) e (:CDR . c) d             -> (b . s) e c d )
    ( (x . z) |e'| (:RTN . |c'|) (s e c . d)   -> (x . s) e c d )
    ( (a b . s) e (:+ . c) d                   -> (x . s) e c d where x = (+ a b) )
    ( (a b . s) e (:- . c) d                   -> (x . s) e c d where x = (- a b) )
    ( (a b . s) e (:* . c) d                   -> (x . s) e c d where x = (* a b) )
    ( (a b . s) e (:= . c) d                   -> (x . s) e c d where x = (= a b) )
    ( (a b . s) e (:> . c) d                   -> (x . s) e c d where x = (> a b) )
    ( (a b . s) e (:< . c) d                   -> (x . s) e c d where x = (< a b) )
    )

(defun run-secd-1 (exp)
  (let ((c (secd.compile:compile-pass1 exp ())))
    (format t ";; code: ~a~%" c)
    (secd-1 's 'e c 'd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-secd-machine-vec (name doc &rest body)
  (let ((s (gensym "S "))
        (e (gensym "E "))
        (pc (gensym "PC ")) ;; PC
        (d (gensym "D "))
        (code (gensym "CODE ")) ;; compiled vector code
        (insn (gensym "insn"))
        (c (gensym "C ")))
    `(defun ,name (,s ,e ,pc ,d ,code)
       ,doc
       (format t ";; code: ~s~%" ,code)
       (tagbody
        :loop
          ;;(format t ";; s: ~s e: ~s pc: ~s d: ~s ~%" ,s ,e ,pc ,d)
          (when (and (>= ,pc 0) (< ,pc (length ,code)) (instruction-p (svref ,code ,pc)))
            (let ((,c (coerce (subseq ,code ,pc) 'list)))
              ;;(format t ";;   c: ~s~%" ,c)
              (case (car ,c)
                ,@(loop :for rule :in body
                     :collect
                     (destructuring-bind (s0 e0 c0 d0 pc0 arrow s1 e1 c1 d1 pc1 &rest rest) rule
                       (declare (ignore arrow))
                       `(,(car c0)
                          (match (list ,s ,e ,pc ,d ,c)
                            ((,s0 ,e0 ,pc0 ,d0 ,(cons insn (cdr c0)))
                             ;;(format t ";;;; match insn:~a~%" ',c0)
                             (declare (ignore ,insn))
                             ;; ,@(loop :for sym :in
                             ;;      (ignorable-sym (list s0 e0 c0 d0) (list s1 e1 c1 d1 (rest->syms rest)))
                             ;;      :collect `(declare (ignore ,sym)))
                             (let ,(rest->binding rest)
                               ;; psetq is better ?
                               (psetq ,s ,(pattern->cons s1)
                                      ,e ,(pattern->cons e1)
                                      ,pc ,pc1
                                      ,d ,(pattern->cons d1))
                               (go :loop)))))))))))
       (values 'stop ,s))))


(def-secd-machine-vec secd-v1 "secd machine sample 2."
    ( s e (:NIL . c) d pc                        -> (nil . s) e c d (+ pc 1))
    ( s e (:STOP . c) d pc                       -> s e c d -1)
    ( s e (:LDC x . c) d pc                      -> (x . s) e c d (+ pc 2))
    ( s e (:LD m n . c) d pc                     -> (x . s) e c d (+ pc 3) where x = (locate m n e) )

    ( s e (:LDF f |c'| . c) d pc                 -> ((f . e) . s) e c d |c'|)

    ( ((|c'| . |e'|) v . s) e (:AP . c) d pc     -> nil (v . |e'|) |c'| (s e |pc'| . d) |c'| where |pc'| = (+ pc 1))

    ( (x . z) |e'| (:RTN . |c'|) (s e |pc'| . d) pc -> (x . s) e c d |pc'|)

    ( (x . s) e (:SEL cT cF cont . c) d pc            -> s e cX (cont . d) cX where cX = (if x cT cF) )
    ( s e (:JOIN . c) (cr . d) pc                -> s e c d cr )

    ( (a b . s) e (:CONS . c) d pc               -> ((a . b) . s) e c d (+ pc 1))
    ( ((a . b) . s) e (:CAR . c) d pc            -> (a . s) e c d (+ pc 1))
    ( ((a . b) . s) e (:CDR . c) d pc            -> (b . s) e c d (+ pc 1))

    ( (a b . s) e (:+ . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (+ a b) )
    ( (a b . s) e (:- . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (- a b) )
    ( (a b . s) e (:* . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (* a b) )
    ( (a b . s) e (:= . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (= a b) )
    ( (a b . s) e (:> . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (> a b) )
    ( (a b . s) e (:< . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (< a b) )
    )


(def-secd-machine-vec secd-v0 "secd machine sample."
  ;; initial state -> transformed state
  ( s e (:LDC x . c) d pc                    -> (x . s) e c d (+ pc 2) )
  ( (a b . s) e (:+ . c) d pc                -> (x . s) e c d (+ pc 1) where x = (+ a b) )
  ( (a b . s) e (:- . c) d pc                -> (x . s) e c d (+ pc 1) where x = (- a b) )
  ( (a b . s) e (:* . c) d pc                -> (x . s) e c d (+ pc 1) where x = (* a b) )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NOW

(def-secd-machine-vec secd-v00 "secd machine sample 2."
    ;; initial state -> transformed state
    ( s e (:NIL . c) d pc                        -> (nil . s) e c d (+ pc 1))
    ( s e (:STOP . c) d pc                       -> s e c d -1)
    ( s e (:LDC x . c) d pc                      -> (x . s) e c d (+ pc 2))
    ( s e (:LD (m . n) . c) d pc                 -> (x . s) e c d (+ pc 3) where x = (locate m n e) )
    ( s e (:LDF f |c'| . c) d pc                 -> ((f . e) . s) e c d |c'|) ;;
    ( ((|c'| . |e'|) v . s) e (:AP . c) d pc     -> nil (v . |e'|) |c'| (s e pc . d) |c'| )
    ( (x . z) |e'| (:RTN . |c'|) (s e pc . d) |pc'|  -> (x . s) e c d pc)
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



