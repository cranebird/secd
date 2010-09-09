(in-package :secd.util)

;; (quote obj)
(defun quoted-symbol-p (x)
  "return t when x is '(quote obj)"
  (and (consp x) (eql (car x) 'quote) (single? (cdr x))))

(defun pattern->predicate (pattern x)
  (cond
    ((null pattern)
     `((null ,x)))
    ((numberp pattern)
     `((eql ,x ,pattern)))
    ((quoted-symbol-p pattern)
     `((equal ,x ',(cadr pattern))))
    ((eql pattern :_) ;; match every thing
     `(t))
    ((keywordp pattern)
     `((eql ,x ,pattern)))
    ((consp pattern)
     `((consp ,x) ,@(pattern->predicate (car pattern) `(car ,x))
       ,@(pattern->predicate (cdr pattern) `(cdr ,x))))
    ((eql pattern t)
     nil)
    (t
     nil)))

(defun pattern->binding (pattern x)
  (cond
    ((null pattern)
     nil)
    ((numberp pattern)
     nil)
    ((quoted-symbol-p pattern)
     nil)
    ((keywordp pattern)
     nil)
    ((eql pattern t)
     nil)
    ((atom pattern)
     `((,pattern ,x)))
    ((single? pattern)
     `(,@(pattern->binding (car pattern) `(car ,x))))
    ((consp pattern)
     `(,@(pattern->binding (car pattern) `(car ,x))
         ,@(pattern->binding (cdr pattern) `(cdr ,x))))))

(defun validate-pattern (pattern-body)
  (let ((ht (make-hash-table :test #'equal))
        (x (gensym "x")))
    (loop :for (pattern . body) :in pattern-body
       :do
       (multiple-value-bind (val present-p)
           (gethash (pattern->predicate pattern x) ht)
         (if present-p
             (error "~%same pattern found: ~s~%~s" pattern val)
             (setf (gethash (pattern->predicate pattern x) ht) pattern))))))

(defmacro match (expr &rest pattern-body)
  (validate-pattern pattern-body)
  (let ((x (gensym "x")))
    `(let ((,x ,expr))
       (cond
         ,@(loop :for (pattern . body) :in pattern-body
              :collect
              `((and ,@(pattern->predicate pattern x))
                (let (,@(pattern->binding pattern x))
                  ,@body)))))))

(defun pattern-n->predicate-n (pattern vs)
  `(and ,@(loop :for p :in pattern :for v :in vs
             :append (pattern->predicate p v))))

(defmacro match-n ((&rest es) &body pattern-body)
  ;;(validate-pattern pattern-body)
  `(cond
     ,@(loop :for (pattern . body) :in pattern-body
          :collect
          `((,@(pattern-n->predicate-n pattern es))
            (let ,(loop :for p :in pattern :for v :in es
                     :append (pattern->binding p v))
              ,@body)))))

