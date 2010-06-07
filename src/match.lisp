(in-package :secd.match)

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
     `((eql ,x ',(cadr pattern))))
    ((eql pattern :_) ;; match every thing
     `(t))
    ((keywordp pattern)
     `((eql ,x ,pattern)))
    ((consp pattern)
     `((consp ,x) ,@(pattern->predicate (car pattern) `(car ,x))
       ,@(pattern->predicate (cdr pattern) `(cdr ,x))))
    (t
     (error "pattern->predicate: unknown pattern: ~a" pattern))))

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
    ((atom pattern)
     `((,pattern ,x)))
    ((single? pattern)
     `(,@(pattern->binding (car pattern) `(car ,x))))
    ((consp pattern)
     `(,@(pattern->binding (car pattern) `(car ,x))
         ,@(pattern->binding (cdr pattern) `(cdr ,x))))
    (t
     (error "pattern->binding: unknown pattern: ~a" pattern))))

(defmacro match (expr &rest pattern-body)
  (let ((x (gensym "x")))
    `(let ((,x ,expr))
       (cond
         ,@(loop :for (pattern . body) :in pattern-body
              :collect
              `((and ,@(pattern->predicate pattern x))
                (let ,(pattern->binding pattern x)
                  ,@body)))))))



