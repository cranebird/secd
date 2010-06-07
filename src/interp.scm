(use gauche.sequence)
(use util.match)

(define vm-compiler-flag-set!
  (with-module gauche.internal vm-compiler-flag-set!))
(define vm-compiler-flag-clear!
  (with-module gauche.internal vm-compiler-flag-clear!))

(define opt-off
  (lambda ()
    (vm-compiler-flag-clear!
     (with-module gauche.internal SCM_COMPILE_NOINLINE_CONSTS))
    (vm-compiler-flag-clear!
     (with-module gauche.internal SCM_COMPILE_NOINLINE_GLOBALS))
    (vm-compiler-flag-clear!
     (with-module gauche.internal SCM_COMPILE_NOINLINE_LOCALS))))

(define compile (with-module gauche.internal compile))
(define vm-code->list (with-module gauche.internal vm-code->list))

(define compiled-code?
  (lambda (obj)
    (is-a? obj <compiled-code>)))

(define vm-code-list->vector
  (lambda (lst)
    (list->vector (map (lambda (x)
                         (cond
                          ((identifier? x)
                           (identifier->symbol x))
                          ((pair? x)
                           (cons (make-keyword (car x)) (cdr x)))
                          ((compiled-code? x)
                           (vm-code-list->vector (vm-code->list x)))
                          (else
                           x)))
                       lst))))

(define compile->vector
  (lambda (exp)
    (vm-code-list->vector (vm-code->list (compile exp ())))))

(define lookup-global
  (lambda (var)
    ;; todo
    #f))

(define flatten
  (lambda (x)
    (define rec
      (lambda (a acc)
        (cond
         ((null? a) acc)
         ((not (pair? a)) (cons a acc))
         (else
          (rec (car a) (rec (cdr a) acc))))))
    (rec x ())))

(define secd
  (lambda (exp)
    (let ((code (compile->vector exp)))
      (print code)
      (let vmloop ((s 's) (e 'e) (pc 0) (d 'd) (val 'val))
        (let ((c (subseq code pc)))
          (format #t ";; s: ~s e: ~s c: ~s d: ~s val: ~s pc: ~s~%"
                  s e (subseq c 0 1) d val pc)
          (match (list s e c d val)
            ((s e #((:CONST) n _ ...) d val)
             (vmloop s e (+ pc 2) d n))
            ((s e #((:CONSTI n) _ ...) d val)
             (vmloop s e (+ pc 1) d n))
            ((s e #((:CONSTI-PUSH n) _ ...) d val)
             (vmloop (cons n s) e (+ pc 1) d val))

            ((s e #((:GREF) var rest ...) d val)
             (vmloop s e (+ pc 2) d (lookup-global var)))

            ((s e #((:PUSH) _ ...) d val)
             (vmloop (cons val s) e (+ pc 1) d val))
            ((s e #((:NUMADDI num) _ ...) d val)
             (vmloop s e (+ pc 1) d (+ num val)))
            ((s e #((:NUMSUBI num) _ ...) d val)
             (vmloop s e (+ pc 1) d (- num val)))
            (((v . s) e #((:NUMMUL2) _ ...) d val)
             (vmloop s e (+ pc 1) d (* v val)))

            (((a . s) e #((:BNGT) then _ ...) d val)
             (if (> val a)
                 (vmloop s e then d val)
                 (vmloop s e (+ pc 2) d val)))

            ((s e #((:CLOSURE) body _ ...) d val) ;; todo
             (vmloop () e (+ pc 2) d (cons e body)))

            ((s e #((:PRE-CALL procedure-id) location _ ...) d val)
             (vmloop s e (+ pc 2) (cons (cons location  val) d) val))

            ((s e #((:RET) rest ...) d val)
             ;; todo
             #f
             )

            ((s e c d val)
             (format #t ";; base case: s: ~s e: ~s c: ~s d: ~s val: ~s pc: ~s~%"
                     s e (subseq c 0 1) d val pc))
            ))))))


(define m 
  (macroexpand 
   '(match (list s e c d val)
      ((s e #((:CONST) n rest ...) d val)
       (vmloop s e (+ pc 2) d n))
      ((s e #((:CONSTI n) rest ...) d val)
       (vmloop s e (+ pc 1) d n))
      ((s e #((:CONSTI-PUSH n) rest ...) d val)
       (vmloop (cons n s) e (+ pc 1) d val))

      ((s e #((:GREF) var rest ...) d val)
       (vmloop s e (+ pc 2) d (lookup-global var)))

      ((s e #((:PUSH) rest ...) d val)
       (vmloop (cons val s) e (+ pc 1) d val))
      ((s e #((:NUMADDI num) rest ...) d val)
       (vmloop s e (+ pc 1) d (+ num val)))
      ((s e #((:NUMSUBI num) rest ...) d val)
       (vmloop s e (+ pc 1) d (- num val)))
      (((v . s) e #((:NUMMUL2) rest ...) d val)
       (vmloop s e (+ pc 1) d (* v val)))

      (((a . s) e #((:BNGT) then rest ...) d val)
       (if (> val a)
           (vmloop s e then d val)
           (vmloop s e (+ pc 2) d val)))

      ((s e #((:CLOSURE) body rest ...) d val) ;; todo
       (vmloop () e (+ pc 2) d (cons e body)))

      ((s e #((:PRE-CALL procedure-id) location rest ...) d val)
       (vmloop s e (+ pc 2) (cons (cons location  val) d) val))

      ((s e #((:RET) rest ...) d val)
       ;; todo
       #f
       )

      ((s e c d val)
       (format #t ";; base case: s: ~s e: ~s c: ~s d: ~s val: ~s pc: ~s~%"
               s e (subseq c 0 1) d val pc))
      )))

(define m_
  (macroexpand 
   '(match (list s e c d val)
      ((s e #((:CONST) n _ ...) d val)
       (vmloop s e (+ pc 2) d n))
      ((s e #((:CONSTI n) _ ...) d val)
       (vmloop s e (+ pc 1) d n))
      ((s e #((:CONSTI-PUSH n) _ ...) d val)
       (vmloop (cons n s) e (+ pc 1) d val))

      ((s e #((:GREF) var _ ...) d val)
       (vmloop s e (+ pc 2) d (lookup-global var)))

      ((s e #((:PUSH) _ ...) d val)
       (vmloop (cons val s) e (+ pc 1) d val))
      ((s e #((:NUMADDI num) _ ...) d val)
       (vmloop s e (+ pc 1) d (+ num val)))
      ((s e #((:NUMSUBI num) _ ...) d val)
       (vmloop s e (+ pc 1) d (- num val)))
      (((v . s) e #((:NUMMUL2) _ ...) d val)
       (vmloop s e (+ pc 1) d (* v val)))

      (((a . s) e #((:BNGT) then _ ...) d val)
       (if (> val a)
           (vmloop s e then d val)
           (vmloop s e (+ pc 2) d val)))

      ((s e #((:CLOSURE) body _ ...) d val) ;; todo
       (vmloop () e (+ pc 2) d (cons e body)))

      ((s e #((:PRE-CALL procedure-id) location _ ...) d val)
       (vmloop s e (+ pc 2) (cons (cons location  val) d) val))

      ((s e #((:RET) _ ...) d val)
       ;; todo
       #f
       )

      ((s e c d val)
       (format #t ";; base case: s: ~s e: ~s c: ~s d: ~s val: ~s pc: ~s~%"
               s e (subseq c 0 1) d val pc))
      )))