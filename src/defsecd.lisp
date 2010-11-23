(in-package :secd)

(deftransition secd%% (s e c d)
  (:transitions
   ( s e (:LDC x . c) d                       -> (x . s) e c d )
   ;;( s e (:LDC x . c) d                       -> (m . s) e c d )
   )
  (:last-value
   (lambda (s e c d) (car s))))

(deftransition secd% (s e c d)
  (:transitions
   ( s e (:LDC x . c) d                       -> (x . s) e c d )
   ( s e (:NIL . c) d                         -> (nil . s) e c d )
   ( (a b . s) e (:+ . c) d                   -> (x . s) e c d where x = (+ a b) )
   ( (a b . s) e (:- . c) d                   -> (x . s) e c d where x = (- a b) )
   )
  (:last-value
   (lambda (s e c d) (car s))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftransition secd (s e c d)
  (:transitions
   ( s e (:NIL . c) d                            -> (nil . s) e c d )
   ( (x . s) e (:POP . c) d                      -> s e c d )
   ( s e (:LD (m . n) . c) d                     -> (x . s) e c d where x = (locate m n e) )
   ( s e (:LDC x . c) d                          -> (x . s) e c d )
   ( s e (:LDF |c'| . c) d                       -> ((:clos |c'| . e) . s) e c d )

   ( ((:clos |c'| . |e'|) v . s) e (:AP . c) d   -> nil (v . |e'|) |c'| (s e c . d) )

   ( ((:cont s e c . d) (v) . |s'|) |e'| (:AP . |c'|) |d'| -> (v . s) e c d)

   ( ((:clos |c'| . |e'|) v . s) e (:TAP) d      -> s (v . |e'|) |c'| d )
   ( ((:cont s e c . d) (v) . |s'|) |e'| (:TAP) |d'| -> (v . s) e c d )

   ( (x . z) |e'| (:RTN . |c'|) (s e c . d)      -> (x . s) e c d )

   ( (x . s) e (:SEL cT cF . c) d                -> s e cX (c . d) where cX = (if (not (eq x #f)) cT cF) )
   ( (x . s) e (:SELR cT cF) d                   -> s e cX d where cX = (if (not (eq x #f)) cT cF) )
   ( s e (:JOIN . c) (cr . d)                    -> s e cr d )
   ( ((:clos |c'| . |e'|) v . s) (nil . e) (:RAP . c) d -> nil |e''| |c'| (s e c . d) where |e''| = (rplaca |e'| v))
   ( s e (:DUM . c) d                            -> s (nil . e) c d)

   ( s e (:LDCT |c'| . c) d                      -> ( ((:cont s e |c'| . d)) . s) e c d )

   ( (b a . s) e (:CONS . c) d                   -> ((b . a) . s) e c d )
   ( ((a . b) . s) e (:CAR . c) d                -> (a . s) e c d )
   ( ((a . b) . s) e (:CDR . c) d                -> (b . s) e c d )
   ( (x . s) e (:CONSP . c) d                    -> (p . s) e c d where p = (if (consp x) #t #f) )
   ( ((:clos |c'| . |e'|) v . s) (nil . e) (:RTAP . c) d -> nil |e''| |c'| d where |e''| = (rplaca |e'| v))
   ( (x . s) e (:SET (m . n) . c) d              -> s |e'| c d where |e'| = (progn (setf (locate m n e) x) e))
   ( (a b . s) e (:+ . c) d                      -> (x . s) e c d where x = (+ a b) )
   ( (a b . s) e (:- . c) d                      -> (x . s) e c d where x = (- a b) )
   ( (a b . s) e (:* . c) d                      -> (x . s) e c d where x = (* a b) )
   ( (a b . s) e (:= . c) d                      -> (x . s) e c d where x = (if (= a b) #t #f) )
   ( (a b . s) e (:> . c) d                      -> (x . s) e c d where x = (if (> a b) #t #f) )
   ( (a b . s) e (:>= . c) d                     -> (x . s) e c d where x = (if (>= a b) #t #f) )
   ( (a b . s) e (:< . c) d                      -> (x . s) e c d where x = (if (< a b) #t #f) )
   ( (a b . s) e (:<= . c) d                     -> (x . s) e c d where x = (if (<= a b) #t #f) )
   ( (a b . s) e (:mod . c) d                    -> (x . s) e c d where x = (mod a b) )
   ( (v . s) e (:VLEN . c) d                     -> (x . s) e c d where x = (length v))
   ( (l . s) e (:L2V . c) d                      -> (v . s) e c d where v = (make-vector l))
   ( (v n . s) e (:VREF . c) d                   -> (x . s) e c d where x = (aref v n))
   ( (v n x . s) e (:VSET . c) d                 -> (v . s) e c d where v = (progn (setf (aref v n) x) v))
   ( (x . s) e (:WRITE . c) d                    -> s e c d where dum = (format t ";; ~s~%" x) )
   ( s e (:DEBUG . c) d                          -> s e c d where dum = (let ((*print-circle* t))
                                                                          (format t ";; debug:~%S: ~a~%E: ~A~%C: ~a~%D: ~a~%"
                                                                                  s e c d)))
   )
  (:last-value
   (lambda (s e c d) (declare (ignore e c d)) (if (consp s) (car s)))))
