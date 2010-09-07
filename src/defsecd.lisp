(in-package :secd.interp)

(deftransition secd% (s e c d)
  (:transitions

   ( s e (:LDC x . c) d                       -> (x . s) e c d )
   ( s e (:NIL . c) d                         -> (nil . s) e c d )

   ( (a b . s) e (:+ . c) d                   -> (x . s) e c d where x = (+ a b) )
   ( (a b . s) e (:- . c) d                   -> (x . s) e c d where x = (- a b) ))
  (:last-value
   (lambda (s e c d) (car s)))
  ;; (:auxiliary
  ;;  ((locate (m n e)
  ;;           (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;;           (declare (type fixnum m n ))
  ;;           (nth (the fixnum (1- n)) (nth (the fixnum (1- m)) e)))))
  )

(deftransition secd%% (s e c d)
  (:transitions
   ( s e (:NIL . c) d                         -> (nil . s) e c d )
   ( s e (:LDC x . c) d                       -> (x . s) e c d )
   ( (a b . s) e (:+ . c) d                   -> (x . s) e c d where x = (+ a b) )
   ( (a b . s) e (:- . c) d                   -> (x . s) e c d where x = (- a b) ))
  (:last-value
   (lambda (s e c d) (car s)))
  )

(deftransition secd%%% (s e c d)
  (:transitions
   ( s e (:NIL . c) d                            -> (nil . s) e c d )
   ( s e (:LDC x . c) d                          -> (x . s) e c d )
   ( s e (:LD (m . n) . c) d                     -> (x . s) e c d where x = (locate m n e) )
   ( s e (:LDF |c'| . c) d                       -> ((|c'| . e) . s) e c d )
   ( (x . s) e (:SEL cT cF . c) d                -> s e cX (c . d) where cX = (if x cT cF) )
   ( s e (:JOIN . c) (cr . d)                    -> s e cr d )
   ( ((|c'| . |e'|) v . s) e (:AP . c) d         -> nil (v . |e'|) |c'| (s e c . d) )
   ( (a b . s) e (:CONS . c) d                   -> ((a . b) . s) e c d )
   ( ((a . b) . s) e (:CAR . c) d                -> (a . s) e c d )
   ( ((a . b) . s) e (:CDR . c) d                -> (b . s) e c d )
   ( (x . z) |e'| (:RTN . |c'|) (s e c . d)      -> (x . s) e c d )
   ( s e (:DUM . c) d                            -> s (nil . e) c d)
   (((|c'| . |e'|) v . s) (nil . e) (:RAP . c) d -> nil |e''| |c'| (s e c . d) where |e''| = (rplaca |e'| v))
   ( (a b . s) e (:+ . c) d                      -> (x . s) e c d where x = (+ a b) )
   ( (a b . s) e (:- . c) d                      -> (x . s) e c d where x = (- a b) )
   ( (a b . s) e (:* . c) d                      -> (x . s) e c d where x = (* a b) )
   ( (a b . s) e (:= . c) d                      -> (x . s) e c d where x = (= a b) )
   ( (a b . s) e (:> . c) d                      -> (x . s) e c d where x = (> a b) )
   ( (a b . s) e (:< . c) d                      -> (x . s) e c d where x = (< a b) ))
  (:last-value
   (lambda (s e c d) (car s)))
  )

(deftransition secd (s e c d)
  (:transitions
   ( s e (:ECHO x . c) d                         -> s e c d where z = (format t ";;ECHO: ~a~%" x))
   ( s e (:NIL . c) d                            -> (nil . s) e c d )
   ( s e (:LDC x . c) d                          -> (x . s) e c d )
   ( s e (:LD (m . n) . c) d                     -> (x . s) e c d where x = (locate m n e) )

   ( s e (:LDF |c'| . c) d                       -> ((:clos |c'| . e) . s) e c d )

   ( (x . s) e (:SEL cT cF . c) d                -> s e cX (c . d) where cX = (if x cT cF) )
   ( (x . s) e (:SELR cT cF) d                   -> s e cX d where cX = (if x cT cF) )
   ( s e (:JOIN . c) (cr . d)                    -> s e cr d )
   ( ((:clos |c'| . |e'|) v . s) e (:TAP) d  -> s (v . |e'|) |c'| d )

   ( ((:cont s e c . d) (v) . |s'|) |e'| (:TAP) |d'| -> (v . s) e c d )

   ( ((:clos |c'| . |e'|) v . s) e (:AP . c) d   -> nil (v . |e'|) |c'| (s e c . d) )

   ( ((:cont s e c . d) (v) . |s'|) |e'| (:AP . |c'|) |d'| -> (v . s) e c d)

   ( s e (:LDCT |c'| . c) d                      -> ( ((:cont s e |c'| . d)) . s) e c d )

   ( (b a . s) e (:CONS . c) d                   -> ((a . b) . s) e c d )

   ( ((a . b) . s) e (:CAR . c) d                -> (a . s) e c d )
   ( ((a . b) . s) e (:CDR . c) d                -> (b . s) e c d )
   ( (x . s) e (:CONSP . c) d                    -> (p . s) e c d where p = (consp x) )


   ( (x . z) |e'| (:RTN . |c'|) (s e c . d)      -> (x . s) e c d )
   ( s e (:DUM . c) d                            -> s (nil . e) c d)
   
   ( ((:clos |c'| . |e'|) v . s) (nil . e) (:RAP . c) d -> nil |e''| |c'| (s e c . d) where |e''| = (rplaca |e'| v))

   ( ((:clos |c'| . |e'|) v . s) (nil . e) (:RTAP . c) d -> nil |e''| |c'| d where |e''| = (rplaca |e'| v))
   
   ( (b a . s) e (:+ . c) d                      -> (x . s) e c d where x = (+ a b) )
   ( (b a . s) e (:- . c) d                      -> (x . s) e c d where x = (- a b) )
   ( (b a . s) e (:* . c) d                      -> (x . s) e c d where x = (* a b) )
   ( (b a . s) e (:= . c) d                      -> (x . s) e c d where x = (= a b) )
   ( (b a . s) e (:> . c) d                      -> (x . s) e c d where x = (> a b) )
   ( (b a . s) e (:< . c) d                      -> (x . s) e c d where x = (< a b) )
   ( (b a . s) e (:mod . c) d                    -> (x . s) e c d where x = (mod a b) )
   
   ( (v . s) e (:VLEN . c) d                     -> (x . s) e c d where x = (length v))
   ( (l . s) e (:L2V . c) d                      -> (v . s) e c d where v = (make-vector l))
   ( (v n . s) e (:VREF . c) d                   -> (x . s) e c d where x = (aref v n))
   ( (v n x . s) e (:VSET . c) d                 -> (v . s) e c d where v = (progn (setf (aref v n) x) v))
   
   ( (x . s) e (:WRITE . c) d                    -> s e c d where dum = (format t ";; ~s~%" x) ))
  (:last-value
   (lambda (s e c d) (car s)))
  )

;; todo
;; (defsecd foo4 (s e c d val pc)
;;   "secd stack machine sample."
;;   ( s e ((:CONST) n . c) d  val pc            -> s e c d n pc2 where pc2 = (+ pc 2) )
;;   ( s e ((:PUSH) . c) d  val pc               -> (val . s) e c d val pc1 where pc1 = (+ pc 1 ))
;;   )
