(in-package :secd.interp)

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

(def-secd-machine secd-1 "secd machine sample."
  ;; initial state -> transformed state
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
  ( (a b . s) e (:< . c) d                      -> (x . s) e c d where x = (< a b) )
  )

(defun run-secd-1 (exp)
  (let ((c (secd.compile:compile-pass1 exp ())))
    (format t ";; code: ~a~%" c)
    (secd-1 's 'e c 'd)))
