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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; (def-secd-machine-vec secd-v00 "secd machine sample 2."
;;     ;; initial state -> transformed state
;;     ( s e (:NIL . c) d pc                        -> (nil . s) e c d (+ pc 1))
;;     ( s e (:STOP . c) d pc                       -> s e c d -1)
;;     ( s e (:LDC x . c) d pc                      -> (x . s) e c d (+ pc 2))
;;     ( s e (:LD (m . n) . c) d pc                 -> (x . s) e c d (+ pc 3) where x = (locate m n e) )
;;     ( s e (:LDF f |c'| . c) d pc                 -> ((f . e) . s) e c d |c'|) ;;
;;     ( ((|c'| . |e'|) v . s) e (:AP . c) d pc     -> nil (v . |e'|) |c'| (s e pc . d) |c'| )
;;     ( (x . z) |e'| (:RTN . |c'|) (s e pc . d) |pc'|  -> (x . s) e c d pc)
;;     )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-secd-machine-vec+ secd-v2 "secd machine sample 2."
  ( s e (:NIL . c) d pc                        -> (nil . s) e c d (+ pc 1))
  ( s e (:STOP . c) d pc                       -> s e c d -1)
  ( s e (:LDC x . c) d pc                      -> (x . s) e c d (+ pc 2))
  ( s e (:LD m n . c) d pc                     -> (x . s) e c d (+ pc 3) where x = (locate m n e) )

  ;; ( s e (:LDF f |c'| . c) d pc                 -> ((f . e) . s) e c d |c'|)

  ;; ( ((|c'| . |e'|) v . s) e (:AP . c) d pc     -> nil (v . |e'|) |c'| (s e |pc'| . d) |c'| where |pc'| = (+ pc 1))

  ;; ( (x . z) |e'| (:RTN . |c'|) (s e |pc'| . d) pc -> (x . s) e c d |pc'|)

  ;; ( (x . s) e (:SEL cT cF cont . c) d pc            -> s e cX (cont . d) cX where cX = (if x cT cF) )
  ;; ( s e (:JOIN . c) (cr . d) pc                -> s e c d cr )

  ;; ( (a b . s) e (:CONS . c) d pc               -> ((a . b) . s) e c d (+ pc 1))
  ;; ( ((a . b) . s) e (:CAR . c) d pc            -> (a . s) e c d (+ pc 1))
  ;; ( ((a . b) . s) e (:CDR . c) d pc            -> (b . s) e c d (+ pc 1))

  ;; ( (a b . s) e (:+ . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (+ a b) )
  ;; ( (a b . s) e (:- . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (- a b) )
  ;; ( (a b . s) e (:* . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (* a b) )
  ;; ( (a b . s) e (:= . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (= a b) )
  ;; ( (a b . s) e (:> . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (> a b) )
  ;; ( (a b . s) e (:< . c) d pc                  -> (x . s) e c d (+ pc 1) where x = (< a b) )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; match
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def-secd-machine secd-0 "secd machine sample."
;;   ;; initial state -> transformed state
;;   ( s e (:NIL . c) d                         -> (nil . s) e c d )
;;   ( s e (:LDC x . c) d                       -> (x . s) e c d )
;;   ( (a b . s) e (:+ . c) d                   -> (x . s) e c d where x = (+ a b) )
;;   ( (a b . s) e (:- . c) d                   -> (x . s) e c d where x = (- a b) )
;;   ( (a b . s) e (:* . c) d                   -> (x . s) e c d where x = (* a b) )
;;   )
;; (defun cl-match-secd (exp)
;;   (let ((s0 's) (e0 'e) (c0 (secd.compile::compile-pass1 exp ())) (d0 'd))
;;     (format t ";; c: ~s~%" c0)
;;     (tagbody
;;      :loop
;;        (match (list s0 e0 c0 d0)
;;          ((list s e (:NIL . c1) d)
;;           (psetq s0 (cons nil s)
;;                  c0 c1)
;;           (goto :loop))
;;          ((list s e (:LDC x . c1) d)
;;           (psetq s0 (cons x s)
;;                  c0 c1)
;;           (goto :loop))
;;          ((list s e c d)
;;           (format t "end: ~a~%" (list s e c d)))))))

