#lang type-expander

(require (for-syntax racket))

;; This seems to be a slow-starting exponential, with a factor of ×2.5
;; each time n is increased by 100.
;; n=500 takes nearly 3 minutes, n=1000 should, by projection, take 4.5 hours.
(define-for-syntax n 200)

(: kons (∀ (A B) (→ A B (Pairof A B))))
(define kons cons)

(: f ((Λ (_)
        (with-syntax ([(T ...)
                       (map (λ (i) (gensym 'τ)) (range n))]
                      [(T₂ ...)
                       (map (λ (i) (gensym 'τ)) (range n))]
                      [(T₃ ...)
                       (map (λ (i) (gensym 'τ)) (range n))])
          #'(∀ (A B T ...)
               (→ (List A T ...)
                  B
                  (∀ (A₂ T₂ ...)
                     (→ (List A₂ T₂ ...)
                        (∀ (A₃ T₃ ...)
                           (→ (List (List A T ...)
                                    (List A₂ T₂ ...)
                                    (List A₃ T₃ ...))
                              (List (List B T ...)
                                    (List B T₂ ...)
                                    (List B T₃ ...))))))))))))
(define (((f l v) ll) alll)
  (list (kons v (cdr (car alll)))
        (kons v (cdr (cadr alll)))
        (kons v (cdr (caddr alll)))))

(define-syntax (callf stx)
  (with-syntax ([(v ...) (range n)]
                [(v₂ ...) (map number->string (range n))]
                [(v₃ ...) (map string->symbol (map number->string (range n)))])
    #'(((f
         (list "a" v ...)
         'x)
        (list "aa" v₂ ...))
       (list (list "a" v ...)
             (list "aa" v₂ ...)
             (list 'aaa 'v₃ ...)))))

(define cf (callf))