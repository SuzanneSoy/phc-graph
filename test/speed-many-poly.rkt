#lang type-expander

(require (for-syntax racket))

;; This seems to be a slow-starting exponential, with a factor of ×2.5
;; each time n is increased by 100.
;; n=500 takes nearly 3 minutes, n=1000 should, by projection, take 4.5 hours.
(define-for-syntax n 200)

(: f ((Λ (_)
        (with-syntax ([(T ...)
                       (map (λ (i) (gensym 'τ)) (range n))])
          #'(∀ (A B T ...)
               (→ (List A T ...) B (List B T ...)))))))
(define (f l v)
  (cons v (cdr l)))

(define-syntax (callf stx)
  (with-syntax ([(v ...) (range n)])
    #'(f (list "a" v ...) 'x)))

(define cf (callf))