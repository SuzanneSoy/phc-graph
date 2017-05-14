#lang type-expander
(require (lib "phc-graph/flexible-with-generalized-ctor.hl.rkt"))
(define-type τ-4-2 (builder-τ 4 2))
#|
(: f τ-4-2)
(define (f kx x ky y)
  (error "Not Yet"))
(define-syntax-rule (F KX X KY Y)
  (inst f propagate-τ
        KX X KY Y))
(ann (F 0 Number 1 String)
     (-> 0 Number 1 String
         (List
          (Pairof Any (Some Number))
          (Pairof Any (Some String))
          (Pairof Any (None (List Zero One)))
          (Pairof Any (None (List Zero One))))))
|#

#|
(: g (∀ (A) (case→ [→ (Some A) A]
                   [→ (None Any) 'none])))
(define (g a)
  (if (Some? a)
      (Some-f a)
      'none))
|#