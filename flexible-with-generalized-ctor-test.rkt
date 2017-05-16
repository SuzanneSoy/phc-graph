#lang type-expander
(require (lib "phc-graph/flexible-with-generalized-ctor.hl.rkt"))
(define-type τ-4-2 (builder-τ 4 2))

#;(τ (U (Pairof Any (None (Listof (Some Any))))
               (Some Any))
            (Some0 Any) Number (Some1 Any) String)
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













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual good implementation:

;(struct (A) Some ([f : A]) #:transparent)
;(struct (A) Some0 Some () #:transparent)
;(struct (A) Some1 Some () #:transparent)
;(struct (A) Some2 Some () #:transparent)
;(struct (A) Some3 Some () #:transparent)
;(struct (A) None ([f : A]) #:transparent)

;; A is:
#;(Pairof Any (U (Some Any) (None (Listof Any))))

(: f (builder-τ 4 2))

(define (f oracle kx x ky y)
  (list (cond
          [((make-predicate '|0|) kx)
           (ann ((inst oracle (∩ (Pairof 0/K (Some 0/X)) (Pairof '|0| Any))) (cons kx (Some x)))
                (∩ (Pairof 0/K (Some 0/X)) (Pairof '|0| Any) A))]
          [((make-predicate '|0|) ky)
           (ann ((inst oracle (∩ (Pairof 1/K (Some 1/X)) (Pairof '|0| Any))) (cons ky (Some y)))
                (∩ (Pairof 1/K (Some 1/X)) (Pairof '|0| Any) A))]
          [else
           ((inst oracle (Pairof '|0| (None (List (∩ 0/K (U '|1| '|2| '|3|)) (∩ 1/K (U '|1| '|2| '|3|))))))
            (cons '|0| (None (list kx ky))))])
        (cond
          [((make-predicate '|1|) kx)
           (ann ((inst oracle (∩ (Pairof 0/K (Some 0/X)) (Pairof '|1| Any))) (cons kx (Some x)))
                (∩ (Pairof 0/K (Some 0/X)) (Pairof '|1| Any) A))]
          [((make-predicate '|1|) ky)
           (ann ((inst oracle (∩ (Pairof 1/K (Some 1/X)) (Pairof '|1| Any))) (cons ky (Some y)))
                (∩ (Pairof 1/K (Some 1/X)) (Pairof '|1| Any) A))]
          [else
           ((inst oracle (Pairof '|1| (None (List (∩ 0/K (U '|0| '|2| '|3|)) (∩ 1/K (U '|0| '|2| '|3|))))))
            (cons '|1| (None (list kx ky))))])
        (cond
          [((make-predicate '|2|) kx)
           (ann ((inst oracle (∩ (Pairof 0/K (Some 0/X)) (Pairof '|2| Any))) (cons kx (Some x)))
                (∩ (Pairof 0/K (Some 0/X)) (Pairof '|2| Any) A))]
          [((make-predicate '|2|) ky)
           (ann ((inst oracle (∩ (Pairof 1/K (Some 1/X)) (Pairof '|2| Any))) (cons ky (Some y)))
                (∩ (Pairof 1/K (Some 1/X)) (Pairof '|2| Any) A))]
          [else
           ((inst oracle (Pairof '|2| (None (List (∩ 0/K (U '|0| '|1| '|3|)) (∩ 1/K (U '|0| '|1| '|3|))))))
            (cons '|2| (None (list kx ky))))])
        (cond
          [((make-predicate '|3|) kx)
           (ann ((inst oracle (∩ (Pairof 0/K (Some 0/X)) (Pairof '|3| Any))) (cons kx (Some x)))
                (∩ (Pairof 0/K (Some 0/X)) (Pairof '|3| Any) A))]
          [((make-predicate '|3|) ky)
           (ann ((inst oracle (∩ (Pairof 1/K (Some 1/X)) (Pairof '|3| Any))) (cons ky (Some y)))
                (∩ (Pairof 1/K (Some 1/X)) (Pairof '|3| Any) A))]
          [else
           ((inst oracle (Pairof '|3| (None (List (∩ 0/K (U '|0| '|1| '|2|)) (∩ 1/K (U '|0| '|1| '|2|))))))
            (cons '|3| (None (list kx ky))))])))
