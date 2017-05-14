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

(struct (A) Some ([f : A]) #:transparent)
(struct (A) Some0 Some () #:transparent)
(struct (A) Some1 Some () #:transparent)
(struct (A) Some2 Some () #:transparent)
(struct (A) Some3 Some () #:transparent)
(struct (A) None ([f : A]) #:transparent)

;; A is:
#;(U (Pairof Any (None (Listof (Some Any))))
     (Some Any))

(struct |0| ())
(struct |1| ())
(struct |2| ())
(struct |3| ())

(: f (∀ (A 0/K 0/X 1/K 1/X)
        (→ (∩ 0/K (U |0| |1| |2| |3|))
           0/X
           (∩ 1/K (U |0| |1| |2| |3|))
           1/X
           (List
            (∩
             (U
              (Pairof |0| (None (List (∩ 0/K (U |1| |2| |3|)) (∩ 1/K (U |1| |2| |3|)))))
              (∩ (Pairof 0/K (Some 0/X)) (Pairof |0| Any))
              (∩ (Pairof 1/K (Some 1/X)) (Pairof |0| Any)))
             A)
            (∩
             (U
              (Pairof |1| (None (List (∩ 0/K (U |0| |2| |3|)) (∩ 1/K (U |0| |2| |3|)))))
              (∩ (Pairof 0/K (Some 0/X)) (Pairof |1| Any))
              (∩ (Pairof 1/K (Some 1/X)) (Pairof |1| Any)))
             A)
            (∩
             (U
              (Pairof |2| (None (List (∩ 0/K (U |0| |1| |3|)) (∩ 1/K (U |0| |1| |3|)))))
              (∩ (Pairof 0/K (Some 0/X)) (Pairof |2| Any))
              (∩ (Pairof 1/K (Some 1/X)) (Pairof |2| Any)))
             A)
            (∩
             (U
              (Pairof |3| (None (List (∩ 0/K (U |0| |1| |2|)) (∩ 1/K (U |0| |1| |2|)))))
              (∩ (Pairof 0/K (Some 0/X)) (Pairof |3| Any))
              (∩ (Pairof 1/K (Some 1/X)) (Pairof |3| Any)))
             A)))))

(define (f kx x ky y)
  (list (cond
          [((make-predicate |0|) kx) (cons kx (Some x))]
          [((make-predicate |0|) ky) (cons ky (Some y))]
          [else (cons |0| (None (list kx ky)))])
        (cond
          [((make-predicate |1|) kx) (cons kx (Some x))]
          [((make-predicate |1|) ky) (cons ky (Some y))]
          [else (cons |1| (None (list kx ky)))])
        (cond
          [((make-predicate |2|) kx) (cons kx (Some x))]
          [((make-predicate |2|) ky) (cons ky (Some y))]
          [else (cons |2| (None (list kx ky)))])
        (cond
          [((make-predicate |3|) kx) (cons kx (Some x))]
          [((make-predicate |3|) ky) (cons ky (Some y))]
          [else (cons |3| (None (list kx ky)))]))
  ((λ () (error "not yet"))))








