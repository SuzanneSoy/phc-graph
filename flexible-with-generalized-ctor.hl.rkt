#lang type-expander/lang
#|hyper-literate #:♦ #:no-auto-require (dotlambda/unhygienic
                                            . type-expander/lang)

♦chunk[<*>|#
       
(provide builder-τ
         None
         Some
         Some?
         Some-f
         N/S)

(require racket/require
         (for-syntax (subtract-in racket/base subtemplate/override)
                     racket/list
                     racket/function
                     subtemplate/override))

(struct N/S ())
(struct (T) Some N/S ([f : T]))
(struct (T) None N/S ([f : T]))

(define-type-expander BinaryTree
  (syntax-parser
    [(_ leafⱼ …)
     ;; TODO: implement BinaryTree.
     #'(List leafⱼ …)]))

(define-type-expander builder-τ
  (syntax-parser
    [(_ n m)
     #:with (Nᵢ …) (range n)
     #:with (Mⱼ …) (range m)
     #:with ((Kᵢⱼ …) …) (map (const (Kⱼ …)) (Nᵢ …))
     #:with ((Xᵢⱼ …) …) (map (const (Xⱼ …)) (Nᵢ …))
     #:with ((Nᵢⱼ …) …) (map (λ (ni) (map (const ni) (Xⱼ …))) (Nᵢ …))
     (define Ns (Nᵢ …))
     (define Ms (Mⱼ …))
     ;(define/with-syntax exceptⱼ (remove Mⱼ Ns)) …
     ;     (define/with-syntax ((exceptᵢⱼ …) …)
     ;       (map (const (exceptⱼ …)) (Nᵢ …)))
     (define/with-syntax (exceptᵢ …) ((remove Nᵢ Ns) …))
     (define/with-syntax ((exceptᵢⱼ …) …)
       ((map (const (remove Nᵢ Ns)) Ms) …))
     
     #'(∀ (A (?@ Kⱼ Xⱼ) …)
          (→ A
             (?@ Kⱼ Xⱼ) …
             (BinaryTree
              (U (Pairof Nᵢ
                         ;; If Kⱼ is Nᵢ, then {∩ Kᵢⱼ {U . exceptᵢⱼ}} will
                         ;; Nothing. We multiply the Nothing together by
                         ;; building a List out of them (a single occurrence
                         ;; of Nothing should collapse the list), so that the
                         ;; result should be Nothing only if there is no Kⱼ
                         ;; equal to Nᵢ. To force TR to propagate Nothing as
                         ;; much as possible, we intersect it with
                         ;; (None Any), which should be a no-op, but makes
                         ;; sure the simplifier which runs with ∩ kicks in.
                         ;; Therefore, the (None whatever) should appear only
                         ;; if there is indeed no key provided for that leaf.
                         (∩ (None (List {∩ Kᵢⱼ {U . exceptᵢⱼ}} …))
                            A))
                 (∩ (Pairof Kᵢⱼ (Some Xᵢⱼ))
                    (Pairof Nᵢⱼ A))
                 …)
              …)))]))

; ../../../.racket/snapshot/pkgs/typed-racket-lib/typed-racket/types/overlap.rkt:40:0: mask-accessor: contract violation
;  expected: mask?
;  given: #f
;(define-type τ-4-2 (builder-τ 4 2))

(define-type t-4-2
  (All (A 0/K 0/X 1/K 1/X)
       (-> A
           0/K
           0/X
           1/K
           1/X
           (List
            (U (Pairof (∩ 0/K Zero) (∩ (Some 0/X) A))
               (Pairof (∩ 1/K Zero) (∩ (Some 1/X) A))
               (Pairof
                Zero
                (∩
                 (None
                  (List
                   (U (∩ 0/K 2) (∩ 0/K 3) (∩ 0/K One))
                   (U (∩ 1/K 2) (∩ 1/K 3) (∩ 1/K One))))
                 A)))
            (U (Pairof (∩ 0/K One) (∩ (Some 0/X) A))
               (Pairof (∩ 1/K One) (∩ (Some 1/X) A))
               (Pairof
                One
                (∩
                 (None
                  (List
                   (U (∩ 0/K 2) (∩ 0/K 3) (∩ 0/K Zero))
                   (U (∩ 1/K 2) (∩ 1/K 3) (∩ 1/K Zero))))
                 A)))
            (U (Pairof (∩ 0/K 2) (∩ (Some 0/X) A))
               (Pairof (∩ 1/K 2) (∩ (Some 1/X) A))
               (Pairof
                2
                (∩
                 (None
                  (List
                   (U (∩ 0/K 3) (∩ 0/K One) (∩ 0/K Zero))
                   (U (∩ 1/K 3) (∩ 1/K One) (∩ 1/K Zero))))
                 A)))
            (U (Pairof (∩ 0/K 3) (∩ (Some 0/X) A))
               (Pairof (∩ 1/K 3) (∩ (Some 1/X) A))
               (Pairof
                3
                (∩
                 (None
                  (List
                   (U (∩ 0/K 2) (∩ 0/K One) (∩ 0/K Zero))
                   (U (∩ 1/K 2) (∩ 1/K One) (∩ 1/K Zero))))
                 A)))))))

;]

