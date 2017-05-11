#lang type-expander/lang
#|hyper-literate #:♦ #:no-auto-require (dotlambda/unhygienic
                                            . type-expander/lang)

♦chunk[<*>|#
       
(provide builder-τ)

(require racket/require
         (for-syntax (subtract-in racket/base subtemplate/override)
                     racket/list
                     racket/function
                     subtemplate/override))

(struct (T) Some ([f : T]))
(struct (T) None ([f : T]))

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
     
     #'(∀ ((?@ Kⱼ Xⱼ) …)
          (→ (?@ Kⱼ Xⱼ) …
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
                            (None Any)))
                 (∩ (Pairof Kᵢⱼ (Some Xᵢⱼ))
                    (Pairof Nᵢⱼ Any))
                 …)
              …)))]))

; ../../../.racket/snapshot/pkgs/typed-racket-lib/typed-racket/types/overlap.rkt:40:0: mask-accessor: contract violation
;  expected: mask?
;  given: #f
(define-type τ-4-2 (builder-τ 4 2))

;]

