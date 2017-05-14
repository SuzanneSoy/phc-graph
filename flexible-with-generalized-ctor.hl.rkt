#lang dotlambda/unhygienic type-expander/lang
#|hyper-literate #:♦ #:no-auto-require (dotlambda/unhygienic
                                            . type-expander/lang)

♦chunk[<*>|#
       
(provide builder-τ
         None
         Some
         Some?
         Some-f
         propagate-τ)

(require racket/require
         (for-syntax (subtract-in racket/base subtemplate/override)
                     syntax/stx
                     racket/list
                     racket/function
                     subtemplate/override)
         (for-meta 2 racket/base))

(struct (T) Some ([f : T]))
(struct (T) None ([f : T]))

(define-type-expander BinaryTree
  (syntax-parser
    [(_ leafⱼ …)
     ;; TODO: implement BinaryTree.
     #'(List leafⱼ …)]))

(define-syntax (def-SomeNone* stx)
  (syntax-case stx ()
    [(_ Some n)
     (with-syntax ([(Someᵢ …) (map (λ (i) (format-id #'Some "Some~a" i))
                                   (range n))])
       #`(begin
           (provide Someᵢ …)
           (struct (T) Someᵢ Some ()) …))]))

(def-SomeNone* Some 4)

(define-type-expander builder-τ
  (syntax-parser
    [(_ n m)
     #:with (Nᵢ …) (range n)
     #:with (Mⱼ …) (range m)
     #:with (Someᵢ …) (map (λ (n) (format-id #'HERE? "Some~a" n)) (Nᵢ …))
     #:with ((Someᵢⱼ …) …) (map λ.(map (const %) (Mⱼ …)) (#'Someᵢ …))
     ;#:with ((Noneᵢⱼ …) …) (map (const #'(Noneᵢ …)) (Nᵢ …))
     #:with ((Kᵢⱼ …) …) (map (const #'(Kⱼ …)) (Nᵢ …))
     #:with ((Xᵢⱼ …) …) (map (const #'(Xⱼ …)) (Nᵢ …))
     #:with ((Nᵢⱼ …) …) (map (λ (ni) (map (const ni) (Xⱼ …))) (Nᵢ …))
     #:with ((Nⱼᵢ …) …) (map (const #'(Nᵢ …)) (Mⱼ …))
     (define Ns (Nᵢ …))
     (define Ms (Mⱼ …))
     (define Somes (Someᵢ …))
     ;(define/with-syntax exceptⱼ (remove Mⱼ Ns)) …
     ;     (define/with-syntax ((exceptᵢⱼ …) …)
     ;       (map (const (exceptⱼ …)) (Nᵢ …)))
     (define/with-syntax (exceptᵢ …) ((remove Nᵢ Ns) …))
     (define/with-syntax (((exceptᵢⱼ …) …) …)
       ((map (const (remove Someᵢ Somes)) Ms) …))

     (define/with-syntax result
       #'(∀ (A (?@ Kⱼ Xⱼ) …)
            (→ (?@ (∩ Kⱼ (Some Any)) (∩ Kⱼ Xⱼ)) …
               (BinaryTree
                (∩ (U (Pairof Nᵢ
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
                              (None (List {∩ Kᵢⱼ {U (exceptᵢⱼ Any) …}} …)))
                      (∩ Kᵢⱼ (Someᵢⱼ Xᵢⱼ))
                      …)
                   A)
                …))))
     (displayln (syntax->datum #'result))
     #'result]))

(define-type propagate-τ
  (Pairof Any
          (U (None (Listof Natural))
             (Some Any))))

; ../../../.racket/snapshot/pkgs/typed-racket-lib/typed-racket/types/overlap.rkt
;:40:0: mask-accessor: contract violation
;  expected: mask?
;  given: #f
;(define-type τ-4-2 (builder-τ 4 2))


;]

