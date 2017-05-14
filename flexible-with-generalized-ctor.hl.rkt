#lang dotlambda/unhygienic type-expander/lang
#|hyper-literate #:♦ #:no-auto-require (dotlambda/unhygienic
                                            . type-expander/lang)

♦chunk[<*>|#
       
(provide builder-τ
         None
         Some
         Some?
         Some-f
         #;propagate-τ)

(require racket/require
         (for-syntax (subtract-in racket/base subtemplate/override)
                     syntax/stx
                     racket/list
                     racket/function
                     subtemplate/override)
         (for-meta 2 racket/base))

(struct (T) Some ([f : T]))
(struct None ())

(define-type-expander BinaryTree
  (syntax-parser
    [(_ leafⱼ …)
     ;; TODO: implement BinaryTree.
     #'(List leafⱼ …)]))

(define-syntax (def-SomeNone* stx)
  (syntax-case stx ()
    [(_ Some None n)
     (with-syntax ([(Someᵢ …) (map (λ (i) (format-id #'Some "Some~a" i))
                                   (range n))]
                   [(Noneᵢ …) (map (λ (i) (format-id #'None "None~a" i))
                                   (range n))])
       #`(begin
           (provide Someᵢ … Noneᵢ …)
           (struct (T) Someᵢ Some ()) …
           (struct Noneᵢ None ()) …))]))

(def-SomeNone* Some None 4)

(define-type-expander builder-τ
  (syntax-parser
    [(_ n m)
     #:with (Nᵢ …) (range n)
     #:with (Mⱼ …) (range m)
     #:with (Someᵢ …) (map (λ (n) (format-id #'HERE? "Some~a" n)) (Nᵢ …))
     #:with ((Someᵢⱼ …) …) (map λ.(map (const %) (Mⱼ …)) (#'Someᵢ …))
     #:with (Noneᵢ …) (map (λ (n) (format-id #'HERE? "None~a" n)) (Nᵢ …))
     ;#:with ((Noneᵢⱼ …) …) (map (const #'(Noneᵢ …)) (Nᵢ …))
     #:with ((Kᵢⱼ …) …) (map (const #'(Kⱼ …)) (Nᵢ …))
     #:with ((Xᵢⱼ …) …) (map (const #'(Xⱼ …)) (Nᵢ …))
     #:with ((Nᵢⱼ …) …) (map (λ (ni) (map (const ni) (Xⱼ …))) (Nᵢ …))
     #:with ((Nⱼᵢ …) …) (map (const #'(Nᵢ …)) (Mⱼ …))
     (define Ns (Nᵢ …))
     (define Nones (#'Noneᵢ …))
     (define Ms (Mⱼ …))
     ;(define/with-syntax exceptⱼ (remove Mⱼ Ns)) …
     ;     (define/with-syntax ((exceptᵢⱼ …) …)
     ;       (map (const (exceptⱼ …)) (Nᵢ …)))
     (define/with-syntax ((exceptᵢ …) …) ((remove Noneᵢ Nones) …))
     (define/with-syntax ((exceptᵢⱼ …) …)
       ((map (const (remove #'Noneᵢ Nones free-identifier=?)) Ms) …))

     (define/with-syntax result
       #'(∀ (A (?@ Kⱼ Xⱼ) …)
            (→ (?@ (∩ Kⱼ (U None (Some Any))) Xⱼ) …
               (BinaryTree
                (∩ (U (∩ Noneᵢ Kᵢⱼ …)
                      (∩ Kᵢⱼ (Someᵢⱼ Xᵢⱼ))
                      …)
                   A)
                …))))
     (displayln (syntax->datum #'result))
     #'result]))

#;(define-type propagate-τ
    (Pairof Any
            (U (None (Listof Natural))
               (Some Any))))

; ../../../.racket/snapshot/pkgs/typed-racket-lib/typed-racket/types/overlap.rkt
;:40:0: mask-accessor: contract violation
;  expected: mask?
;  given: #f
;(define-type τ-4-2 (builder-τ 4 2))


;]

