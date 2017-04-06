#lang type-expander

(provide check-a-same-as-b
         check-a-stronger-than-b
         check-same-type)

(require phc-toolkit
         (lib "phc-graph/invariants-phantom.hl.rkt")
         (for-syntax phc-toolkit/untyped))

(define-syntax (check-a-stronger-than-b stx)
  (syntax-case stx ()
    [(_ stronger weaker)
     (syntax/top-loc stx
       (begin (check-ann (ann witness-value stronger)
                         weaker)
              (check-not-tc
               (ann (ann witness-value weaker) stronger))))]))
       
(define-syntax (check-a-same-as-b stx)
  (syntax-case stx ()
    [(_ a b)
     (syntax/top-loc stx
       (begin
         (check-ann (ann witness-value a) b)
         (check-ann (ann witness-value b) a)))]))

(define-syntax (check-same-type stx)
  (syntax-case stx ()
    [(_ a b)
     (syntax/top-loc stx
       (begin
         (check-not-exn:
          (λ () (λ ([x : a]) (check-ann x b))))
         (check-not-exn:
          (λ () (λ ([x : b]) (check-ann x a))))))]))