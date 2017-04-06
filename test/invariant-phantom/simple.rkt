#lang type-expander

(require (lib "phc-graph/invariants-phantom.hl.rkt")
         "util.rkt"
         phc-graph/dot-lang
         phc-toolkit)

(define-type-expander (Π stx)
  (syntax-case stx ()
    [(_ . π)
     (parse-path #'π)]))

(check-same-type
 (Π (λdot a aa) ((λdot b c))* (λdot d e))
 (Rec
  R
  (U (Pairof Any R)
     (Pairof
      (Pairof 'a AnyType)
      (Pairof
       (Pairof 'aa AnyType)
       (Rec
        R
        (U (Pairof
            (Pairof 'b AnyType)
            (Pairof (Pairof 'c AnyType) R))
           (List (Pairof 'd AnyType) (Pairof 'e AnyType)))))))))
(struct a ()); the field.
(check-same-type
 (Π (dot :a aa) ((λdot b c))* (λdot d e))
 (Rec
  R
  (U (Pairof Any R)
     (Pairof
      (Pairof AnyField a)
      (Pairof
       (Pairof 'aa AnyType)
       (Rec
        R
        (U (List (Pairof 'd AnyType) (Pairof 'e AnyType))
           (Pairof (Pairof 'b AnyType) (Pairof (Pairof 'c AnyType) R)))))))))
(check-same-type
 (Π (dot :a) ((λdot b c))* (λdot d e))
 (Rec
  R
  (U (Pairof Any R)
     (Pairof
      (Pairof AnyField a)
      (Rec
       R
       (U (List (Pairof 'd AnyType) (Pairof 'e AnyType))
          (Pairof (Pairof 'b AnyType) (Pairof (Pairof 'c AnyType) R))))))))

(check-same-type
 (Π (dot :a) ((λdot b c) ((λdot w)) * (λdot x y))* (λdot d e))
 (Rec
  R
  (U (Pairof Any R)
     (Pairof
      (Pairof AnyField a)
      (U (List (Pairof 'd AnyType) (Pairof 'e AnyType))
         (Pairof
          (Pairof 'b AnyType)
          (Pairof
           (Pairof 'c AnyType)
           (Rec
            R
            (U (Pairof (Pairof 'w AnyType) R)
               (Pairof
                (Pairof 'x AnyType)
                (Pairof (Pairof 'y AnyType) R)))))))))))

#|

(check-ann witness-value (Invariants)) ;; No invariants
(check-ann witness-value (Invariants (≡ (_ a) (_ a b c))))

(check-a-stronger-than-b (Invariants (≡ (_ a) (_ a b c)))
                         (Invariants))

(check-a-same-as-b (Invariants (≡ (_ a) (_ a b c)))
                   (Invariants (≡ (_ a b c) (_ a))))

(check-a-stronger-than-b (Invariants (≡ (_) (_ b c))
                                     (≡ (_) (_ b d)))
                         (Invariants (≡ (_) (_ b c))))
(check-a-stronger-than-b (Invariants (≡ (_) (_ b d))
                                     (≡ (_) (_ b c)))
                         (Invariants (≡ (_) (_ b c))))

;; ∀ .b.d(.a.b.>d)* of length ≥ 5
;; is stronger than
;; ∀ .b.d(.a.b.>d)* of length ≥ 8
;; as the elements of the latter are included in the former, but
;; the first element (length = 5) is missing in the latter, so the
;; former constrains more paths.
(check-a-stronger-than-b (Invariants (≡ (_)
                                        (_ b d ↙ a b (d))))
                         (Invariants (≡ (_)
                                        (_ b d a b d ↙ a b (d)))))

(check-a-stronger-than-b (Invariants (≡ (_)
                                        (_ a b c ↙ d (e))))
                         (Invariants (≡ (_)
                                        (_ a b c d e))))
|#


(check-ann witness-value (Invariants)) ;; No invariants
(check-ann witness-value (Invariants (≡ (_ a) (_ a b c))))

(check-a-stronger-than-b (Invariants (≡ (_ a) (_ a b c)))
                         (Invariants))

(check-a-same-as-b (Invariants (≡ (_ a) (_ a b c)))
                   (Invariants (≡ (_ a b c) (_ a))))

(check-a-stronger-than-b (Invariants (≡ (_) (_ b c))
                                     (≡ (_) (_ b d)))
                         (Invariants (≡ (_) (_ b c))))
(check-a-stronger-than-b (Invariants (≡ (_) (_ b d))
                                     (≡ (_) (_ b c)))
                         (Invariants (≡ (_) (_ b c))))

;; ∀ .b.d(.a.b.>d)* of length ≥ 5
;; is stronger than
;; ∀ .b.d(.a.b.>d)* of length ≥ 8
;; as the elements of the latter are included in the former, but
;; the first element (length = 5) is missing in the latter, so the
;; former constrains more paths.
(check-a-stronger-than-b (Invariants (≡ (_)
                                        (_ b d ↙ a b (d))))
                         (Invariants (≡ (_)
                                        (_ b d a b d ↙ a b (d)))))

(check-a-stronger-than-b (Invariants (≡ (_)
                                        (_ a b c ↙ d (e))))
                         (Invariants (≡ (_)
                                        (_ a b c d e))))