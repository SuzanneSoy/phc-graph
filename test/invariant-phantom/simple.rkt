#lang typed/dotlambda
(require type-expander)

(require (lib "phc-graph/invariants-phantom.hl.rkt")
         "util.rkt"
         phc-graph/dot-lang
         phc-toolkit)

(check-same-type
 (Π .a.aa(.b.c)*.d.e)
 (Rec
  R1
  (U (Pairof Any R1)
     (Pairof
      (Pairof 'a AnyType)
      (Pairof
       (Pairof 'aa AnyType)
       (Rec
        R2
        (U (Pairof
            (Pairof 'b AnyType)
            (Pairof (Pairof 'c AnyType) R2))
           (List (Pairof 'd AnyType) (Pairof 'e AnyType)))))))))

(struct a AnyType ()); the node type a.
(struct b AnyType ()); the node type b.

(check-same-type
 (Π :a.aa(.b.c)*.d.e)
 (Rec
  R1
  (U (Pairof Any R1)
     (Pairof
      (Pairof AnyField a)
      (Pairof
       (Pairof 'aa AnyType)
       (Rec
        R2
        (U (List (Pairof 'd AnyType) (Pairof 'e AnyType))
           (Pairof (Pairof 'b AnyType) (Pairof (Pairof 'c AnyType) R2)))))))))
(check-same-type
 (Π :a(.b.c)*.d.e)
 (Rec
  R1
  (U (Pairof Any R1)
     (Pairof
      (Pairof AnyField a)
      (Rec
       R2
       (U (List (Pairof 'd AnyType) (Pairof 'e AnyType))
          (Pairof (Pairof 'b AnyType) (Pairof (Pairof 'c AnyType) R2))))))))

(check-same-type
 (Π :a(.b.c(.w)*.x.y)*.d.e)
 (Rec
  R1
  (U (Pairof Any R1)
     (Pairof
      (Pairof AnyField a)
      (Rec
       R2
       (U (List (Pairof 'd AnyType) (Pairof 'e AnyType))
          (Pairof
           (Pairof 'b AnyType)
           (Pairof
            (Pairof 'c AnyType)
            (Rec
             R3
             (U (Pairof (Pairof 'w AnyType) R3)
                (Pairof
                 (Pairof 'x AnyType)
                 (Pairof (Pairof 'y AnyType) R2))))))))))))

;; TODO: test with deeper nesting of ()*

(check-same-type
 (Invariant :a(.b.c(.w)*.x.y)*.d.e ≡ :a(.b.c)*.d.e)
 (Invariant :a(.b.c(.w)*.x.y)*.d.e ≡ :a(.b.c)*.d.e))

(check-same-type
 (Invariant :a(.b.c(.w)*.x.y)*.d.e
            ∈
            :a(.b.c)*.d.e)
 (Invariant :a(.b.c)*.d.e
            ∋
            :a(.b.c(.w)*.x.y)*.d.e))


;;;

(check-ann witness-value (Invariants)) ;; No invariants
(check-ann witness-value (Invariants (:a ≡ :a.b.c)))

(check-a-stronger-than-b (Invariants (:a ≡ :a.b.c))
                         (Invariants))
(check-a-same-as-b (Invariants (:a ≡ :a.b.c))
                   (Invariants (:a.b.c ≡ :a)))
(check-a-stronger-than-b (Invariants (: ≡ :b.c)
                                     (: ≡ :b.d))
                         (Invariants (: ≡ :b.c)))
(check-a-stronger-than-b (Invariants (: ≡ :b.d)
                                     (: ≡ :b.c))
                         (Invariants (: ≡ :b.c)))

;; ∀ .b.d(.a.b.>d)* of length ≥ 5
;; is stronger than
;; ∀ .b.d(.a.b.>d)* of length ≥ 8
;; as the elements of the latter are included in the former, but
;; the first element (length = 5) is missing in the latter, so the
;; former constrains more paths.
(check-a-stronger-than-b (Invariants (: ≡ .b.d(.a.b.d)*))
                         (Invariants (: ≡ .b.d.a.b.d(.a.b.d)*)))

(check-a-stronger-than-b (Invariants (: ≡ .a.b.c(.d.e)*))
                         (Invariants (: ≡ .a.b.c.d.e)))


(check-a-stronger-than-b (Invariants (.l ≥ (+ (length .a.b.c(.d.e)*) 3)))
                         (Invariants (.l ≥ (+ (length .a.b.c(.d.e)*) 2))))

(check-a-stronger-than-b (Invariants (.l ≥ (+ (length .a.b.c(.d.e)*) 1)))
                         (Invariants (.l ≥ (length .a.b.c(.d.e)*))))

(check-a-stronger-than-b (Invariants (.l ≤ (+ (length .a.b.c(.d.e)*) 2)))
                         (Invariants (.l ≤ (+ (length .a.b.c(.d.e)*) 3))))

(check-a-stronger-than-b (Invariants (.l ≤ (length .a.b.c(.d.e)*)))
                         (Invariants (.l ≤ (+ (length .a.b.c(.d.e)*) 1))))

(check-a-stronger-than-b (Invariants (.l = (length .a.b.c(.d.e)*)))
                         (Invariants (.l ≥ (length .a.b.c(.d.e)*))))

(check-a-stronger-than-b (Invariants (.l = (+ (length .a.b.c(.d.e)*) 1)))
                         (Invariants (.l ≤ (+ (length .a.b.c(.d.e)*) 1))))

(check-same-type (Invariants (.l = (length .a.b.c(.d.e)*)))
                 (Invariants (.l = (+ (length .a.b.c(.d.e)*) 0))))

(check-a-stronger-than-b (Invariants (:a.l ≥ (+ (length :a.f.g) 3))
                                     (:a ≡ :a.f.h)
                                     (:a ∉ :a.f.g))
                         (Invariants (:a.l ≥ (+ (length :a.f.g) 2))
                                     (:a ≢ :a.f.g.0)
                                     (:a ≢ :a.f.g.1)))
