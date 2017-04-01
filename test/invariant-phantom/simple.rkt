#lang type-expander

(require (lib "phc-graph/invariants-phantom.hl.rkt")
         "util.rkt"
         phc-toolkit)

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