#lang phc-graph/xtyped

(require "../flexible-with2.hl.rkt"
         phc-toolkit/typed-rackunit-extensions)

(check-ann ((record-builder 4 1 2 4 7 14) 'a 'b 'c 'd 'e)
           (Pairof
            (Pairof
             (Pairof (Pairof empty1/τ 'a) (Pairof 'b empty1/τ))
             (Pairof (Pairof 'c empty1/τ) (Pairof empty1/τ 'd)))
            (Pairof empty4/τ (Pairof empty2/τ (Pairof 'e empty1/τ)))))

(with-ρ (Row)
  (define-type test
    (∀ρ (A #:ρ Row)
        (→ (List A Row)
           (record a b . Row)
           (List A Row)
           (record a c . Row)))))

