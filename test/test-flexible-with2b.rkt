#lang type-expander

(require (lib "phc-graph/flexible-with2.hl.rkt")
         phc-toolkit/typed-rackunit-extensions)

(with-ρ (Row)
  (define-type test
    (∀ρ (A #:ρ Row)
        (→ Any Any))))

(with-ρ (Row)
  (define-type test2
    (∀ρ (A #:ρ Row)
        (→ Any Any))))
