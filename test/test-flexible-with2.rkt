#lang type-expander

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

;; TODO: I'm pretty sure that the lift-ing will cause some unbound identifier
;; or out of context id errors, when τ' refers to a locally-declared type.

(with-ρ (Row)
  (define-type testf-τ (∀ρ (A #:ρ Row)
                           (→ (→ (List A Row)
                                 (record x y . Row)
                                 (List A Row)
                                 (record y z . Row))
                              Void)))
  (: testf
     (∀ρ (A #:ρ Row)
         (→ (→ (List A Row)
               (record x y . Row)
               (List A Row)
               (record y z . Row))
            Void)))
  (define (testf f)
    (: tmp (→ (record w x . Row) Any))
    (define (tmp r) r)
    (void)))

(let ()
  (with-ρ (Row)
    (define-type Naht Integer)
    (: testf
       (∀ρ (A #:ρ Row)
           (→ (→ (List Naht A Row)
                 (record x y . Row)
                 (List Naht A Row)
                 (record y z . Row))
              Void)))
    (define (testf f)
      (: tmp (→ (record w x . Row) Any))
      (define (tmp r) r)
      (void)))
  testf)