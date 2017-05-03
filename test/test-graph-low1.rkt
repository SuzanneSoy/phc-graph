#lang dotlambda/unhygienic type-expander/lang

(require (for-syntax (lib "phc-graph/main-draft.hl.rkt")))
(require phc-adt)
(adt-init)

(define-syntax low-graph low-graph-impl)

(low-graph
 g
 #:∀ (A)
 (node City [streets : (Listof Street)])
 (node Street [name : String] [a : A])
 (mapping (make-city [names : (Listof (Pairof String A))])
   : City
   (City (map make-street names)))
 (mapping (make-street [p : (Pairof String A)])
   : Street
   (Street (car p) (cdr p))))