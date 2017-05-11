#lang phc-graph/make-lang-example
(let ()
  (ann 1 Number) ;; from type-expander/lang
  (ann (∘ add1 sub1) (-> Number Number)) ;; From phc-toolkit
  (void))