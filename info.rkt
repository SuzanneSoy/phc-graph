#lang info
(define collection "phc-graph")
(define deps '("base"
               "rackunit-lib"
               "phc-toolkit"
               "phc-adt"
               "type-expander"
               "hyper-literate"
               "scribble-enhanced"
               "typed-racket-lib"
               "srfi-lite-lib"
               "delay-pure"
               "typed-map"
               "scribble-lib"
               "pconvert-lib"
               "remember"
               "extensible-parser-specifications"
               "subtemplate"
               "stxparse-info"
               "dotlambda"
               "typed-worklist"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "remember"
                     "typed-racket-doc"
                     "aful"
                     "scribble-math"))
(define scribblings
  '(("scribblings/phc-graph.scrbl" ()
                                   ("Data Structures"))
    ("scribblings/phc-graph-implementation.scrbl" (multi-page)
                                                  ("Data Structures"))))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '("Georges Dupéron"))
