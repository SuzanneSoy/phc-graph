#lang scribble/manual
@require[@for-label[phc-graph
                    racket/base]]

@title{Ph.C Graph library: Implementation}
@author[@author+email["Suzanne Soy" "racket@suzanne.soy"]]

This library is implemented using literate programming. The implementation
details are presented in the following sections. The user documentation is in
the @other-doc['(lib "phc-graph/scribblings/phc-graph.scrbl")] document.

@(table-of-contents)

@include-section[(submod "../traversal.hl.rkt" doc)]
@include-section[(submod "../flexible-with2.hl.rkt" doc)]
@include-section[(submod "../invariants-phantom.hl.rkt" doc)]
@include-section[(submod "../graph-info.hl.rkt" doc)]
@include-section[(submod "../graph-type.hl.rkt" doc)]
@include-section[(submod "../graph.hl.rkt" doc)]
@include-section[(submod "../main-draft.hl.rkt" doc)]
@include-section[(submod "../flexible-with-generalized-ctor.hl.rkt" doc)]
