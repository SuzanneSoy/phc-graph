#lang hyper-literate #:♦ #:no-auto-require (dotlambda/unhygienic . racket/base)

♦require[scribble-math
         racket/require
         (for-label (subtract-in (only-meta-in 0 type-expander/lang)
                                 subtemplate/override)
                    typed-worklist
                    type-expander/expander
                    phc-toolkit/untyped/aliases
                    phc-toolkit/untyped/syntax-parse
                    subtemplate/override)]

♦title[#:style (with-html5 manual-doc-style)
       #:tag "graph-draft"
       #:tag-prefix "phc-graph/graph-draft"]{Draft of the implementation of
 the graph macro}

♦(chunks-toc-prefix
  '("(lib phc-graph/scribblings/phc-graph-implementation.scrbl)"
    "phc-graph/graph-draft"))

♦chunk[<overview>
       (define low-graph-impl
         (syntax-parser
           [<signature+metadata>
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            |<phase 1: call mappings and extract placeholders>|
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            |<phase 2: inline placeholders within node boundaries>|
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            |<phase 3: replace indices with promises>|
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            <equality-coalescing>
            <invariants+auto-fill>
            <inflexible-row-polymorphism>
            <flexible-row-polymorphism>
            <polymorphic-node-types-and-mappings>
            ;<general-purpose-graph-algorithms>
            ;<garbage-collection>
            |<phase~1: call mappings and extract placeholders>|
            ]))]

♦chunk[<signature+metadata>
       <signature>
       <metadata>]

♦chunk[<signature>
       (_ graph-name
          #:∀ (pvarₕ …)
          ({~lit node} nodeᵢ [fieldᵢⱼ cᵢⱼ:colon field-τᵢⱼ] …)
          …
          ({~lit mapping} (mappingₖ [argₖₗ :colon arg-τₖₗ] …)
                          :colon result-τₖ
                          . bodyₖ)
          …)]

♦chunk[<metadata>
       (void)]

♦chunk[|<phase 1: call mappings and extract placeholders>|
       '<worklist>
       '<call-mapping-functions+placeholders>
       '<extract-placeholders> ;; and put them into the worklist
       ]

♦chunk[|<phase~1: call mappings and extract placeholders>|
       #'(begin
           (define
             #:∀ (pvarₕ …)
             (graph-name [rootₖ : (Listof (List arg-τₖₗ …))] …)

             ;; TODO: move these to a separate literate programming chunk
             (define-type nodeᵢ (tagged nodeᵢ [fieldᵢⱼ cᵢⱼ field-τᵢⱼ] …))
             …

             (define (make-placeholderₖ argₖₗ …)
               (list 'placeholderₖ argₖₗ …))
             …
             
             (worklist
              (list rootₖ …)
              ((λ ([args : (List arg-τₖₗ …)])
                 (define-values (argₖₗ …) (apply values args))
                 (define result
                   (let* ([mappingₖ make-placeholderₖ]
                          …
                          [argₖₗ 'convert-inflexible-to-flexible?]
                          …
                          [argₖₗ 'invariant-well-scopedness?]
                          …)
                     (error "NOT IMPL YET.787543")
                     ;. bodyₖ
                     '(bodyₖ)))
                 ;; returns placeholders + the result:
                 '(extract-placeholders result)
                 (error "NOT IMPL YET.8946513648"))
               …)
              ((List arg-τₖₗ …) result-τₖ) …)))]

♦chunk[|<phase 1: call mappings and extract placeholders>|
       ;; Phase 1: call the mapping functions on the input data
       #'(: phase-1 (∀ (pvarₕ …) ;; or use this? (nodes-pvar … mapping-pvar … …)
                       (→ (List (Listof mapping-arg-type) ddd)
                          (List (Listof mapping-result-type) ddd))))
       #'(begin
           ;; Maybe this should be done last, when all phases are available?
           (define (phase1-many-roots (argₖₗ …) …) 'TODO)
           (define (phase1-single-root-for-mapping (argₖₗ …)) 'TODO)
           …)]

♦chunk[|<phase 2: inline placeholders within node boundaries>|
       ;; Phase 2: inline placeholders within node boundaries
       '(generate-worklist
         nodes
         #'(…?))
       '{(funcion which for a mapping-result → inserts nodes into worklist) …}
       '(for the root mapping results
          call the function to insert nodes and keep the surrounding part)
       '(for each mapping result
          call the function to insert nodes)]

♦chunk[|<phase 3: replace indices with promises>|
       ;; Phase 3: Replace indices with promises
       ;; Phase 3a: have an empty set of invariant witnesses, and call the
       ;;           invariants for checking
       ;; Phase 3b: have the full set of invariant witnesses.
       ;; TODO phase 3: auto-fill.
       (void)]

♦chunk[<equality-coalescing>
       ;; implement as always-#f-unless-eq? for now
       (void)]
♦chunk[<invariants+auto-fill>
       (void)]
♦chunk[<inflexible-row-polymorphism>
       (void)]
♦chunk[<flexible-row-polymorphism>
       (void)]
♦chunk[<polymorphic-node-types-and-mappings>
       (void)]

♦chunk[<overview>
       ; high-level graph API:
       #;(<metadata2>
          <extending-existing-graph-types>
          <invariants-for-extended-graph-types>
          <auto-generate-mappings>)]

Row polymorphism: make a generic struct->vector and vector->struct?

♦chunk[<*>
       (provide low-graph-impl
                (for-template (all-from-out "literals.rkt")))
       
       (require (for-template (only-meta-in 0 type-expander/lang)
                              typed-worklist
                              phc-adt)
                type-expander/expander
                phc-toolkit/untyped/aliases
                phc-toolkit/untyped/syntax-parse
                subtemplate/override)

       (require (for-template "literals.rkt"))
       <overview>]