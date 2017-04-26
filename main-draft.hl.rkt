#lang aful/unhygienic hyper-literate type-expander/lang

@chunk[<overview>
       #;(define-syntax low-graph
           (syntax-parser
             [<signature>
              <metadata>
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              <worklist>
              <call-mapping-functions+placeholders>
              <extract-placeholders> ;; and put them into the worklist
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              <inline-placeholders-within-node-boundaries>
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              <replace-indices-with-promises>
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              <equality-coalescing> ;; leave off as always-#f-unless-eq?
              <invariants+auto-fill>
              <inflexible-row-polymorphism>
              <flexible-row-polymorphism>
              <polymorphic-node-types-and-mappings>
              ;<general-purpose-graph-algorithms>
              ;<garbage-collection>
              ]))

       (define-syntax low-graph
         (syntax-parser
           [<signature+metadata>
            ;; Phase 1: call the mapping functions on the input data
            (: phase-1 (∀ (nodes-pvar … mapping-pvar … …)
                          (→ (List (Listof mapping-arg-type) ddd)
                             (List (Listof mapping-result-type) ddd))))
            (define (phase-1 roots)
              (work
               roots
               ((λ (mapping-arguments)
                  (define result
                    (let* ([mapping-name make-placeholder]
                           …
                           [arg convert-inflexible-to-flexible?]
                           …
                           [arg invariant-well-scopedness?]
                           …)
                      mapping-code))
                  ;; returns placeholders + the result:
                  (extract-placeholders result))
                …)
               (mapping-arg-type mapping-result-type) …))
            (begin
              ;; Maybe this should be done last, when all phases are available?
              (define (phase1-many-roots) 'TODO)
              (define (phase1-single-root-for-mapping) 'TODO)
              …)
            ;; Phase 2: inline placeholders within node boundaries
            (generate-worklist
             nodes
             #'(…?))
            (funcion which for a mapping-result → inserts nodes into worklist) …
            (for the root mapping results
              call the function to insert nodes and keep the surrounding part)
            (for each mapping result
              call the function to insert nodes)
            ;; Phase 3: Replace indices with promises
            ;; Phase 3a: have an empty set of invariant witnesses, and call the
            ;;           invariants for checking
            ;; Phase 3b: have the full set of invariant witnesses.
            ;; TODO phase 3: auto-fill.
            ]))

       ; high-level graph API:
       '(<metadata2>
         <extending-existing-graph-types>
         <invariants-for-extended-graph-types>
         <auto-generate-mappings>)]

Row polymorphism: make a generic struct->vector and vector->struct?

@chunk[<*>
       (void)]