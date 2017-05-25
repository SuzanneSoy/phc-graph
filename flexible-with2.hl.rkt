#lang hyper-literate #:꩜ (dotlambda/unhygienic . type-expander/lang)

꩜title[#:style manual-doc-style ;#:style (with-html5 manual-doc-style)
       #:tag "flexible-with"
       #:tag-prefix "phc-graph/flexible-with"]{Flexible functional
 modification and extension of records}

꩜(chunks-toc-prefix
  '("(lib phc-graph/scribblings/phc-graph-implementation.scrbl)"
    "phc-graph/flexible-with"))

꩜require[scriblib/footnote
         scriblib/figure
         hyper-literate/diff1
         racket/runtime-path]

꩜require[scribble/core
         scribble/html-properties]
꩜elem[#:style (style #f (list (css-addition
                               #"
.Figure,
.FigureMulti,
.FigureMultiWide,
.Herefigure {
  border-color: #b5b5b5 !important;
}")))]

꩜section{Goals}

Our goal here is to have strongly typed records, with row polymorphism (a
꩜racket[Rest] row type variable can range over multiple possibly-present
fields), and structural type equivalence (two record types are identical if
they have the same fields and the type of the fields is the same in both record
types).

꩜section{Overview}

We represent a flexible record as a tree, where the leaves are field values.
Every field which occurs anywhere in the program is assigned a constant index.
This index determines which leaf is used to store that field's values. In
order to avoid storing in-memory a huge tree for every record, the actual
fields are captured by a closure, and the tree is lazily generated (node by
node) upon access.

The type for a flexible record can support row polymorphism: the type of
fields which may optionally be present are represented by a polymorphic type
꩜; TODO: "not only one" sounds like "¬only … but also" instead of "¬limited to…"
variable. Note that this means that not only one type variable is used, but
several꩜note{In principle, each potentially-present field would need a
 distinct type variable, but whole potentially-present branches may be
 collapsed to a single type variable if no access is made to the variables
 within.}.


꩜section{Generating the tree type with polymorphic holes}

We define in this section facilities to automatically generate this list of
polymorphic type variables. In order to avoid having a huge number of type
variables, a branch containing only optional fields can be collapsed into a
single type variable. An exception to this rule is when a field needs to be
added or modified by the user code: in this case the polymorphic type variable
for that field must be preserved, and the branch may not entirely be
collapsed.

We take as an example the case where that there are 8 fields (꩜racketid[f1],
꩜racketid[f2], ꩜racketid[a], ꩜racketid[b], ꩜racketid[f5], ꩜racketid[f6],
꩜racketid[f7] and ꩜racketid[u]) used in the whole program. Records are
therefore represented as a tree of depth 4 (including the root node) with 8
leaves. A record with the fields ꩜racketid[a] and ꩜racketid[b] will be
represented as a tree where the branch containing ꩜racketid[f1] and
꩜racketid[f2] is collapsed. Furthermore, the right branch of the root,
containing ꩜racketid[f5], ꩜racketid[f6], ꩜racketid[f7] and ꩜racketid[u] will
also be collapsed.

꩜define-runtime-path[scribblings/collapse.png
                     "scribblings/collapsed.png"]
꩜figure["fig:flex-with-tree:collapsed"
        ꩜elem{The tree representing the type for a record with fields
         ꩜racketid[a] and ꩜racketid[b] is ꩜racket[((C¹ . (a . b)) . C²)], where
         ꩜racket[Cⁱ] indicates that a single polymorphic type is used to
         represent the type of the whole collapsed branch.}
        ꩜image[scribblings/collapse.png]{}]

꩜define-runtime-path[scribblings/collapsed-updated.png
                     "scribblings/collapsed-updated.png"]
꩜figure["fig:flex-with-tree:collapsed-updated"
        ꩜elem{Updating the node ꩜racketid[u], which appears somewhere in the
         collapsed branch ꩜racketid[C²] is not possible, because the type
         abstracted away by ꩜racketid[C²] may be different before and after the
         update.}
        ꩜image[scribblings/collapsed-updated.png]{}]

꩜define-runtime-path[scribblings/collapsed-breakdown.png
                     "scribblings/collapsed-breakdown.png"]
꩜figure["fig:flex-with-tree:collapsed-updated"
        ꩜elem{We therefore break apart the collapsed branch ꩜racketid[C²]. The
         tree representing the type for a record with fields ꩜racketid[a] and
         ꩜racketid[b], and where the field ꩜racketid[u] may be updated or added,
         is ꩜racket[((C¹ . (a . b)) . (|C²′| . (C³ . u)))]. Note that
         ꩜racketid[u] may refer to a possibly absent field (in which case the
         polymorphic type variable will be instantiated with ꩜racket[None].}
        ꩜image[scribblings/collapsed-breakdown.png]{}]

꩜section{From a selection of field indieces to a tree shape}

꩜defproc[#:kind "phase 1 procedure"
         (idx→tree [none-wrapper (->d [depth exact-nonnegative-integer?]
                                      any/c)]
                   [leaf-wrapper (->d [i-in-vec exact-nonnegative-integer?]
                                      [leaf-idx exact-nonnegative-integer?]
                                      any/c)]
                   [node-wrapper (->d [left any/c]
                                      [right any/c]
                                      any/c)]
                   [vec (vectorof exact-nonnegative-integer?)])
         r]{
 Given a flat list of field indicies (that is, leaf positions in the tree
 representation), ꩜racket[idx→tree] generates tree-shaped code, transforming
 each leaf with ꩜racket[_leaf-wrapper], each intermediate node with
 ꩜racket[_node-wrapper] and each leaf not present in the initial list with
 ꩜racket[none-wrapper].}



꩜chunk[<idx→tree>
       (define/contract (idx→tree #:none-wrapper none-wrapper
                                  #:leaf-wrapper leaf-wrapper
                                  #:node-wrapper node-wrapper
                                  #:vec vec)
         <idx→tree/ctc>
         (define (r #:depth depth
                    #:vec-bounds vec-start vec-after-end
                    #:leaf-bounds first-leaf after-last-leaf)
           (cond
             |<idx→tree cases>|))
         r)]

The ꩜racket[idx→tree] is a two-step curried function, and the first step
returns a function with the following signature:

꩜defproc[(r [#:depth depth exact-nonnegative-integer?]
            [#:vec-bounds vec-start exact-nonnegative-integer?]
            #;|        | [vec-after-end exact-nonnegative-integer?]
            [#:leaf-bounds first-leaf exact-nonnegative-integer?]
            #;|         | [after-last-leaf exact-nonnegative-integer?])
         any/c]{}

The ꩜racket[idx→tree] function works by performing repeated dichotomy on the
vector of present fields ꩜racket[vec].

꩜hlite[|<idx→tree cases>| {-/ (cond = _ _ _)}
       (cond
         [(= vec-start vec-after-end)
          (none-wrapper depth)]
         [(= (+ first-leaf 1) after-last-leaf)
          (leaf-wrapper vec-start (vector-ref vec vec-start))]
         [else
          (let* ([leftmost-right-leaf (/ (+ first-leaf after-last-leaf) 2)]
                 [vec-left-branch-start vec-start]
                 [vec-left-branch-after-end
                  (find-≥ leftmost-right-leaf vec vec-start vec-after-end)]
                 [vec-right-branch-start vec-left-branch-after-end]
                 [vec-right-branch-after-end vec-after-end])
            (node-wrapper
             (r #:depth (sub1 depth)
                #:vec-bounds vec-left-branch-start
                #;|        | vec-left-branch-after-end
                #:leaf-bounds first-leaf
                #;|         | leftmost-right-leaf)
             (r #:depth (sub1 depth)
                #:vec-bounds vec-right-branch-start
                #;|        | vec-right-branch-after-end
                #:leaf-bounds leftmost-right-leaf
                #;|         | after-last-leaf)))])]

The ꩜racket[find-≥] function performs the actual dichotomy, and finds the
first index within the given bounds for which ꩜racket[(vector-ref vec result)]
is greater than or equal to ꩜racket[val] (if there is none, then the upper
exclusive bound is returned).

꩜chunk[<dichotomy>
       (define/contract (find-≥ val vec start end)
         (->i ([val exact-nonnegative-integer?]
               [vec vector?] ;; (sorted-vectorof exact-nonnegative-integer? <)
               [start (vec) (integer-in 0 (sub1 (vector-length vec)))]
               [end (vec start) (integer-in (add1 start) (vector-length vec))])
              #:pre (val vec start) (or (= start 0)
                                        (< (vector-ref vec (sub1 start)) val))
              #:pre (val vec end) (or (= end (vector-length vec))
                                      (> (vector-ref vec end) val))
              [result (start end) (integer-in start end)])
         (if (= (- end start) 1) ;; there is only one element
             (if (>= (vector-ref vec start) val)
                 start
                 end)
             (let ()
               (define mid (ceiling (/ (+ start end) 2)))
               (if (>= (vector-ref vec mid) val)
                   (find-≥ val vec start mid)
                   (find-≥ val vec mid end)))))]

꩜;TODO: use a "spoiler" to hide the contents.
꩜chunk[#:save-as hidden:<idx→tree/ctc> <idx→tree/ctc>
       (-> #:none-wrapper (->d ([depth exact-nonnegative-integer?])
                               [result any/c])
           #:leaf-wrapper (->d ([i-in-vec exact-nonnegative-integer?]
                                [leaf-idx exact-nonnegative-integer?])
                               any)
           #:node-wrapper (->d ([left any/c]
                                [right any/c])
                               any)
           #:vec (vectorof exact-nonnegative-integer?)
           (-> #:depth exact-nonnegative-integer?
               #:vec-bounds exact-nonnegative-integer?
               #;|        | exact-nonnegative-integer?
               #:leaf-bounds exact-nonnegative-integer?
               #;|         | exact-nonnegative-integer?
               any/c))]

꩜section{Empty branches (branches only containing missing fields)}

For efficiency and to reduce memory overhead, we pre-define values for
branches of depth ꩜racket[_d ∈ (range 0 _n)] which only contain missing
fields.

꩜chunk[<empty-branches>
       ;; TODO: clean this up (use subtemplate).
       (define-syntax defempty
         (syntax-parser
           [(_ n:nat)
            #:with (empty1 emptyA …)
            (map (λ (depth)
                   (string->symbol (format "empty~a" (expt 2 depth))))
                 (range (add1 (syntax-e #'n))))
            #:with (emptyB … _) #'(empty1 emptyA …)
            #:with (empty1/τ emptyA/τ …) (stx-map λ.(format-id % "~a/τ" %)
                                                  #'(empty1 emptyA …))
            #:with (emptyB/τ … _) #'(empty1/τ emptyA/τ …)
            (syntax-local-introduce
             #'(begin (define-type empty1/τ 'none)
                      (define-type emptyA/τ (Pairof emptyB/τ emptyB/τ))
                      …
                      (define empty1 : empty1/τ 'none)
                      (define emptyA : emptyA/τ (cons emptyB emptyB))
                      …
                      (provide empty1 emptyA …
                               empty1/τ emptyA/τ …)))]))
       (defempty 10)]

꩜section{Creating a record builder given a set of field indices}

꩜chunk[<record-builder>
       (define-syntax record-builder
         (syntax-parser
           ;; depth ≥ 0. 0 ⇒ a single node, 1 ⇒ 2 nodes, 2 ⇒ 4 nodes and so on.
           [(_ depth:nat idx:nat …)
            #:when (not (check-duplicates (syntax->datum #'(idx …))))
            (define vec (list->vector (sort (syntax->datum #'(idx …)) <)))
            (define arg-vec (vector-map (λ (idx)
                                          (format-id #;TODO:FIXME: (syntax-local-introduce #'here)
                                                     "arg~a"
                                                     idx))
                                        vec))
            (define/with-syntax #(arg …) arg-vec)
            (define/with-syntax (arg/τ …) (stx-map (λ (arg)
                                                     (format-id arg "~a/τ" arg))
                                                   #'(arg …)))
            
            (define/with-syntax tree
              ((idx→tree <record-builder/wrappers>
                         #:vec vec)
               #:depth (syntax-e #'depth)
               #:vec-bounds 0 (vector-length vec)
               #:leaf-bounds 0 (expt 2 (syntax-e #'depth))))
            #'(λ #:∀ (arg/τ …) ([arg : arg/τ] …)
                tree)]))]

꩜hlite[<record-builder/wrappers> {-/ (_ = _ _ _ _ _ _ _ _ _ -/ _ vec)}
       (idx→tree
        #:none-wrapper λdepth.(format-id #'here "empty~a" (expt 2 depth))
        #:leaf-wrapper λi.idx.(vector-ref arg-vec i)
        #:node-wrapper λl.r.(list #'cons l r)
        #:vec vec)]

꩜section{Type of a record, with a multiple holes}

꩜; TODO: this is near identical to record-builder, refactor.
꩜chunk[<record-type>
       (define-type-expander record-type
         (syntax-parser
           [(_ depth:nat idx:nat …)
            #:when (not (check-duplicates (syntax->datum #'(idx …))))
            (define vec (list->vector (sort (syntax->datum #'(idx …)) <)))
            (define arg-vec (vector-map (λ (idx)
                                          (format-id #;TODO:FIXME: (syntax-local-introduce #'here)
                                                     "arg~a"
                                                     idx))
                                        vec))
            (define/with-syntax #(arg …) arg-vec)
            (define sidekicks-len 0)
            (define sidekicks '())
            (define/with-syntax tree
              ((idx→tree <record-type/wrappers>
                         #:vec vec)
               #:depth (syntax-e #'depth)
               #:vec-bounds 0 (vector-length vec)
               #:leaf-bounds 0 (expt 2 (syntax-e #'depth))))
            #`(∀ (arg … #,@sidekicks) tree)]))]

꩜hlite[<record-type/wrappers> {-/ (_ = _ _ _ _ _ _ _ _ _ -/ _ vec)}
       (idx→tree
        #:none-wrapper λdepth.(begin
                                (define sidekick (format-id #'here "row~a" sidekicks-len))
                                (set! sidekicks-len (add1 sidekicks-len))
                                (set! sidekicks (cons sidekick sidekicks))
                                sidekick)
        #:leaf-wrapper λi.idx.(vector-ref arg-vec i)
        #:node-wrapper λl.r.(list #'Pairof l r)
        #:vec vec)]

꩜chunk[<record-type>
       (begin-for-syntax
         (struct ρ-wrapper (id)
           #:property prop:procedure
           (λ (stx)
             (raise-syntax-error
              (if (and (stx-pair? stx) (identifier? (stx-car stx)))
                  (syntax-e (stx-car stx))
                  'ρ)
              "invalid use of row type variable"
              stx))))
       (define-syntax with-ρ
         (syntax-parser
           [(_ (ρ …) . body)
            #'(splicing-letrec-syntax ([ρ (ρ-wrapper #'ρ)]
                              …)
                . body)]))
       
       (define-for-syntax ρ-table (make-free-id-table))
       (define-type-expander ∀ρ
         (syntax-parser
           [(_ (A:id … #:ρ ρ:id …) τ)
            (for ([ρ (in-syntax #'(ρ …))])
              (free-id-table-set! ρ-table ρ <make-lifo-set>))
            (define expanded (expand-type #'(∀ (A …)
                                               (Let ([ρ (Pairof 'Hello 'ρ)]
                                                     …)
                                                 τ))))
            (define/syntax-parse ({~literal tr:∀} (A′ …) . τ′) expanded)
            ;; TODO: do this in the delayed part, after the whole program has
            ;; been expanded, so that we can know which fields ρ is used with.
            (define/syntax-parse (ρ′ …)
              (remove-duplicates
               (append* (for/list ([ρ (in-syntax #'(ρ …))])
                          (<lifo-set→list> (free-id-table-ref ρ-table ρ))))))
            (displayln #'(tr:∀ (A′ …) . τ′))
            (lift-delayed-module-end-declaration
             #'(define-type delayed-type
                 (∀ (A′ … ρ′ …) . τ′)))
            #'delayed-type]))

       (define-type-expander record
         (syntax-parser
           [(_ f:id … . {~or (#:ρ ρ:id) ρ:id})
            (define set
              (free-id-table-ref! ρ-table #'ρ (λ () <make-lifo-set>)))
            (for ([f (in-syntax #'(f …))])
              (<lifo-set-add!> set (syntax->datum f)))
            #'(List 'f … 'ρ)]))

       (define-syntax ρ-inst
         (syntax-parser
           [(_ f A:id … #:ρ ρ:id …)
            #'TODO]))]

꩜chunk[<make-lifo-set>
       ;; ordered free-identifier-set, last added = first in the order.
       (cons (box '()) (mutable-set))]

꩜chunk[<lifo-member?>
       (λ (s v) (set-member? (cdr s) v))]

꩜chunk[<lifo-set-add!>
       (λ (s v)
         (set-box! (car s) (cons v (unbox (car s))))
         (set-add! (cdr s) v))]

꩜chunk[<lifo-set→list>
       (λ (s) (unbox (car s)))]

꩜section{Type of a record, with a single hole}

In order to functionally update records, the updating functions will take a
tree where the type of a single leaf needs to be known. This of course means
that branches that spawn off on the path from the root have to be given a
polymorphic type, so that the result can have the same type for these branches
as the original value to update.

꩜CHUNK[<tree-type-with-replacement>
       (define-for-syntax (tree-type-with-replacement n last τ*)
         (define-values (next mod) (quotient/remainder n 2))
         (cond [(null? τ*) last]
               [(= mod 0)
                (tree-type-with-replacement next
                                            #`(Pairof #,last #,(car τ*))
                                            (cdr τ*))]
               [else
                (tree-type-with-replacement next
                                            #`(Pairof #,(car τ*) #,last)
                                            (cdr τ*))]))]

꩜section{Functionally updating a tree-record}

꩜subsection{Adding and modifying fields}

Since we only deal with functional updates of immutable records, modifying a
field does little more than discarding the old value, and injecting the new
value instead into the new, updated record.

Adding a new field is done using the same exact operation: missing fields are
denoted by a special value, ꩜racket['NONE], while present fields are
represented as instances of the polymorphic struct ꩜racket[(Some T)]. Adding a
new field is therefore as simple as discarding the old ꩜racket['NONE] marker,
and replacing it with the new value, wrapped with ꩜racket[Some]. A field
update would instead discard the old instance of ꩜racket[Some], and replace it
with a new one.

꩜CHUNK[<make-replace-in-tree-body>
       (if (= i 1)
           #'(delay/pure/stateless replacement)
           (let* ([bits (to-bits i)]
                  [next (from-bits (cons #t (cddr bits)))]
                  [mod (cadr bits)])
             (define/with-syntax next-id (vector-ref low-names (sub1 next)))
             (if mod
                 #`(replace-right (inst next-id #,@τ*-limited+T-next)
                                  tree-thunk
                                  replacement)
                 #`(replace-left (inst next-id #,@τ*-limited+T-next)
                                 tree-thunk
                                 replacement))))]

꩜CHUNK[<define-replace-in-tree>
       (define-pure/stateless
         (: replace-right (∀ (A B C R) (→ (→ (Promise B) R (Promise C))
                                          (Promise (Pairof A B))
                                          R
                                          (Promise (Pairof A C)))))
         (define
           #:∀ (A B C R)
           (replace-right [next-id : (→ (Promise B) R (Promise C))]
                          [tree-thunk : (Promise (Pairof A B))]
                          [replacement : R])
           (delay/pure/stateless
            (let ([tree (force tree-thunk)])
              (let ([left-subtree (car tree)]
                    [right-subtree (cdr tree)])
                (cons left-subtree
                      (force (next-id (delay/pure/stateless right-subtree)
                                      replacement))))))))
       (define-pure/stateless
         (: replace-left (∀ (A B C R) (→ (→ (Promise A) R (Promise C))
                                         (Promise (Pairof A B))
                                         R
                                         (Promise (Pairof C B)))))
         (define
           #:∀ (A B C R)
           (replace-left [next-id : (→ (Promise A) R (Promise C))]
                         [tree-thunk : (Promise (Pairof A B))]
                         [replacement : R])
           (delay/pure/stateless
            (let ([tree (force tree-thunk)])
              (let ([left-subtree (car tree)]
                    [right-subtree (cdr tree)])
                (cons (force (next-id (delay/pure/stateless left-subtree)
                                      replacement))
                      right-subtree))))))

       (define-for-syntax (define-replace-in-tree
                            low-names names rm-names τ* i depth)
         (define/with-syntax name (vector-ref names (sub1 i)))
         (define/with-syntax rm-name (vector-ref rm-names (sub1 i)))
         (define/with-syntax low-name (vector-ref low-names (sub1 i)))
         (define/with-syntax tree-type-with-replacement-name
           (gensym 'tree-type-with-replacement))
         (define/with-syntax tree-replacement-type-name
           (gensym 'tree-replacement-type))
         (define τ*-limited (take τ* depth))
         (define τ*-limited+T-next (if (= depth 0)
                                       (list #'T)
                                       (append (take τ* (sub1 depth))
                                               (list #'T))))
         #`(begin
             (provide name rm-name)
             (define-type (tree-type-with-replacement-name #,@τ*-limited T)
               (Promise #,(tree-type-with-replacement i #'T τ*-limited)))

             (define-pure/stateless
               (: low-name
                  (∀ (#,@τ*-limited T)
                     (→ (tree-type-with-replacement-name #,@τ*-limited Any)
                        T
                        (tree-type-with-replacement-name #,@τ*-limited T))))
               (define
                 #:∀ (#,@τ*-limited T)
                 (low-name [tree-thunk : (tree-type-with-replacement-name
                                          #,@τ*-limited Any)]
                           [replacement : T])
                 : (Promise #,(tree-type-with-replacement i #'T τ*-limited))
                 #,<make-replace-in-tree-body>))

             (: name
                (∀ (#,@τ*-limited T)
                   (→ (tree-type-with-replacement-name #,@τ*-limited Any)
                      T
                      (tree-type-with-replacement-name #,@τ*-limited
                                                       (Some T)))))
             (define (name tree-thunk replacement)
               (low-name tree-thunk (Some replacement)))
             
             (: rm-name
                (∀ (#,@τ*-limited)
                   (→ (tree-type-with-replacement-name #,@τ*-limited (Some Any))
                      (tree-type-with-replacement-name #,@τ*-limited 'NONE))))
             (define (rm-name tree-thunk)
               (low-name tree-thunk 'NONE))))]

꩜section{Auxiliary values}

The following sections reuse a few values which are derived from the list of
fields:

꩜CHUNK[<utils>
       (define all-fields #'(field …))
       (define depth-above (ceiling-log2 (length (syntax->list #'(field …)))))
       (define offset (expt 2 depth-above))
       (define i*-above (range 1 (expt 2 depth-above)))
       (define names (list->vector
                      (append (map (λ (i) (format-id #'here "-with-~a" i))
                                   i*-above)
                              (stx-map (λ (f) (format-id f "with-~a" f))
                                       #'(field …)))))
       (define rm-names (list->vector
                         (append (map (λ (i) (format-id #'here "-without-~a" i))
                                      i*-above)
                                 (stx-map (λ (f) (format-id f "without-~a" f))
                                          #'(field …)))))
       (define low-names (list->vector
                          (append (map (λ (i) (format-id #'here "-u-with-~a" i))
                                       i*-above)
                                  (stx-map (λ (f) (format-id f "u-with-~a" f))
                                           #'(field …)))))]

꩜section{Type of a tree-record}

꩜CHUNK[<τ-tree-with-fields>
       (define-for-syntax (τ-tree-with-fields struct-fields fields)
         (define/with-syntax (struct-field …) struct-fields)
         (define/with-syntax (field …) fields)
         <utils>
         ;; Like in convert-from-struct
         (define lookup
           (make-free-id-table
            (for/list ([n (in-syntax all-fields)]
                       [i (in-naturals)])
              (cons n (+ i offset)))))
         (define fields+indices
           (sort (stx-map λ.(cons % (free-id-table-ref lookup %))
                          #'(struct-field …))
                 <
                 #:key cdr))
  
         (define up (* offset 2))

         ;; Like in convert-fields, but with Pairof
         (define (f i)
           ;(displayln (list i '/ up (syntax->datum #`#,fields+indices)))
           (if (and (pair? fields+indices) (= i (cdar fields+indices)))
               (begin0
                 `(Some ,(caar fields+indices))
                 (set! fields+indices (cdr fields+indices)))
               (if (>= (* i 2) up) ;; DEPTH
                   ''NONE
                   (begin
                     `(Pairof ,(f (* i 2))
                              ,(f (add1 (* i 2))))))))
         (f 1))]

꩜section{Conversion to and from record-trees}

꩜CHUNK[<define-struct↔tree>
       (define-for-syntax (define-struct↔tree
                            offset all-fields τ* struct-name fields)
         (define/with-syntax (field …) fields)
         (define/with-syntax fields→tree-name
           (format-id struct-name "~a→tree" struct-name))
         (define/with-syntax tree→fields-name
           (format-id struct-name "tree→~a" struct-name))
         (define lookup
           (make-free-id-table
            (for/list ([n (in-syntax all-fields)]
                       [i (in-naturals)])
              (cons n (+ i offset)))))
         (define fields+indices
           (sort (stx-map λ.(cons % (free-id-table-ref lookup %))
                          fields)
                 <
                 #:key cdr))
         #`(begin
             (: fields→tree-name (∀ (field …)
                                    (→ field …
                                       (Promise
                                        #,(τ-tree-with-fields #'(field …)
                                                              all-fields)))))
             (define (fields→tree-name field …)
               (delay/pure/stateless
                #,(convert-fields (* offset 2) fields+indices)))

             (: tree→fields-name (∀ (field …)
                                    (→ (Promise
                                        #,(τ-tree-with-fields #'(field …)
                                                              all-fields))
                                       (Values field …))))
             (define (tree→fields-name tree-thunk)
               (define tree (force tree-thunk))
               #,(convert-back-fields (* offset 2) fields+indices))))]

꩜subsection{Creating a new tree-record}

꩜CHUNK[<convert-fields>
       (define-for-syntax (convert-fields up fields+indices)
         ;(displayln fields+indices)
         (define (f i)
           ;(displayln (list i '/ up (syntax->datum #`#,fields+indices)))
           (if (and (pair? fields+indices) (= i (cdar fields+indices)))
               (begin0
                 `(Some ,(caar fields+indices))
                 (set! fields+indices (cdr fields+indices)))
               (if (>= (* i 2) up) ;; DEPTH
                   ''NONE
                   `(cons ,(f (* i 2))
                          ,(f (add1 (* i 2)))))))
         ;(displayln (syntax->datum #`#,(f 1)))
         (f 1))]


꩜subsection{Extracting all the fields from a tree-record}

We traverse the tree in preorder, and accumulate definitions naming the
interesting subparts of the trees (those where there are fields).

꩜CHUNK[<convert-back-fields>
       (define-for-syntax (convert-back-fields up fields+indices)
         (define result '())
         (define definitions '())
         (define (f i t)
           (if (and (pair? fields+indices) (= i (cdar fields+indices)))
               (begin0
                 (begin
                   (set! result (cons #`(Some-v #,t) result))
                   #t)
                 (set! fields+indices (cdr fields+indices)))
               (if (>= (* i 2) up) ;; DEPTH
                   #f
                   (let* ([left-t (string->symbol
                                   (format "subtree-~a" (* i 2)))]
                          [right-t (string->symbol
                                    (format "subtree-~a" (add1 (* i 2))))]
                          [left (f (* i 2) left-t)]
                          [right (f (add1 (* i 2)) right-t)])
                     (cond
                       [(and left right)
                        (set! definitions (cons #`(define #,left-t (car #,t))
                                                definitions))
                        (set! definitions (cons #`(define #,right-t (cdr #,t))
                                                definitions))
                        #t]
                       [left
                        (set! definitions (cons #`(define #,left-t (car #,t))
                                                definitions))
                        #t]
                       [right
                        (set! definitions (cons #`(define #,right-t (cdr #,t))
                                                definitions))
                        #t]
                       [else
                        #f])))))
         (f 1 #'tree)
         #`(begin #,@definitions (values . #,(reverse result))))]

꩜section{Defining the converters and accessors for each known record type}

꩜CHUNK[<define-trees>
       (define-for-syntax (define-trees stx)
         (syntax-case stx ()
           [(bt-fields-id (field …) [struct struct-field …] …)
            (let ()
              <utils>
              (define ∀-types (map λ.(format-id #'here "τ~a" %)
                                   (range (add1 depth-above))))
              (define total-nb-functions (vector-length names))
              <define-trees-result>)]))]

꩜CHUNK[<bt-fields-type>
       (define-for-syntax (bt-fields-type fields)
         (λ (stx)
           (syntax-case stx ()
             [(_ . fs)
              #`(∀ fs (Promise #,(τ-tree-with-fields #'fs
                                                     fields)))])))]

꩜CHUNK[<define-trees-result>
       #`(begin
           (define-type-expander bt-fields-id
             (bt-fields-type #'#,(syntax-local-introduce #'(field …))))
           #,@(map λ.(define-replace-in-tree low-names
                       names rm-names ∀-types % (floor-log2 %))
                   (range 1 (add1 total-nb-functions)))
           #;#,@(map λ.(define-remove-in-tree rm-names ∀-types % (floor-log2 %))
                     (range 1 (add1 total-nb-functions)))
           #,@(map λ.(define-struct↔tree
                       offset all-fields ∀-types %1 %2)
                   (syntax->list #'(struct …))
                   (syntax->list #'([struct-field …] …))))]

꩜subsection{Putting it all together}

꩜chunk[<maybe>
       (struct (T) Some ([v : T]) #:transparent)
       (define-type (Maybe T) (U (Some T) 'NONE))]

꩜chunk[<*>
       (require delay-pure
                "flexible-with-utils.hl.rkt"
                (for-syntax (rename-in racket/base [... …])
                            syntax/parse
                            syntax/stx
                            racket/syntax
                            racket/list
                            syntax/id-table
                            racket/set
                            racket/sequence
                            racket/vector
                            racket/contract
                            type-expander/expander)
                (for-meta 2 racket/base)
                (only-in phc-graph/xtyped lift-delayed-module-end-declaration)
                (prefix-in tr: (only-in typed/racket ∀))
                racket/splicing)

       (provide (for-syntax define-trees)
                ;; For tests:
                (struct-out Some)

                ;;DEBUG:
                (for-syntax τ-tree-with-fields)
                record-builder
                ∀ρ
                with-ρ
                record)
       
       <maybe>
       <tree-type-with-replacement>
       <define-replace-in-tree>
       ;<define-remove-in-tree>
       <convert-fields>
       <convert-back-fields>
       <τ-tree-with-fields>
       <define-struct↔tree>
       <define-trees>
       <bt-fields-type>
       (begin-for-syntax
         <dichotomy>
         <idx→tree>)
       <empty-branches>
       <record-builder>
       <record-type>]

꩜include-section[(submod "flexible-with-utils.hl.rkt" doc)]