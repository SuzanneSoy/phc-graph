#lang hyper-literate #:꩜ #:no-auto-require (dotlambda/unhygienic
                                            . type-expander/lang)

꩜title[#:tag "generalized-flex-ctor"]{Generalised constructor functions for
 flexible structures}

꩜(require hyper-literate/diff1)
꩜(init)

꩜chunk[<*>
       (provide builder-τ
                None
                Some
                Some?
                Some-f
                propagate-τ)

       (require racket/require
                (for-syntax (subtract-in racket/base subtemplate/override)
                            syntax/stx
                            racket/list
                            racket/function
                            subtemplate/override)
                (for-meta 2 racket/base))

       (struct (T) Some ([f : T]))
       (struct (T) None ([f : T]))

       (define-type-expander BinaryTree
         (syntax-parser
           [(_ leafⱼ …)
            ;; TODO: implement BinaryTree.
            #'(List leafⱼ …)]))

       (define-syntax (def-SomeNone* stx)
         (syntax-case stx ()
           [(_ Some n)
            (with-syntax ([(Someᵢ …) (map (λ (i) (format-id #'Some "Some~a" i))
                                          (range n))])
              #`(begin
                  (provide Someᵢ …)
                  (struct (T) Someᵢ Some ()) …))]))

       (def-SomeNone* Some 4)

       <builder-τ>

       <propagate-τ>]

We first define the builder function's type. Since this type is rather
complex, we define it using a macro. The type expander takes two arguments.
The first argument ꩜racket[n] indicates the total number of fields which
appear in the program (i.e. on the number of leaves of the generated tree
type), and the second argument ꩜racket[m] indicates how many key/value pairs
the function accepts.

꩜chunk[<builder-τ>
       (define-type-expander builder-τ
         (syntax-parser
           [(_ n m)
            <builder-τ-with-1>
            <builder-τ-with-2>
            <builder-τ-with-3>
            #'|<builder-function-type''>|]))]

We start by defining a few syntax pattern variables which will be used in the
later definitions. The lists ꩜racket[Nᵢ] and ꩜racket[Mⱼ] range over the field
and argument indices, respectively:

꩜chunk[<builder-τ-with-1>
       #:with (Nᵢ …) (range n)
       #:with (Mⱼ …) (range m)]

The builder function takes a number of keys and values, and builds a (promise
for) a binary tree where the leaves corresponding to those keys contain given
value, and other leaves contain ꩜racket[None]. We could write (a simplified
form of) the builder function type as follows:

꩜chunk[<builder-function-type>
       (∀ ({?@ Kⱼ Xⱼ} …)
          (→ (code:comment "; Keys and values:")
             {?@ (∩ Kⱼ (U 'NSymⱼᵢ …)) Xⱼ} …
             ;; Result type:
             (BinaryTree |<Some or None>| …)))]

We expect each key ꩜racket[Kⱼ] to be a symbol of the shape ꩜racket[|0|],
꩜racket[|1|], ꩜racket[|2|] and so on:

꩜chunk[<builder-τ-with-2>
       #:with (NSymᵢ …) ((string->symbol (format "~a" Nᵢ)) …)
       #:with ((NSymⱼᵢ …) …) (map (const (NSymᵢ …)) (Mⱼ …))]

The type of each leaf of the binary tree should be ꩜racket[(Some Xⱼ)] if a
corresponding ꩜racket[Kⱼ] matches the leaf name, and ꩜racket[None] otherwise.

꩜chunk[|<Some or None>|
       (U |<(Some Xⱼ) if Kⱼ = NSymᵢ>|
          |<None if ∀ k ∈ Kⱼ, k ≠ NSymᵢ>|)]

This type-level conditional is achieved via a trick involving intersection
types. The ꩜racket[Kⱼ] type should be a singleton type containing exactly one
of the ꩜racket['NSymᵢ …] symbols. For a given leaf with index ꩜racket[i], if
the ꩜racket[Kⱼ] key is the type ꩜racket['NSymᵢ], then the intersection type
꩜racket[(∩ Kⱼ 'NSymᵢ)] is ꩜racket['NSymᵢ]. Conversely, if the ꩜racket[Kⱼ] key
is not ꩜racket['NSymᵢ], the intersection will be the bottom type
꩜racket[Nothing]. No values inhabit the bottom type, and Typed Racket can
determine that there is no pair whose first (or second) element has the type
꩜racket[Nothing], since no concrete value could be used to construct such a
pair.

꩜chunk[|<(Some Xⱼ) if Kⱼ = NSymᵢ>|
       (Pairof (∩ Kᵢⱼ 'NSymᵢⱼ)
               (Some Xᵢⱼ))
       …]

where ꩜racket[Kᵢⱼ], ꩜racket[Xᵢⱼ] and ꩜racket[NSymᵢⱼ] are defined as follows:

꩜chunk[<builder-τ-with-3>
       #:with ((Kᵢⱼ …) …) (map (const (Kⱼ …)) (Nᵢ …))
       #:with ((Xᵢⱼ …) …) (map (const (Xⱼ …)) (Nᵢ …))
       #:with ((NSymᵢⱼ …) …) (map λni.(map (const ni) (Xⱼ …)) (NSymᵢ …))]

We use this fact to construct a pair above. Its first element is either
꩜racket['NSymᵢ] when ꩜racket[Kⱼ] is ꩜racket['NSymᵢ], and ꩜racket[Nothing]
otherwise. The second element of the pair contains our expected
꩜racket[(Some Xⱼ)] type, but the whole pair is collapsed to ꩜racket[Nothing]
when ꩜racket[Kⱼ] is not ꩜racket['NSymᵢ].

We use a similar approach to conditionally produce the ꩜racket[None] element,
but instead of intersecting ꩜racket[Kⱼ] with ꩜racket['NSymᵢ], we intersect it
with the complement of ꩜racket['NSymᵢ]. Typed Racket lacks the possibility to
negate a type, so we manually compute the complement of ꩜racket['NSymᵢ] in the
set of possible keys (that is, ꩜racket['NSymᵢ …]).

꩜chunk[<builder-τ-with-3>
       #:with NSyms (NSymᵢ …)
       #:with Ms (Mⱼ …)
       #:with (exceptᵢ …) ((remove NSymᵢ NSyms) …)
       #:with (((exceptᵢⱼ …) …) …) ((map (const exceptᵢ) Ms) …)]

If ꩜racket[Kⱼ] is ꩜racket[Nᵢ], then ꩜racket[{∩ Kⱼ {U . exceptᵢ}}] will be
꩜racket[Nothing]. We take the Cartesian product of the intersections by
building a ꩜racket[List] out of them. A single occurrence of ꩜racket[Nothing]
will collapse the whole list to ꩜racket[Nothing], because the Cartesian
product of the empty set and any other set will produce the empty set.

The resulting type should therefore be ꩜racket[Nothing] only if there is no
꩜racket[Kⱼ] equal to ꩜racket[Nᵢ], and be the list of symbols
꩜racket[(List . exceptᵢ)] otherwise.

꩜chunk[|<None if ∀ k ∈ Kⱼ, k ≠ NSymᵢ>|
       (Pairof 'NSymᵢ
               (None (List {∩ Kᵢⱼ {U 'exceptᵢⱼ …}} …)))]

This approach relies on the fact that occurrences of ꩜racket[Nothing] within
structs and pairs containing collapse the entire struct or pair type to
꩜racket[Nothing]. Unfortunately, current versions of Typed Racket perform this
simplification step in some situations but not others:

꩜itemlist[
 ꩜item{It simplifies polymorphic types when they are defined;}
 ꩜item{When a polymorphic type is instantiated, the parts which are directly
  affected by the intersection with a polymorphic type variable are subject to
  this simplification;}
 ꩜item{However, occurrences of ꩜racket[Nothing] which occur as a result of
  instantiating the type variable do not propagate outside of the intersection
  itself. This means that given the following type:
  ꩜racketblock[(define-type (Foo A) (U (Pairof (∩ Integer A) String) Boolean))]
  its instantiation ꩜racket[(Foo Symbol)] will produce the type
  ꩜racket[(U (Pairof Nothing String) Boolean)], but this type will not be
  simplified to ꩜racket[(U Nothing Boolean)] or equivalently to
  ꩜racket[Boolean].}]

To force Typed Racket to propagate ꩜racket[Nothing] outwards as much as
we need, we intersect the whole form with a polymorphic type ꩜racket[A]:

꩜hlite[|<builder-function-type'>|
       {/(∀(+ _ / _ _)(→ _ _ ooo (bt + (∩ / _ + A) / _)))}
       (∀ (A {?@ Kⱼ Xⱼ} …)
          (→ (code:comment "; Keys and values:")
             {?@ (∩ Kⱼ (U 'NSymⱼᵢ …)) Xⱼ} …
             ;; Result type:
             (BinaryTree (∩ |<Some or None>| A) …)))]

The type ꩜racket[propagate-τ] defined below is used to instantiate ꩜racket[A],
and is carefully picked so that its intersection will in no way change the
result type (i.e. the intersection with ꩜racket[A] will be an identity
operation where it is used). In other words, ꩜racket[propagate-τ] has the same
shape as the leaves of the binary tree. This intersection however forces the
simplification step to be performed on the affected parts once the type is
instantiated.

꩜chunk[<propagate-τ>
       (define-type propagate-τ
         (Pairof Any
                 (U (None (Listof Natural))
                    (Some Any))))]

꩜;Use chunkref instead of ꩜racket[|<Some or None>|] ?

The implementation of the builder function will need to convert values with
the ꩜racket[|<Some or None>|] type to values of type
꩜racket[(∩ |<Some or None>| A)]. Since the intersection could, in principle,
be any subtype of ꩜racket[|<Some or None>|], we request that the caller
supplies a proof that the conversion is possible. This proof takes the form of
an ꩜racket[oracle] function, which transforms an element with the type
꩜racket[propagate-τ] (which is a supertype of every possible
꩜racket[|<Some or None>|] type) into an element with the type
꩜racket[(∩ A B)], where ꩜racket[B] is the original type of the value to
upgrade.

꩜chunk[<oracle-type>
       (∀ (B) (→ (∩ B
                    (Pairof Any (U (Some Any) (None (Listof Any)))))
                 (∩ A B)))]

The builder function type is updated as follows:

꩜hlite[|<builder-function-type''>|
       {/(∀(_ _ _)(→ + _ _ / _ _ ooo _))}
       (∀ (A {?@ Kⱼ Xⱼ} …)
          (→ (code:comment "; Oracle:")
             <oracle-type>
             (code:comment "; Keys and values:")
             {?@ (∩ Kⱼ (U 'NSymⱼᵢ …)) Xⱼ} …
             ;; Result type:
             (BinaryTree (∩ |<Some or None>| A) …)))]



