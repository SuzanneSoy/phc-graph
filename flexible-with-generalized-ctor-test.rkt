#lang type-expander
(require "flexible-with-generalized-ctor.hl.rkt"
         "binarytree.hl.rkt")
(provide f-4-2 f-8-3)

(builder-f f-4-2 4 2)

(ann ((inst f-4-2 propagate-τ '|1| Number '|3| String)
      oracle '|1| 100 '|3| "bee")
     (BinaryTree
      (Promise (Pairof #f Any))
      (Promise (Pairof '|1| Number))
      (Promise (Pairof #f Any))
      (Promise (Pairof '|3| String))))

(builder-f f-8-3 8 3)

(ann ((inst f-8-3 propagate-τ '|1| Number '|3| String '|7| Symbol)
      oracle '|1| 100 '|3| "bee" '|7| 'buzz)
     (BinaryTree
      (Promise (Pairof #f Any))
      (Promise (Pairof '|1| Number))
      (Promise (Pairof #f Any))
      (Promise (Pairof '|3| String))
      (Promise (Pairof #f Any))
      (Promise (Pairof #f Any))
      (Promise (Pairof #f Any))
      (Promise (Pairof '|7| Symbol))))