#lang type-expander
(require (lib "phc-graph/flexible-with-generalized-ctor.hl.rkt"))
(provide f g)

(builder-f f 4 2)

#;((inst f propagate-τ '|1| Number '|3| String)
   oracle '|1| 100 '|3| "bee")

(builder-f g 8 3)

#;((inst g propagate-τ '|1| Number '|3| String '|7| Symbol)
   oracle '|1| 100 '|3| "bee" '|7| 'buzz)