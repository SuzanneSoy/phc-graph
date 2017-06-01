#lang typed/racket
(require syntax/parse
         phc-toolkit/percent
         (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-class parse-args
    (pattern rest:id
             #:with sig #'rest
             #:with {extract ...} #'{})
    (pattern ({~or {~literal syntax} {~literal unsyntax}} . rest)
             ;; The rest would need to be an stx, but that's normally impossible
             ;; (unless there was some extension to #%app and function
             ;; definitions which allowed this).
             #:with sig (raise-syntax-error
                         "Unexpected syntax pattern in a tail position")
             #:with {extract ...} #'{})
    (pattern (({~literal syntax} hd) . tl:parse-args)
             #:with (tmp) (generate-temporaries #'(hd))
             #:with sig #'(tmp . tl.sig)
             #:with {extract ...} #`{#'hd = tmp tl.extract ...})
    (pattern (({~literal unsyntax} hd:parse-args) . tl:parse-args)
             #:with (tmp) (generate-temporaries #'(hd))
             #:with sig #'(tmp . tl.sig)
             #:with {extract ...} #'{hd.sig = (syntax->datum tmp)
                                            hd.extract ...
                                            tl.extract ...})
    (pattern (hd:id . tl:parse-args)
             #:with sig #'(hd . tl.sig)
             #:with {extract ...} #'{tl.extract ...})
    (pattern {~and last ()}
             #:with sig #'last
             #:with {extract ...} #'{})))
  
(define-syntax def
  (syntax-parser
    [(_ ({~literal syntax} name:id) . body)
     #'(define-syntax name
         (syntax-parser
           [self:id . body]))]
    [(_ (name:id . args:parse-args) . body)
     #`(define (name . args.sig)
         (% args.extract ...
            . body))]))

#;{
   (def (foo #'(aa . bb) x)
     #`(#,x bb . aa))

   (def (bar #,(aa . bb) x)
     (list x bb aa))

   (module+ test
     (require rackunit)
     (check-equal? (syntax->datum
                    (foo #'(xx . yy) 42))
                   '(42 yy . xx)))
   }