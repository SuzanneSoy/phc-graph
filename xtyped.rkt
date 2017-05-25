#lang racket

(module reader syntax/module-reader
  phc-graph/xtyped)

(require type-expander/lang)

(provide (rename-out [-#%module-begin #%module-begin])
         (except-out (all-from-out type-expander/lang)
                     #%module-begin)
         (for-syntax lift-delayed-module-end-declaration))

(define-for-syntax lifted (make-parameter #f))
(define-for-syntax (lift-delayed-module-end-declaration decl)
  (set-box! (lifted) (cons decl (unbox (lifted))))
  (void))

(define-syntax (-#%module-begin-2 stx)
  (syntax-case stx ()
    [(_ -mb . more)
     (with-syntax ([((lifted-def ...) (pmb . rest))
                    (parameterize ([lifted (box '())])
                      (define expanded
                        (local-expand (datum->syntax
                                       stx
                                       `(,#'#%plain-module-begin . ,#'more)
                                       stx
                                       stx)
                                      'module-begin
                                      '()))
                      (list (map syntax-local-introduce (unbox (lifted)))
                            expanded))])
       #`(begin
           (define-type #,(datum->syntax #'-mb 'T) Integer)
           lifted-def ...
           . rest))]))
(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(-mb . more)
     #'(#%module-begin (-#%module-begin-2 -mb . more))]))
