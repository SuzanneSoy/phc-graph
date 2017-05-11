#lang at-exp racket/base

(module reader syntax/module-reader
  phc-graph/make-lang)

(provide (rename-out [-#%module-begin #%module-begin]))

(require (for-syntax racket/base
                     setup/collects)
         scribble/manual)

(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(self #:require req)
     ;; TODO: isn't there a more reliable way to get the "require path"
     ;;       for the source module of #'self ?
     (let* ([src (syntax-source #'self)]
            [modpath (path->module-path src)]
            [md (if (and (pair? modpath)
                         (eq? (car modpath) 'lib)
                         (string? (cadr modpath))
                         (null? (cddr modpath))
                         (regexp-match ".rkt$" (cadr modpath)))
                    (string->symbol
                     (substring (cadr modpath)
                                0
                                (- (string-length (cadr modpath)) 4)))
                    modpath)])
       #`(-#%module-begin #:module #,md #:require req))]
    [(_ #:module md ;; TODO: detect this automatically
        #:require (req ...))
     #`(#%module-begin
        (module reader syntax/module-reader
          md)
        @module[scrbl racket/base (require scribble/manual)]{
      @defmodule[md]{
       This module language re-provides the following modules:
       @itemlist[(item (racketmodname req)) ...]
      }
     }
        (module doc racket/base
          (require (submod ".." scrbl))
          (provide (all-from-out (submod ".." scrbl))))
        (require req ...)
        (provide (all-from-out req ...)))]))

