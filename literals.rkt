#lang racket

(define-syntax-rule (provide-literals name ...)
  (begin
    (provide name ...)
    (define-syntax (name stx)
      (raise-syntax-error 'name
                          "can only be used in some special contexts"
                          stx))
    ...))

(provide-literals mapping node)