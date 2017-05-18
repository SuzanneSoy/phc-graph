#lang type-expander

(provide BinaryTree)

(require (for-syntax syntax/parse
                     phc-toolkit/aliases))

(define-type-expander BinaryTree
  (syntax-parser
    [(_ leafⱼ …)
     ;; TODO: implement BinaryTree.
     #'(List leafⱼ …)]))