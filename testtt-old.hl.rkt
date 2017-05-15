#lang hyper-literate #:♦ (dotlambda/unhygienic . type-expander/lang)

♦title{testttt}

♦(require (for-syntax syntax/parse
                      (rename-in racket/base [... …]))
          scribble/core
          scribble/html-properties)
♦(elem
  #:style (style "dim"
                 (list (css-addition
                        #"
.dim > *:not(.undim) {
  style: \"brightness(150%) contrast(50%) opacity(0.7)\";
  background: rgba(255, 255, 255, 0.36);
}

.undim {
  style: initial;
  background: none;
}")

                       (js-addition
                        #"
(function(f) {
  // A 'simple' onLoad function
  if (window.document.readyState == 'complete') {
    f();
  } else if (window.document.addEventListener) {
    window.document.addEventListener('DOMContentLoaded', f, false);
  } else if (window.attachEvent) {
    window.attachEvent('onreadystatechange', function() {
      if (window.document.readyState == 'complete') {
        f();
      }
    });
  } else {
    var oldLoad = window.onload;
    if (typeof(oldLoad) == 'function') {
      window.onload = function() {
        try {
          oldLoad();
        } finally {
          f();
        }
      };
    } else {
      window.onload = f;
    }
  }
})(function() {
  for (var starti = 0; starti < dims.length; starti++) {
    (function (){
      var start = dims.item(starti);
      while (!start.classList.contains('SCodeFlow')){
        start = start.parentNode;
        if (start == document) {
          return; // abort for this node
        }
      }
      var loopfn = function(mode, e) {
        var chs = e.childNodes;
        for (var chi = 0; chi < chs.length; chi++) {
          var ch = chs.item(chi);
          if (ch.classList !== undefined) {
            if (ch.classList.contains('dim')) {
              mode = 'dim';
              continue;
            } else if (ch.classList.contains('undim')) {
              mode = 'undim';
              continue;
            } else {
              ch.classList.add('el-' + mode);
              mode = loopfn(mode, ch);
            }
          }
        }
        return mode;
      }
      loopfn('undim', start);
    })();
  }
});
"))))

♦chunk[<a>
       (define x 1)]

♦(define-syntax-rule (patch-<a> new-name pat new)
   (begin
     (define-syntax (do-it stx)
       (syntax-parse #'(define x 1)
         [pat #`(chunk new-name new)]))
     (do-it)))

♦patch-<a>[<a2>
           (def n v)
           (def n (+ v 2))]


♦(begin-for-syntax
   #;(define-splicing-syntax-class dim (pattern {~seq}))
   #;(define-splicing-syntax-class undim (pattern {~seq}))

   (define (dim-elt last-known)
     (datum->syntax #'here
                    (syntax->datum #'#,(elem #:style "dim"))
                    last-known))
   (define (undim-elt last-known)
     (datum->syntax #'here
                    (syntax->datum #'#,(elem #:style "undim"))
                    last-known))

   (define-syntax-class pat
     #:attributes (xpat xres)
     (pattern {~literal :dim}
              #:with xpat #'{~seq}
              #:with xres #'#,(dim-elt))
     (pattern {~literal :undim}
              #:with xpat #'{~seq}
              #:with xres #'#,(undim-elt))
     (pattern {~literal …}
              #:with xpat #'(… …)
              #:with xres #'xpat)
     (pattern x:id
              #:with (xpat) (generate-temporaries #'(x))
              #:with _ (begin (displayln (list #'x #'xpat)) 0)
              #:with xres #'xpat)
     (pattern (p:pat …)
              #:with (whole) (generate-temporaries #'(whole))
              #:with xpat #'{~and whole (p.xpat …)}
              #:with xres #'#,(quasisyntax/loc #'whole (p.xres …)))))

♦(define-syntax (hlite stx)
   (define (stx-or-first b)
     (if (syntax? b)
         b
         (car b)))
   (syntax-case stx ()
     [(self name guide1 . body)
      (and (identifier? #'self)
           (identifier? #'name))
      (let loop ([guide #'(_ _ . guide1)]
                 [body (datum->syntax
                        stx
                        `(,(datum->syntax #'here 'chunk #'self)
                          ,#'name
                          . ,(syntax-e #'body))
                        stx)])
        (cond
          [(and (identifier? guide)
                (free-identifier=? guide (quote-syntax …)))
           (raise-syntax-error 'hlite
                               "ellipses are not supported (yet)"
                               guide)]
          [(and (identifier? guide)
                (free-identifier=? guide #':dim))
           (dim-elt (stx-or-first body))]
          [(and (identifier? guide)
                (free-identifier=? guide #':undim))
           (undim-elt (stx-or-first body))]
          [(identifier? guide)
           body]
          [(syntax? guide)
           ;; TODO: probably not the best course of action (we're
           ;; loosing some of the stx-list vs stx-pair information
           ;; here).
           (loop (syntax-e guide) body)]
          ;; TODO: check that this never lets "body" be something else
          ;; than a syntax object or a list starting with a syntax
          ;; object.
          [(and (pair? guide)
                (identifier? (car guide))
                (or (free-identifier=? (car guide) #':dim)
                    (free-identifier=? (car guide) #':undim)))
           (cons (loop (car guide) body)
                 (loop (cdr guide) body))]
          [(syntax? body)
           (datum->syntax body
                          (loop guide (syntax-e body))
                          body
                          body)]
          [(pair? guide)
           (unless (pair? body)
             (raise-syntax-error
              'hlite
              (format
               "expected a pair, as indicated by the pattern ~a"
               (syntax->datum (datum->syntax #f guide)))
              body
              #f))
           (cons (loop (car guide) (car body))
                 (loop (cdr guide) (cdr body)))]
          [else
           body]))])

   ;; Old implementation. Does not get the srclocs right for the escaped parts.
   #;(syntax-parser
       [(self name :pat . body)
        (displayln this-syntax)
        (local-require racket/pretty)
        (define result
          #`(begin
              #,#'(define-syntax do-it
                    (syntax-parser
                      [(self2 . xpat)
                       #:with chk (syntax/loc #'self2 chunk)
                       (displayln (quasisyntax/loc this-syntax
                                    (chk name . xres)))
                       #`(elem #:style "highlightable"
                               #,(quasisyntax/loc this-syntax
                                   (chk name . xres)))]))
              #,(quasisyntax/loc this-syntax
                  (#,(syntax/loc #'self do-it) . body))))
        (pretty-print (syntax->datum result))
        result]))

♦hlite[<c> {:dim _ :undim _}
       a b]

♦hlite[<c> {:dim (def args :undim _ :dim _ _) a}
       (define (x v)
         (+ 1 1)
         (2 2 2) 3)
       42]

♦chunk[<*>
       <a>]