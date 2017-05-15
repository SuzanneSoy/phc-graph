#lang hyper-literate #:♦ (dotlambda/unhygienic . racket/base)

♦title{testttt}

♦(require (for-syntax syntax/parse
                      (rename-in racket/base [... …]))
          scribble/core
          scribble/html-properties)
♦(elem
  #:style (style "dim"
                 (list (css-addition
                        #"
.el-dim {
  filter: brightness(150%) contrast(30%) opacity(0.7);
  background: none; /* rgba(82, 103, 255, 0.36); */
}

.el-hliteadd{
  filter: initial;
  background: rgba(108, 175, 108, 0.36);
}

.el-hliterm {
  filter: initial;
  background: rgba(173, 54, 54, 0.36);
}

.el-undim {
  filter: initial;
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
  var process = function (startmode, start){
    if (!(start.classList.contains(startmode))) {
      return; // early return, we have already handled this element.
    }
    
    while (!(start.classList.contains('SCodeFlow'))){
      start = start.parentNode;
      if (start == document) {
        return; // abort for this node
      }
    }
    var loopfn = function(mode, e) {
      var chs = e.childNodes;
      var copychs = [];
      // Copy the array-ish in cas replacing elements causes JS to update it.
      for (var chi = 0; chi < chs.length; chi++) {
        copychs[chi] = chs.item(chi);
      }
      for (var chi = 0; chi < copychs.length; chi++) {
        var ch = copychs[chi];
        if (ch.nodeType == Node.ELEMENT_NODE && ch.classList !== undefined) {
          if (ch.classList.contains('dim')) {
            ch.classList.remove('dim');
            mode = 'dim';
            continue;
          } else if (ch.classList.contains('hliteadd')) {
            ch.classList.remove('hliteadd');
            mode = 'hliteadd';
            continue;
          } else if (ch.classList.contains('hliterm')) {
            ch.classList.remove('hliterm');
            mode = 'hliterm';
            continue;
          } else if (ch.classList.contains('undim')) {
            ch.classList.remove('undim');
            mode = 'undim';
            continue;
          } else {
            mode = loopfn(mode, ch);
          }
        } else if (ch.nodeType == Node.TEXT_NODE) {
            // Replace the text node with a span with the correct class.
            var s = document.createElement('span');
            s.classList.add('el-' + mode);
            e.replaceChild(s, ch);
            s.appendChild(ch);
        }
      }
      return mode;
    }
    loopfn('undim', start);
  };

  var outerloop = function(mode){
    var listish = document.getElementsByClassName(mode);
    var copylist = [];
    for (var starti = 0; starti < listish.length; starti++) {
      // We must copy the list because it is automatically updated
      // when mutating the DOM.
      copylist[starti] = listish.item(starti);
    }
    for (var starti = 0; starti < copylist.length; starti++) {
      process(mode, copylist[starti]);
    }
  }
  
  outerloop('dim');
  outerloop('hliteadd');
  outerloop('hliterm');
  outerloop('undim');
});
"))))


♦(begin-for-syntax
   (define (dim-elt loc)
     (datum->syntax #'here (syntax->datum #'#,(elem #:style "dim")) loc))
   (define (undim-elt loc)
     (datum->syntax #'here (syntax->datum #'#,(elem #:style "undim")) loc))
   (define (hliteadd-elt loc)
     (datum->syntax #'here (syntax->datum #'#,(elem #:style "hliteadd")) loc))
   (define (hliterm-elt loc)
     (datum->syntax #'here (syntax->datum #'#,(elem #:style "hliterm")) loc))

   ;; Old implementation. Does not get the srclocs right for the escaped parts.
   #;(define-syntax-class pat
       #:attributes (xpat xres)
       (pattern {~literal /}
                #:with xpat #'{~seq}
                #:with xres #'#,(dim-elt))
       (pattern {~literal =}
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
      (let ()
        (define disp
          (let loop ([guide #'(_ _ _ . guide1)]
                     [body (datum->syntax
                            stx
                            `(,(datum->syntax #'here 'chunk #'self)
                              #:display-only
                              ,#'name
                              . ,(syntax-e #'body))
                            stx)])
            (cond
              [(and (identifier? guide)
                    (free-identifier=? guide (quote-syntax …)))
               (raise-syntax-error 'hlite
                                   "ellipses are not supported (yet)"
                                   guide)]
              [(and (identifier? guide) (free-identifier=? guide #'/))
               (dim-elt (stx-or-first body))]
              [(and (identifier? guide) (free-identifier=? guide #'=))
               (undim-elt (stx-or-first body))]
              [(and (identifier? guide) (free-identifier=? guide #'-))
               (hliterm-elt (stx-or-first body))]
              [(and (identifier? guide) (free-identifier=? guide #'+))
               (hliteadd-elt (stx-or-first body))]
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
                    (or (free-identifier=? (car guide) #'/)
                        (free-identifier=? (car guide) #'=)
                        (free-identifier=? (car guide) #'-)
                        (free-identifier=? (car guide) #'+)))
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
               body])))
        #`(begin
            (chunk #:save-as dummy name . body)
            #,disp))])

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
                       #,(quasisyntax/loc this-syntax
                           (chk name . xres))]))
              #,(quasisyntax/loc this-syntax
                  (#,(syntax/loc #'self do-it) . body))))
        (pretty-print (syntax->datum result))
        result]))

We define the function foo as follows:

♦chunk[<foo>
       (define (foo v)
         (+ 1 v))]

However, due to implementation details, we need to add ♦racket[π] to this
value:

♦hlite[|<foo'>| {/ (def args (_ _ + _ / _))}
       (define (foo v)
         (+ 1 π v))]

In order to optimise the sum of ♦racket[1] and ♦racket[π], we extract the
computation to a global helper constant:

♦hlite[|<foo''>| {+ _ _ / (def args (_ - _ _ + _ / _))}
       (define π 3.1414592653589793)
       (define one-pus-π (+ 1 π))
       (define (foo v)
         (+ 1 π one-pus-π v))]

The whole program is therefore:

♦chunk[<*>
       |<foo''>|
       (foo 42)]