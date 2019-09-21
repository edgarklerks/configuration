(module stx-shell-lang racket 
  (require syntax/parse)
  (require racket/generic)
  (require test-engine/racket-tests)

  ;; Empty type 
   (define-struct abstract-node ())
  
  ;; This is a generic interface to work in an easy way with astnodes
   (define-generics astnode 
        (typename astnode)
        (get-data astnode))
  
  ;; The base struct, only defines the name 
  (define-struct ast-node (name)
                #:transparent
                #:super struct:abstract-node
                #:methods gen:astnode
                [(define (typename this)
                   (match this
                     ((struct ast-node (name)) name)))
                 (define (get-data this)
                   (match this 
                     [(struct ast-node (name)) (list name)]))])
  
  ;; Constructor for var-sh-node
  ;; this node is a compound of two syntaxes:
  ;; blub <- ($ f1 f2 f3 f4)
  ;; var-decl   sh-decl
  ;; THe semantics of this syntax is that it binds the rhs to the lhs in a lazy way. 
  ;; rhs <- ($ f1 f2 f3 f4)
  ;; rhs == ($ f1 f2 f3 f4 f5)
  ;; 
  

  
   (define-struct var-sh-struct (var shnode)
     #:transparent
     #:super struct:ast-node
     #:methods gen:astnode
                [(define (typename this)
                   (match this
                     ((struct ast-node (name)) name)))
                 (define (get-data this)
                   (match this 
                     [(struct var-sh-struct (var shnode)) (list var shnode)]))])
  ;; var-expr binds a name to a arbitrary expr in a lazy way:
  ;; a <- expr 
  ;; a == expr 
  
   (define-struct var-expr-struct  (var expr)
     #:transparent
     #:super struct:ast-node
      #:methods gen:astnode
                [(define (typename this)
                   (match this
                     ((struct ast-node (name)) name)))
                 (define (get-data this)
                   (match this 
                     [(struct var-expr-struct (var expr)) (list var expr)]))])
  
  ;; sh-expr-node is a ast node from the syntax:
  ;; ($ f1 f2 f3 f4 f5 f6 (v1)?)
  ;; It semantics are dependend on the . operator:
  ;; ($ f1 f2 f3 f4 f5) -> (f1.f2.f3.f4.f5), where . is an operator specified by the user.
  ;; In case of function composition this wil yield:
  ;; f x -> f1(f2(f3(f4(f5(x))))
  ;; if there is a non function at the end, the whole expression will be applied:
  ;; ($ f1 f2 v) ->  (apply (f1.f2) v) 
  ;; The user should specify an apply operator for this. 
  ;; Sometimes the objects f1, f2 are not a mapping and thus . doesn't work on mappings, then it will be semantically behave 
  ;; like a monoid. 
  ;; Apply simple collapses the structure then. 
  ;; (a . b . c) -> (a x (b x c)) 
  ;; apply (a x (b x c)) -> collapse (a x (b x c)) -> collapse (a x bc) -> abc  
  
  
  
   (define-struct sh-expr-struct (exprs) 
     #:transparent
     #:super struct:ast-node
     #:methods gen:astnode
                [(define (typename this)
                   (match this 
                     [(struct ast-node (name)) name]))
                 (define (get-data this)
                   (match this 
                     [(struct sh-expr-struct (exprs)) exprs]))])
    
  
  ;; Parser
  
   (define-syntax-class sh-expr
     #:datum-literals ($)
     #:attributes(ast)
     (pattern [$ expr1:expr ...] #:attr ast #`(sh-expr-node `(list expr1 ...))))
 
  
   (define-splicing-syntax-class var-decl
        #:datum-literals (<-)
        #:attributes(ast)
        (pattern [var:id <- body:sh-expr] #:attr ast #`(var-sh-node (quote var) body.ast))
        (pattern [var:id <- body:expr] #:attr ast #`(var-expr-node (quote var)   (quasiquote body))))
  
    (define-splicing-syntax-class shell-lang 
      #:attributes(ast)
      (pattern t:sh-expr #:attr ast #'t.ast)
      (pattern g:var-decl #:attr ast #'g.ast))      
  
  
    ;; Constructor
  (define/contract (var-sh-node var shnode)
    (-> symbol? sh-expr-struct? var-sh-struct?)
     (var-sh-struct 'var-sh var shnode))
  
  (define/contract (var-expr-node var expr)
    (-> symbol? any/c var-expr-struct?)
     (var-expr-struct 'var-expr var expr))
  
  (define/contract (sh-expr-node exprs)
    (-> (listof any/c) sh-expr-struct?)
    (sh-expr-struct 'sh-expr exprs))
  
  
  
  ;; tests
  
  
  
  (provide shell-lang        
           (contract-out (get-data (-> astnode? (listof any/c))))
           gen:astnode
           astnode?
           typename
           (struct-out ast-node) 

           (struct-out var-sh-struct) 
           (struct-out var-expr-struct) 
           var-expr-node
           var-sh-node
           sh-expr-node
           (struct-out sh-expr-struct)))
  