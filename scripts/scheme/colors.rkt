
(module shell-lang racket
  (provide shell)
  (require data/order)
  (require data/splay-tree)
  (require syntax/parse)
  (require (for-syntax syntax/parse "stx-shell-lang.rkt"))
  (require "stx-shell-lang.rkt")
  (require racket/generic)
  (require racket/contract)
  (require racket/contract/region)
  (require racket/contract/combinator)
  (require racket/syntax)
  (require test-engine/racket-tests)

  (define (new-scope)
    (make-splay-tree (order 'string-order string? string=? string=?)))

 ;; Creates a srcloc object out of a syntax object
  (define (make-srcloc syntax-object)
   (srcloc (syntax-source syntax-object) (syntax-line syntax-object)
           (syntax-column syntax-object) (syntax-position syntax-object)
           (syntax-position syntax-object) (syntax-span syntax-object)))

  (struct exn:fail:shell  (errormsg continuation-mark)
    #:transparent
    #:extra-constructor-name make-shell-error)
  (struct exn:fail:shell:program exn:fail:shell (srcloc srcstring)
    #:transparent
    #:extra-constructor-name make-shell-program-error)
  (struct exn:fail:shell:contract (contract-error)
    #:transparent
    #:extra-constructor-name make-shell-contract-error)

  (define-syntax (shell stx)
    (syntax-parse stx
         [(_ sh:shell-lang ...) #`(list #`sh.ast ...)]))

  ;; Unroll expressions
  ;; This will flatten expressions, while keeping the scope annotated.
  ;; (a <- b)
  ;; (c <-  (shell (a <- ($ a x p d e)
  ;;                     (ret a)             )))
  ;; ($ + c d)
  ;; can be flattened as:
  ;; scopelevel  expr
  ;; 0           (a <- b)
  ;; 1           (a <- ($ a x p d e))
  ;; 0           c <- (ret a)
  ;; ($ + c d)


  (define-syntax-rule (unroll/syntax prog)
    (with-contract unroll/syntax #:result (not/c syntax?)
         ;; tell shit, we only want syntaxes
         (let ((sobject (contract syntax? prog 'unroll/syntax 'unroll/shell)))
              (unroll/shell (eval-syntax sobject)))))

  (define (unroll/ast-node prog)
       (with-contract unroll/ast-node #:result ast-node?
             (let ((sobject (contract ast-node? prog 'unroll/ast-node 'unroll/shell)))
               (case (typename sobject)
                 ('var-expr (unroll/var-expr sobject))
                 ('var-sh (unroll/var-sh sobject))
                 ('sh-expr (unroll/sh-expr sobject))
                 ;; It would be nice if we have src information too.
                 (else (raise (make-shell-error "Unknown node in unroll/ast-node" (current-continuation-marks)))
                 )))))
  (define (unroll/var-sh prog)
    (with-contract unroll/var-sh #:result var-sh-struct?
                (let ((sobject (contract var-sh-struct? prog 'unroll/var-sh 'unroll/shell)))
                  (match sobject
                    ((var-sh-struct var sh-expr) (var-sh-node var (unroll/shell sh-expr)))))))

  (define (unroll/sh-expr prog)
    (with-contract unroll/sh-expr #:result sh-expr-struct?
                (let ((sobject (contract sh-expr-struct? prog 'unroll/sh-expr 'unroll/shell)))
                  (match sobject
                    ((sh-expr-struct exprs) (sh-expr-node (unroll/shell exprs)))))))

  (define (unroll/var-expr prog)
    (with-contract unroll/var-expr #:result var-expr-struct?
                   (let ((sobject (contract var-expr-struct? prog 'unroll/var-expr 'unroll/shell)))
                     (match sobject
                       [(var-expr-struct var expr) (var-expr-node var (unroll/shell expr))]
                       ))))

  (define (unroll/list prog)
    (with-contract unroll/list #:result (listof any/c)
    (let ((sobject (contract (listof any/c) prog 'unroll/list 'unroll/shell)))
      (for/list ((i sobject))
        (unroll/shell i)
        ))))

  (define (unroll/shell prog)
      (cond
        ((syntax? prog ) (unroll/syntax prog))
        ((ast-node? prog) (unroll/ast-node prog))
        ((list? prog)  (unroll/list prog))
        (else prog)
        ))


 ;; Execute the program with an evaluator2
  (define-syntax-rule (execute/with evaluator prog)
    (with-contract execute/with
                   #:result any/c
     (print prog)
           ))

 (define (pprint prog)
   (for/list ((i prog))
     (cond
       ((var-expr-struct? i) (displayln (format "var ~s <- (~s)\n" (var-expr-struct-var i) (var-expr-struct-expr i))))
       ((sh-expr-struct? i) (for ((j (sh-expr-struct-exprs i )))

                              (displayln (format "($ ~s)" (quasiquote (unquote j)

     ))))))))

;; A small test
  (let ((g
 (shell
          (test <- ($ (+ 1 2 3) (+ 3 4 5) (+ 9 1 2)))
          (tast <- 3)
          ($ (+ 2) (+ 4) (+ 5 6) (shell (a <- b)))
          (tests <- btest))))

    (pprint (unroll/shell g)))
  )
