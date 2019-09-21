(module shell-lang racket
  (provide shell)

  (require srfi/71 data/splay-tree)
  (require syntax/parse (for-syntax syntax/parse))
  (define-syntax (shell stx)
    (letrec
	((scope (make-splay-tree (order 'string-order string? string=?))))
      (define-syntax-class variable-decls
	#:description "variable declarations in sh"
	(pattern (var:id "=" rhs:expr))
	)
      (syntax-parse stx
	[( _ v:variable-decls) #'(splaytree-set! scope v.var v.rhs)]))
    )
  )
