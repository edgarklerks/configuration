#lang racket

(require racket/control)
(require racket/contract/combinator)
(require racket/contract)
(require (for-syntax racket/syntax))

(define-syntax (pred stx)
  (syntax-case stx ()
    [(_ b (c body) ... (else n)) #`(cond ((c b) body) ... (else n))]
    [(_ b (c body) ...) #`(cond ((c b) body) ...)]
    [(_ b (c (body ...)) ... (else n))  #`(cond ((c b) (begin body ...)) ... (else n))  ]
    [(_ b (c (body ...)) ...) #`(cond ((c b) (begin body ...)) ...)]))

(define (contract-test v)
 (print v) 
 )

(define-syntax-rule (sum/c c ...) 
  
  (make-flat-contract #:name 'sum/c #:first-order (lambda (x) #f) #:projection (sum/c-macro c ...)

 ))

(define-syntax (sum/c-macro stx)
  (syntax-case stx ()
    [(_ c ...)   #`(lambda (blame)
                     (lambda (v)
                       (let/cc return 
                       (printf "c ... : ~e ~n" `(c ...))
                       (blame-add-context blame "sum/c")
                       (define failed-handler #f)
                       (define switch-back-handler #f)
                       (printf "value: ~s ~n" v )
                       ;; Interpret the error and do an appropiate action based on that. 
                    (let ((contract-error-type (call-with-composable-continuation (lambda (k) 
                                                                                      (set! failed-handler k)
                                                                                    ))))
                              (when switch-back-handler
                              (blame-add-context blame "sum/c")
                              (match (car contract-error-type)
                                    ;; An contract is wrongly drawn up, reraise it
                                   ('done (return (cadr contract-error-type)))
                                   ('contract-error (raise (cadr contract-error-type)))
                                   ;; Pass to the next
                                   ('type-error (switch-back-handler 'next))
                                   ;; Something else happened
                                   ('misc-error (raise (cadr contract-error-type)))
                                   ('no-match (raise-blame-error blame v '(expected: "~e"  given: "~e") '(c ...) v))
                                               
                      ))
                       
                     (let ((cs (list c ...)))
                        (define skip-next #f)
                        (printf "cs: ~e ~n" cs)
                        (for/list ((ctr cs))
                          ;; Control the skipping of items
                          (let ((command (call-with-composable-continuation (lambda (k)
                                                                              (set! switch-back-handler k)
                                                                              ))))
                          (when (symbol? command)  
                          (printf "Switch-back-handler, command: ~e, checker: ~e, value: ~e ~n" command ctr v)
                          (when (eq? command 'next) (set! skip-next #t))))
                          
                          ;; Catch errors, so we can send it to the failed-handler
                          (with-handlers ([exn:fail:contract?
                                           (lambda (e)
                                          (failed-handler (list 'type-error e)))]
                                          [any/c
                                           (lambda (e)
                                            (failed-handler (list 'misc-error e)))])
                          (if skip-next
                              (set! skip-next #f)
                          (let ((ar (procedure-arity ctr) ))
                          (cond 
                          ((eq? 1 ar) (when (ctr v) (failed-handler (list 'done v))))
                          ((eq? 2 ar) (when ((ctr blame)) (failed-handler (list 'done v))))
                          (else (failed-handler (list 'contract-error (make-exn:fail:contract:arity 
                                                                       (format "~e doesn't have the correct arity, expected arity: 1 or 2" (quote ctr) ) 
                                                                       (current-continuation-marks))))))))))
                       ;; None matched...
                       (blame-add-context blame "")
                       (failed-handler (list 'no-match)))))
                       
                       
                       ))]))

;; Steals a blame object from another contract violation
(define-syntax (test-contract stx)
           #`(let ((steal-handler #f)
                    (blame-object #f))
              
              ;; The steal handler 
              (let ((stolen (call-with-composable-continuation 
                     (lambda (k) (set! steal-handler k)))))
                        ;; If it is succesfully stolen, set it stolen. 
                        (when (blame? stolen)
                        (set! blame-object stolen)))
              
               ;; The steal contract
             (define blamestealer/c 
             (make-flat-contract #:name 'blamestealer/c
                                 #:first-order (lambda (x) #t)
                                 #:projection (lambda (b) (steal-handler b) (lambda (v) v))))
             
              ;; If we have an object, return it...
              (unless (blame? blame-object)
     
                  (contract blamestealer/c 1 'shit-click 'blub)
                )
                blame-object))

(define (test-shift-reset x)
  (define ks #f)
  (define run 0)
  (reset (let ((e (* 2 (shift k (set! ks k)))))
        (printf "run: ~e e: ~e ~n" run e)
        (set! run (+ 1 run))
    (when (number? e) 
        (when (< e 1000) (set! e (* e 2)) (ks e) )
         e
      )))       
  (let ((g (ks x)))
  (printf "(ks x) => run: ~e ~n" run)
  (let ((t (ks g)))
  (printf "(ks e) => run: ~e ~n" run)
  (let ((q (ks t)))
  (printf "(ks e) => run: ~e ~n" run)
  (ks q
  )))))

 
(test-shift-reset 3)
(printf "next ~n")
(test-shift-reset 4)
(printf "next ~n")
(test-shift-reset 5)

(define test/c (sum/c number? boolean?))
;; (contract test/c #t 'blub 'blub)

(define test-pred 
  (pred 1
        (char? 'char)
        (number? 'number)
        )
  )

(define (continuation-or-falsum/c blame)
  (lambda (v)
  (pred v
   (continuation? #t)
   (false? #t)
   (else (raise-blame-error blame v '(expected: <continuation> or <#f> given: "~e") v))
   )
  ))

(define ProgramState (class object%
   

    (init)
    (define/public (initialized?)
      (if (and loopa loopb)
          #t
          #f
          )
      )
    (define/public (set!/loopa loopa-cont)
      (set! loopa loopa-cont))

    (define/public (set!/loopb loopb-cont)
      (set! loopb loopb-cont))

    (define/public (set!/current-loop cl)
      (set! current-loop cl))

    (define loopa #f)

    (define loopb #f)

    (define current-loop #f)

    (define/public (switch-loop k)

      (if (eq? current-loop 'loopb)
          (begin
            (set! current-loop 'loopb)
            (loopb k))
          (begin
            (set! current-loop 'loopa)
            (loopb k))))))

;; Main routine
(define (main)

  (let ((ps (new ProgramState)))
    1
    )

  )


(define (loopa cont-loopb)
1
  )
