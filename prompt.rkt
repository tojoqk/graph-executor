#lang typed/racket

(provide Prompt Prompt-Operator Prompt-Value current-prompt prompt)

(define-type (Prompt A)
  (case-> (-> String (List 'choose
                           (-> Any Boolean : #:+ A)
                           (Listof (U (∩ A (U Symbol String))
                                      (List (∩ A (U Symbol String False)) String))))
              [#:type (Option Symbol)]
              (∩ (U Symbol String False) A))
          (-> String (List 'string) [#:type (Option Symbol)] String)
          (-> String (List 'integer) [#:type (Option Symbol)] Integer)
          (-> String (List 'natural) [#:type (Option Symbol)] Natural)
          (-> String (List 'positive) [#:type (Option Symbol)] Positive-Integer)
          (-> String (List 'range 'from Natural 'to Natural) [#:type (Option Symbol)] Natural)
          (-> String (List 'range 'from Integer 'to Integer) [#:type (Option Symbol)] Integer)
          (-> String (List 'random Positive-Integer) [#:type (Option Symbol)] Natural)))

(define-type (Prompt-Operator A)
  (U (List 'choose
           (-> Any Boolean : #:+ A)
           (Listof (U (∩ A (U Symbol String))
                      (List (∩ A (U Symbol String False)) String))))
     (List 'string)
     (List 'integer)
     (List 'natural)
     (List 'positive)
     (List 'range 'from Natural 'to Natural)
     (List 'range 'from Integer 'to Integer)
     (List 'random Positive-Integer)))

(define-type Prompt-Value (U Symbol String False Integer))

(: current-prompt (Parameterof (Option (Prompt Any))))
(define current-prompt (make-parameter #f))

(: prompt (All (A) (Prompt A)))
(define (prompt title op #:type [type #f])
  (cond [(current-prompt) => (lambda ([p : (Prompt Any)])
                               (let ([value (p title op #:type type)])
                                 (case (car op)
                                   [(choose) (assert value (cadr op))]
                                   [else value])))]
        [else (error 'prompt "called outside of trans")]))
