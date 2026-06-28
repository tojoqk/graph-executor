#lang typed/racket

(provide Prompt Prompt-Operator Prompt-Value current-prompt prompt)

(define-type (Prompt A)
  (case-> (-> String (List 'const
                           (-> Any Boolean : #:+ A)
                           (∩ A Prompt-Value))
              (∩ A Prompt-Value))
          (-> String (List 'choose
                           (-> Any Boolean : #:+ A)
                           (Listof (U (∩ A String)
                                      (List (∩ A String) String))))
              (∩ String A))
          (-> String (List 'string) String)
          (-> String (List 'integer) Integer)
          (-> String (List 'natural) Natural)
          (-> String (List 'positive) Positive-Integer)
          (-> String (List 'range 'from Natural 'to Natural) Natural)
          (-> String (List 'range 'from Integer 'to Integer) Integer)
          (-> String (List 'random Positive-Integer) Natural)))

(define-type (Prompt-Operator A)
  (U (List 'choose
           (-> Any Boolean : #:+ A)
           (Listof (U (∩ A String)
                      (List (∩ A String) String))))
     (List 'string)
     (List 'integer)
     (List 'natural)
     (List 'positive)
     (List 'range 'from Natural 'to Natural)
     (List 'range 'from Integer 'to Integer)
     (List 'random Positive-Integer)))

(define-type Prompt-Value (U String Integer))

(: current-prompt (Parameterof (Option (Prompt Any))))
(define current-prompt (make-parameter #f))

(: prompt (All (A) (Prompt A)))
(define (prompt title op)
  (cond [(current-prompt) => (lambda ([p : (Prompt Any)])
                               (let ([value (p title op)])
                                 (case (car op)
                                   [(choose const) (assert value (cadr op))]
                                   [else value])))]
        [else (error 'prompt "called outside of trans")]))
