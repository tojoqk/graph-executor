#lang typed/racket

(provide Prompt Prompt-Implementation Prompt-Type Prompt-Value Prompt-Attributes current-prompt prompt)

(define-type (Prompt A)
  (case-> (-> String (List 'choose
                           (-> Any Boolean : #:+ A)
                           (Listof (U (∩ A String)
                                      (List (∩ A String) String))))
              (∩ String A))
          (-> String (List 'string) String)
          (-> String (List 'integer) Integer)
          (-> String (List 'natural) Natural)
          (-> String (List 'positive) Positive-Integer)
          (-> String (List 'range Positive-Integer Positive-Integer) Positive-Integer)
          (-> String (List 'range Natural Natural) Natural)
          (-> String (List 'range Integer Integer) Integer)
          (-> String (List 'random Positive-Integer) Natural)))

(define-type Prompt-Attributes (Listof (Pairof Symbol (U String Symbol Integer))))

(define-type (Prompt-Implementation A)
  (case-> (-> String (List 'choose
                           (-> Any Boolean : #:+ A)
                           (Listof (U (∩ A String)
                                      (List (∩ A String) String))))
              (Values (∩ String A) Prompt-Attributes))
          (-> String (List 'string) (Values String Prompt-Attributes))
          (-> String (List 'integer) (Values Integer Prompt-Attributes))
          (-> String (List 'natural) (Values Natural Prompt-Attributes))
          (-> String (List 'positive) (Values Positive-Integer Prompt-Attributes))
          (-> String (List 'range Positive-Integer Positive-Integer) (Values Positive-Integer Prompt-Attributes))
          (-> String (List 'range Natural Natural) (Values Natural Prompt-Attributes))
          (-> String (List 'range Integer Integer) (Values Integer Prompt-Attributes))
          (-> String (List 'random Positive-Integer) (Values Natural Prompt-Attributes))))

(define-type Prompt-Type (U 'choose 'string 'integer 'natural 'positive 'range 'random))
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
