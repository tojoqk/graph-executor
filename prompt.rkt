#lang typed/racket

(provide Prompt Prompt-Value current-prompt prompt)

(define-type (Prompt A)
  (case-> (->* (String (List 'const
                             (-> Any Boolean : #:+ A)
                             (∩ A Prompt-Value)))
               ((Immutable-HashTable Symbol Any))
               (∩ A Prompt-Value))
          (->* (String (List 'choose
                             (-> Any Boolean : #:+ A)
                             (Listof (U (∩ A String)
                                        (List (∩ A String) String)))))
               ((Immutable-HashTable Symbol Any))
               (∩ String A))
          (->* (String (List 'string))
               ((Immutable-HashTable Symbol Any))
               String)
          (->* (String (List 'integer))
               ((Immutable-HashTable Symbol Any))
               Integer)
          (->* (String (List 'natural))
               ((Immutable-HashTable Symbol Any))
               Natural)
          (->* (String (List 'positive))
               ((Immutable-HashTable Symbol Any))
               Positive-Integer)
          (->* (String (List 'range Natural Natural))
               ((Immutable-HashTable Symbol Any))
               Natural)
          (->* (String (List 'range Integer Integer))
               ((Immutable-HashTable Symbol Any))
               Integer)
          (->* (String (List 'random Positive-Integer))
               ((Immutable-HashTable Symbol Any))
               Natural)))

(define-type Prompt-Value (U String Integer))

(: current-prompt (Parameterof (Option (Prompt Any))))
(define current-prompt (make-parameter #f))

(: prompt (All (A) (Prompt A)))
(define (prompt title op [attrs  ((inst hash Symbol Any))])
  (cond [(current-prompt) => (lambda ([p : (Prompt Any)])
                               (let ([value (p title op attrs)])
                                 (case (car op)
                                   [(choose const) (assert value (cadr op))]
                                   [else value])))]
        [else (error 'prompt "called outside of trans")]))
