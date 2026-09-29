#lang typed/racket

(require "prompt/base.rkt")
(require "../plugin/prompt/console.rkt")

(provide prompt current-prompt prompt-without-trans
         prompt-choose prompt-string prompt-integer prompt-natural
         prompt-positive-integer prompt-between prompt-random
         (all-from-out "prompt/base.rkt"))

(: current-prompt (Parameterof Prompt-Implementation))
(define current-prompt (make-parameter console-prompt))

(: prompt-without-trans Prompt-Implementation)
(define (prompt-without-trans _info _op)
  (error 'prompt "called outside of trans"))

(define-type (Prompt A)
  (case-> (->* (Prompt-Content (List 'choose (-> Any Boolean : #:+ A) (Listof (∩ A Prompt-Value)) (-> (∩ A Prompt-Value) String)))
               ((Listof Symbol)) (∩ A Prompt-Value))
          (->* (Prompt-Content (List 'string)) ((Listof Symbol)) String)
          (->* (Prompt-Content (List 'integer)) ((Listof Symbol)) Integer)
          (->* (Prompt-Content (List 'natural)) ((Listof Symbol)) Natural)
          (->* (Prompt-Content (List 'positive-integer)) ((Listof Symbol)) Positive-Integer)
          (->* (Prompt-Content (List 'between Positive-Integer Positive-Integer)) ((Listof Symbol)) Positive-Integer)
          (->* (Prompt-Content (List 'between Natural Natural)) ((Listof Symbol)) Natural)
          (->* (Prompt-Content (List 'between Integer Integer)) ((Listof Symbol)) Integer)
          (->* (Prompt-Content (List 'random Positive-Integer)) ((Listof Symbol)) Natural)))

(: prompt (All (A) (Prompt A)))
(define (prompt content op [tags '()])
  (define info (prompt-info content #:tags tags))
  (define p (current-prompt))
  (case (car op)
    [(choose)
     (define-values (value _extra)
       (p info `(choose ,(second op) ,(third op)
                        ,(lambda ([x : Prompt-Value])
                           (if ((second op) x)
                               ((fourth op) x)
                               (error 'prompt "invalid choice type" x))))))
     (assert value (cadr op))]
    [(between)
     (define-values (value _extra) (p info op))
     (let ([from (second op)] [to (third op)])
       (if (and (<= from value) (<= value to))
           value
           (error 'prompt "between implementation error")))]
    [else (define-values (value _extra) (p info op))
          value]))

(: prompt-choose (->* (Prompt-Content (Listof Prompt-Value)) ((Listof Symbol)) Prompt-Value))
(define (prompt-choose content choices [tags '()]) (prompt content (op-choose prompt-value? choices) tags))
(: prompt-string (->* (Prompt-Content) ((Listof Symbol)) String))
(define (prompt-string content [tags '()]) (prompt content (op-string) tags))
(: prompt-integer (->* (Prompt-Content) ((Listof Symbol)) Integer))
(define (prompt-integer content [tags '()]) (prompt content (op-integer) tags))
(: prompt-natural (->* (Prompt-Content) ((Listof Symbol)) Natural))
(define (prompt-natural content [tags '()]) (prompt content (op-natural) tags))
(: prompt-positive-integer (->* (Prompt-Content) ((Listof Symbol)) Positive-Integer))
(define (prompt-positive-integer content [tags '()]) (prompt content (op-positive-integer) tags))
(: prompt-between (->* (Prompt-Content Integer Integer)((Listof Symbol)) Integer))
(define (prompt-between content from to [tags '()]) (prompt content (op-between from to) tags))
(: prompt-random (->* (Prompt-Content Positive-Integer) ((Listof Symbol)) Natural))
(define (prompt-random content n [tags '()]) (prompt content (op-random n) tags))

