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
  (case-> (->* (String (List 'choose (-> Any Boolean : #:+ A) (Listof (∩ A Prompt-Value)) (-> (∩ A Prompt-Value) String)))
               ((Listof Symbol)) (∩ A Prompt-Value))
          (->* (String (List 'string)) ((Listof Symbol)) String)
          (->* (String (List 'integer)) ((Listof Symbol)) Integer)
          (->* (String (List 'natural)) ((Listof Symbol)) Natural)
          (->* (String (List 'positive-integer)) ((Listof Symbol)) Positive-Integer)
          (->* (String (List 'between Positive-Integer Positive-Integer)) ((Listof Symbol)) Positive-Integer)
          (->* (String (List 'between Natural Natural)) ((Listof Symbol)) Natural)
          (->* (String (List 'between Integer Integer)) ((Listof Symbol)) Integer)
          (->* (String (List 'random Positive-Integer)) ((Listof Symbol)) Natural)))

(: prompt (All (A) (Prompt A)))
(define (prompt title op [tags '()])
  (define info (prompt-info title #:tags tags))
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

(: prompt-choose (->* (String (Listof Prompt-Value)) ((Listof Symbol)) Prompt-Value))
(define (prompt-choose title choices [tags '()]) (prompt title (op-choose prompt-value? choices) tags))
(: prompt-string (->* (String) ((Listof Symbol)) String))
(define (prompt-string title [tags '()]) (prompt title (op-string) tags))
(: prompt-integer (->* (String) ((Listof Symbol)) Integer))
(define (prompt-integer title [tags '()]) (prompt title (op-integer) tags))
(: prompt-natural (->* (String) ((Listof Symbol)) Natural))
(define (prompt-natural title [tags '()]) (prompt title (op-natural) tags))
(: prompt-positive-integer (->* (String) ((Listof Symbol)) Positive-Integer))
(define (prompt-positive-integer title [tags '()]) (prompt title (op-positive-integer) tags))
(: prompt-between (->* (String Integer Integer)((Listof Symbol)) Integer))
(define (prompt-between title from to [tags '()]) (prompt title (op-between from to) tags))
(: prompt-random (->* (String Positive-Integer) ((Listof Symbol)) Natural))
(define (prompt-random title n [tags '()]) (prompt title (op-random n) tags))

