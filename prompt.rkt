#lang typed/racket

(provide Prompt Prompt-Type Prompt-Value Prompt-Attributes current-prompt prompt
         Prompt-Info Prompt-Implementation
         prompt-info-value
         (rename-out [prompt-info?? prompt-info?])
         (except-out (struct-out prompt-info) prompt-info? make-prompt-info)
         Prompt-Info-Choose (struct-out prompt-info-choose)
         Prompt-Info-String (struct-out prompt-info-string)
         Prompt-Info-Integer (struct-out prompt-info-integer)
         Prompt-Info-Natural (struct-out prompt-info-natural)
         Prompt-Info-Positive (struct-out prompt-info-positive)
         Prompt-Info-Range (struct-out prompt-info-range)
         Prompt-Info-Random (struct-out prompt-info-random))

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

(define-type Prompt-Type (U 'choose 'string 'integer 'natural 'positive 'range 'random))
(define-type Prompt-Value (U String Integer))

(: current-prompt (Parameterof (Option Prompt-Implementation)))
(define current-prompt (make-parameter #f))

(: prompt (All (A) (Prompt A)))
(define (prompt title op)
  (cond [(current-prompt) => (lambda ([p : Prompt-Implementation])
                               (let ([info (p title op)])
                                 (case (car op)
                                   [(choose) (assert (prompt-info-choose-value info) (cadr op))]
                                   [(range)
                                    (let ([val (prompt-info-range-value info)])
                                      (let ([from (second op)] [to (third op)])
                                        (if (and (<= from val) (<= val to))
                                            val
                                            (error 'prompt "range implementation error"))))]
                                   [else (prompt-info-value info)])))]
        [else (error 'prompt "called outside of trans")]))

(define-type Prompt-Implementation
  (case-> (-> String (List 'choose
                           Any
                           (Listof (U String
                                      (List String String))))
              Prompt-Info-Choose)
          (-> String (List 'string) Prompt-Info-String)
          (-> String (List 'integer) Prompt-Info-Integer)
          (-> String (List 'natural) Prompt-Info-Natural)
          (-> String (List 'positive) Prompt-Info-Positive)
          (-> String (List 'range Integer Integer) Prompt-Info-Range)
          (-> String (List 'random Positive-Integer) Prompt-Info-Random)))

(: prompt-info-value (case-> (-> Prompt-Info-Choose String)
                             (-> Prompt-Info-String String)
                             (-> Prompt-Info-Integer Integer)
                             (-> Prompt-Info-Natural Natural)
                             (-> Prompt-Info-Positive Positive-Integer)
                             (-> Prompt-Info-Range Integer)
                             (-> Prompt-Info-Random Natural)
                             (-> Prompt-Info Prompt-Value)))
(define (prompt-info-value pi)
  (cond [(prompt-info-choose? pi) (prompt-info-choose-value pi)]
        [(prompt-info-string? pi) (prompt-info-string-value pi)]
        [(prompt-info-integer? pi) (prompt-info-integer-value pi)]
        [(prompt-info-natural? pi) (prompt-info-natural-value pi)]
        [(prompt-info-positive? pi) (prompt-info-positive-value pi)]
        [(prompt-info-range? pi) (prompt-info-range-value pi)]
        [(prompt-info-random? pi) (prompt-info-random-value pi)]))

(define-type Prompt-Info (U Prompt-Info-Choose
                            Prompt-Info-String
                            Prompt-Info-Integer
                            Prompt-Info-Natural
                            Prompt-Info-Positive
                            Prompt-Info-Range
                            Prompt-Info-Random))

(define-predicate prompt-info?? Prompt-Info)

(struct prompt-info ([title : String]
                     [attributes : Prompt-Attributes])
  #:constructor-name make-prompt-info
  #:transparent)
(struct prompt-info-choose prompt-info ([value : String]
                                        [items : (Listof (U (List String String) String))])
  #:type-name Prompt-Info-Choose
  #:transparent)
(struct prompt-info-string prompt-info ([value : String])
  #:type-name Prompt-Info-String
  #:transparent)
(struct prompt-info-integer prompt-info ([value : Integer])
  #:type-name Prompt-Info-Integer
  #:transparent)
(struct prompt-info-natural prompt-info ([value : Natural])
  #:type-name Prompt-Info-Natural
  #:transparent)
(struct prompt-info-positive prompt-info ([value : Positive-Integer])
  #:type-name Prompt-Info-Positive
  #:transparent)
(struct prompt-info-range prompt-info ([value : Integer]
                                       [maximum : Integer]
                                       [minimum : Integer])
  #:type-name Prompt-Info-Range
  #:transparent)

(struct prompt-info-random prompt-info ([value : Natural]
                                        [bound : Positive-Integer])
  #:type-name Prompt-Info-Random
  #:transparent)
