#lang typed/racket

(provide Prompt Prompt-Type Prompt-Value Prompt-Op Prompt-Attributes current-prompt prompt prompt/untyped
         Prompt-Result prompt-result Prompt-Implementation
         prompt-result-value prompt-result-attributes prompt-result-info
         Prompt-Result-Choose
         Prompt-Result-String
         Prompt-Result-Integer
         Prompt-Result-Natural
         Prompt-Result-Positive-Integer
         Prompt-Result-Between
         Prompt-Result-Random
         Prompt-Info (rename-out [prompt-info* prompt-info]) prompt-info-title prompt-info-tags)

(struct prompt-info ([title : String]
                     [tags : (Listof Symbol)])
  #:type-name Prompt-Info)

(: prompt-info* (-> String [#:tags (Listof Symbol)] Prompt-Info))
(define (prompt-info* title #:tags [tags '()])
  (prompt-info title tags))

(define-type (Prompt A)
  (case-> (->* (String (List 'choose (-> Any Boolean : #:+ A) (Listof (∩ A Symbol)))) ((Listof Symbol)) (∩ Symbol A))
          (->* (String (List 'choose (Listof Symbol))) ((Listof Symbol)) Symbol)
          (->* (String (List 'string)) ((Listof Symbol)) String)
          (->* (String (List 'integer)) ((Listof Symbol)) Integer)
          (->* (String (List 'natural)) ((Listof Symbol)) Natural)
          (->* (String (List 'positive-integer)) ((Listof Symbol)) Positive-Integer)
          (->* (String (List 'between Positive-Integer Positive-Integer)) ((Listof Symbol)) Positive-Integer)
          (->* (String (List 'between Natural Natural)) ((Listof Symbol)) Natural)
          (->* (String (List 'between Integer Integer)) ((Listof Symbol)) Integer)
          (->* (String (List 'random Positive-Integer)) ((Listof Symbol)) Natural)))

(define-type Prompt-Attributes (Listof (Pairof Symbol (U String Symbol Integer))))

(define-type Prompt-Type (U 'choose 'string 'integer 'natural 'positive-integer 'between 'random))
(define-type Prompt-Op (U (List 'choose Procedure (Listof Symbol))
                          (List 'choose (Listof Symbol))
                          (List 'string)
                          (List 'integer)
                          (List 'natural)
                          (List 'positive-integer)
                          (List 'between Positive-Integer Positive-Integer)
                          (List 'between Natural Natural)
                          (List 'between Integer Integer)
                          (List 'random Positive-Integer)))
(define-type Prompt-Value (U Symbol String Integer))

(: current-prompt (Parameterof (Option Prompt-Implementation)))
(define current-prompt (make-parameter #f))

(: prompt (All (A) (Prompt A)))
(define (prompt title op [tags '()])
  (define info (prompt-info* title #:tags tags))
  (cond [(current-prompt) => (lambda ([p : Prompt-Implementation])
                               (define-values (value _attrs) (p info op))
                               (case (car op)
                                 [(choose) (if (procedure? (cadr op))
                                               (assert value (cadr op))
                                               value)]
                                 [(between)
                                  (let ([from (second op)] [to (third op)])
                                    (if (and (<= from value) (<= value to))
                                        value
                                        (error 'prompt "between implementation error")))]
                                 [else value]))]
        [else (error 'prompt "called outside of trans")]))

(: prompt/untyped (All (A) (-> String Prompt-Op (Listof Symbol) Prompt-Value)))
(define (prompt/untyped title op [tags '()])
  (case (car op)
    [(string) (prompt title op tags)]
    [(integer) (prompt title op tags)]
    [(natural) (prompt title op tags)]
    [(positive-integer) (prompt title op tags)]
    [(between) (prompt title op tags)]
    [(random) (prompt title op tags)]
    [(choose) (if (procedure? (second op))
                  (error 'prompt/untyped "choose with a predicate is not supported in untyped context; use '(choose (<symbol> ...)) instead")
                  (prompt title op tags))]))

(define-type Prompt-Implementation
  (case-> (-> Prompt-Info (U (List 'choose Procedure (Listof Symbol))
                             (List 'choose (Listof Symbol)))
              (Values Symbol Prompt-Attributes))
          (-> Prompt-Info (List 'string) (Values String Prompt-Attributes))
          (-> Prompt-Info (List 'integer) (Values Integer Prompt-Attributes))
          (-> Prompt-Info (List 'natural) (Values Natural Prompt-Attributes))
          (-> Prompt-Info (List 'positive-integer) (Values Positive-Integer Prompt-Attributes))
          (-> Prompt-Info (List 'between Positive-Integer Positive-Integer) (Values Positive-Integer Prompt-Attributes))
          (-> Prompt-Info (List 'between Natural Natural) (Values Natural Prompt-Attributes))
          (-> Prompt-Info (List 'between Integer Integer) (Values Integer Prompt-Attributes))
          (-> Prompt-Info (List 'random Positive-Integer) (Values Natural Prompt-Attributes))))

(: prompt-result-value (case-> (-> Prompt-Result-Choose String)
                               (-> Prompt-Result-String String)
                               (-> Prompt-Result-Integer Integer)
                               (-> Prompt-Result-Natural Natural)
                               (-> Prompt-Result-Positive-Integer Positive-Integer)
                               (-> Prompt-Result-Between Integer)
                               (-> Prompt-Result-Random Natural)
                               (-> Prompt-Result Prompt-Value)))
(define (prompt-result-value pi) (car (fourth pi)))

(: prompt-result-attributes (-> Prompt-Result Prompt-Attributes))
(define (prompt-result-attributes pi) (cdr (fourth pi)))

(: prompt-result-info (-> Prompt-Result Prompt-Info))
(define (prompt-result-info pi) (third pi))

(define-type Prompt-Result (List 'prompt Prompt-Op Prompt-Info (Pairof Prompt-Value Prompt-Attributes)))

(: prompt-result (-> Prompt-Op Prompt-Info Prompt-Value Prompt-Attributes Prompt-Result))
(define (prompt-result op info value attrs)
  (list 'prompt op info (cons value attrs)))

(define-type Prompt-Result-Choose (List 'prompt (U (List 'choose Procedure (Listof String))
                                                   (List 'choose (Listof String)))
                                        String
                                        (Pairof String Prompt-Attributes)))
(define-type Prompt-Result-String (List 'prompt (List 'string) String (Pairof String Prompt-Attributes)))
(define-type Prompt-Result-Integer (List 'prompt (List 'integer) String (Pairof Integer Prompt-Attributes)))
(define-type Prompt-Result-Natural (List 'prompt (List 'natural) String (Pairof Natural Prompt-Attributes)))
(define-type Prompt-Result-Positive-Integer (List 'prompt (List 'positive-integer) String (Pairof Positive-Integer Prompt-Attributes)))
(define-type Prompt-Result-Between  (U (List 'prompt (List 'between Natural Natural) String (Pairof Natural Prompt-Attributes))
                                     (List 'prompt (List 'between Positive-Integer Positive-Integer) String (Pairof Positive-Integer Prompt-Attributes))
                                     (List 'prompt (List 'between Integer Integer) String (Pairof Integer Prompt-Attributes))))
(define-type Prompt-Result-Random (List 'prompt (List 'random Positive-Integer) String (Pairof Natural Prompt-Attributes)))
