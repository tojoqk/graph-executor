#lang typed/racket

(provide Prompt Prompt-Type Prompt-Value Prompt-Op Prompt-Attributes current-prompt prompt
         Prompt-Info Prompt-Implementation
         prompt-info-value prompt-info-attributes prompt-info-title
         Prompt-Info-Choose
         Prompt-Info-String
         Prompt-Info-Integer
         Prompt-Info-Natural
         Prompt-Info-Positive-Integer
         Prompt-Info-Range
         Prompt-Info-Random)

(define-type (Prompt A)
  (case-> (-> String (List 'choose (-> Any Boolean : #:+ A) (Listof (∩ A String))) (∩ String A))
          (-> String (List 'choose (Listof String)) String)
          (-> String (List 'string) String)
          (-> String (List 'integer) Integer)
          (-> String (List 'natural) Natural)
          (-> String (List 'positive-integer) Positive-Integer)
          (-> String (List 'range Positive-Integer Positive-Integer) Positive-Integer)
          (-> String (List 'range Natural Natural) Natural)
          (-> String (List 'range Integer Integer) Integer)
          (-> String (List 'random Positive-Integer) Natural)))

(define-type Prompt-Attributes (Listof (Pairof Symbol (U String Symbol Integer))))

(define-type Prompt-Type (U 'choose 'string 'integer 'natural 'positive-integer 'range 'random))
(define-type Prompt-Op (U (List 'choose Procedure (Listof String))
                          (List 'choose (Listof String))
                          (List 'string)
                          (List 'integer)
                          (List 'natural)
                          (List 'positive-integer)
                          (List 'range Positive-Integer Positive-Integer)
                          (List 'range Natural Natural)
                          (List 'range Integer Integer)
                          (List 'random Positive-Integer)))
(define-type Prompt-Value (U String Integer))

(: current-prompt (Parameterof (Option Prompt-Implementation)))
(define current-prompt (make-parameter #f))

(: prompt (All (A) (Prompt A)))
(define (prompt title op)
  (cond [(current-prompt) => (lambda ([p : Prompt-Implementation])
                               (define-values (value _attrs) (p title op))
                               (case (car op)
                                 [(choose) (if (procedure? (cadr op))
                                               (assert value (cadr op))
                                               value)]
                                 [(range)
                                  (let ([from (second op)] [to (third op)])
                                    (if (and (<= from value) (<= value to))
                                        value
                                        (error 'prompt "range implementation error")))]
                                 [else value]))]
        [else (error 'prompt "called outside of trans")]))

(define-type Prompt-Implementation
  (case-> (-> String (U (List 'choose Procedure (Listof String))
                        (List 'choose (Listof String)))
              (Values String Prompt-Attributes))
          (-> String (List 'string) (Values String Prompt-Attributes))
          (-> String (List 'integer) (Values Integer Prompt-Attributes))
          (-> String (List 'natural) (Values Natural Prompt-Attributes))
          (-> String (List 'positive-integer) (Values Positive-Integer Prompt-Attributes))
          (-> String (List 'range Positive-Integer Positive-Integer) (Values Positive-Integer Prompt-Attributes))
          (-> String (List 'range Natural Natural) (Values Natural Prompt-Attributes))
          (-> String (List 'range Integer Integer) (Values Integer Prompt-Attributes))
          (-> String (List 'random Positive-Integer) (Values Natural Prompt-Attributes))))

(: prompt-info-value (case-> (-> Prompt-Info-Choose String)
                             (-> Prompt-Info-String String)
                             (-> Prompt-Info-Integer Integer)
                             (-> Prompt-Info-Natural Natural)
                             (-> Prompt-Info-Positive-Integer Positive-Integer)
                             (-> Prompt-Info-Range Integer)
                             (-> Prompt-Info-Random Natural)
                             (-> Prompt-Info Prompt-Value)))
(define (prompt-info-value pi) (car (fourth pi)))

(: prompt-info-attributes (-> Prompt-Info Prompt-Attributes))
(define (prompt-info-attributes pi) (cdr (fourth pi)))

(: prompt-info-title (-> Prompt-Info String))
(define (prompt-info-title pi) (third pi))

(define-type Prompt-Info (List 'prompt Prompt-Op String (Pairof Prompt-Value Prompt-Attributes)))

(define-type Prompt-Info-Choose (List 'prompt (U (List 'choose Procedure (Listof String))
                                                 (List 'choose (Listof String)))
                                      String
                                      (Pairof String Prompt-Attributes)))
(define-type Prompt-Info-String (List 'prompt (List 'string) String (Pairof String Prompt-Attributes)))
(define-type Prompt-Info-Integer (List 'prompt (List 'integer) String (Pairof Integer Prompt-Attributes)))
(define-type Prompt-Info-Natural (List 'prompt (List 'natural) String (Pairof Natural Prompt-Attributes)))
(define-type Prompt-Info-Positive-Integer (List 'prompt (List 'positive-integer) String (Pairof Positive-Integer Prompt-Attributes)))
(define-type Prompt-Info-Range  (U (List 'prompt (List 'range Natural Natural) String (Pairof Natural Prompt-Attributes))
                                   (List 'prompt (List 'range Positive-Integer Positive-Integer) String (Pairof Positive-Integer Prompt-Attributes))
                                   (List 'prompt (List 'range Integer Integer) String (Pairof Integer Prompt-Attributes))))
(define-type Prompt-Info-Random (List 'prompt (List 'random Positive-Integer) String (Pairof Natural Prompt-Attributes)))
