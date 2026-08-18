#lang typed/racket

(provide Prompt Prompt-Type Prompt-Value Prompt-Op Prompt-Attributes current-prompt prompt
         Prompt-Info prompt-info Prompt-Implementation
         prompt-info-value prompt-info-attributes prompt-info-meta
         Prompt-Info-Choose
         Prompt-Info-String
         Prompt-Info-Integer
         Prompt-Info-Natural
         Prompt-Info-Positive-Integer
         Prompt-Info-Range
         Prompt-Info-Random
         Prompt-Meta (rename-out [prompt-meta* prompt-meta]) prompt-meta-title prompt-meta-tags prompt-meta-options
         Prompt-Option (struct-out prompt-option))

(struct prompt-option ()
  #:type-name Prompt-Option)

(struct prompt-meta ([title : String]
                     [tags : (Listof Symbol)]
                     [options : (Listof Prompt-Option)])
  #:type-name Prompt-Meta)

(: prompt-meta* (-> String [#:tags (Listof Symbol)] [#:options (Listof Prompt-Option)] Prompt-Meta))
(define (prompt-meta* title #:tags [tags '()] #:options [options '()])
  (prompt-meta title tags options))

(define-type (Prompt A)
  (case-> (-> (U String Prompt-Meta) (List 'choose (-> Any Boolean : #:+ A) (Listof (∩ A String))) (∩ String A))
          (-> (U String Prompt-Meta) (List 'choose (Listof String)) String)
          (-> (U String Prompt-Meta) (List 'string) String)
          (-> (U String Prompt-Meta) (List 'integer) Integer)
          (-> (U String Prompt-Meta) (List 'natural) Natural)
          (-> (U String Prompt-Meta) (List 'positive-integer) Positive-Integer)
          (-> (U String Prompt-Meta) (List 'range Positive-Integer Positive-Integer) Positive-Integer)
          (-> (U String Prompt-Meta) (List 'range Natural Natural) Natural)
          (-> (U String Prompt-Meta) (List 'range Integer Integer) Integer)
          (-> (U String Prompt-Meta) (List 'random Positive-Integer) Natural)))

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
(define (prompt title-or-meta op)
  (define meta (if (string? title-or-meta)
                   (prompt-meta* title-or-meta)
                   title-or-meta))
  (cond [(current-prompt) => (lambda ([p : Prompt-Implementation])
                               (define-values (value _attrs) (p meta op))
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
  (case-> (-> Prompt-Meta (U (List 'choose Procedure (Listof String))
                             (List 'choose (Listof String)))
              (Values String Prompt-Attributes))
          (-> Prompt-Meta (List 'string) (Values String Prompt-Attributes))
          (-> Prompt-Meta (List 'integer) (Values Integer Prompt-Attributes))
          (-> Prompt-Meta (List 'natural) (Values Natural Prompt-Attributes))
          (-> Prompt-Meta (List 'positive-integer) (Values Positive-Integer Prompt-Attributes))
          (-> Prompt-Meta (List 'range Positive-Integer Positive-Integer) (Values Positive-Integer Prompt-Attributes))
          (-> Prompt-Meta (List 'range Natural Natural) (Values Natural Prompt-Attributes))
          (-> Prompt-Meta (List 'range Integer Integer) (Values Integer Prompt-Attributes))
          (-> Prompt-Meta (List 'random Positive-Integer) (Values Natural Prompt-Attributes))))

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

(: prompt-info-meta (-> Prompt-Info Prompt-Meta))
(define (prompt-info-meta pi) (third pi))

(define-type Prompt-Info (List 'prompt Prompt-Op Prompt-Meta (Pairof Prompt-Value Prompt-Attributes)))

(: prompt-info (-> Prompt-Op Prompt-Meta Prompt-Value Prompt-Attributes Prompt-Info))
(define (prompt-info op meta value attrs)
  (list 'prompt op meta (cons value attrs)))

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
