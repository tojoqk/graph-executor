#lang typed/racket

(provide Prompt Prompt-Type Prompt-Value Prompt-Op current-prompt
         prompt
         op-choose op-choose-predicate op-choose-choices op-choose-show
         op-string op-integer op-natural op-positive-integer
         op-between op-between-from op-between-to
         op-random op-random-bound
         prompt-choose prompt-string prompt-integer prompt-natural prompt-positive-integer prompt-between prompt-random
         Prompt-Result prompt-result Prompt-Implementation
         prompt-result-value prompt-result-extra prompt-result-info
         Prompt-Record prompt-record? prompt-record prompt-record-value prompt-record-extra
         Prompt-Result
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

(: op-choose (All (A) (-> (-> Any Boolean : #:+ A)
                          (Listof (∩ A Symbol))
                          [#:show (-> (∩ A Symbol) String)]
                          (List 'choose (-> Any Boolean : #:+ A)
                                (Listof (∩ A Symbol))
                                (-> (∩ A Symbol) String)))))
(define (op-choose predicate choices #:show [show symbol->string])
  (list 'choose predicate choices show))
(: op-choose-predicate (All (A) (-> (List 'choose (-> Any Boolean : #:+ A)
                                          (Listof (∩ A Symbol))
                                          (-> (∩ A Symbol) String))
                                    (-> Any Boolean : #:+ A))))
(define (op-choose-predicate op) (second op))
(: op-choose-choices (All (A) (-> (List 'choose (-> Any Boolean : #:+ A)
                                        (Listof (∩ A Symbol))
                                        (-> (∩ A Symbol) String))
                                  (Listof (∩ A Symbol)))))
(define (op-choose-choices op) (third op))
(: op-choose-show (All (A) (-> (List 'choose (-> Any Boolean : #:+ A)
                                     (Listof (∩ A Symbol))
                                     (-> (∩ A Symbol) String))
                               (-> (∩ A Symbol) String))))
(define (op-choose-show op) (fourth op))
(: op-string (-> (List 'string)))
(define (op-string) '(string))
(: op-integer (-> (List 'integer)))
(define (op-integer) '(integer))
(: op-natural (-> (List 'natural)))
(define (op-natural) '(natural))
(: op-positive-integer (-> (List 'positive-integer)))
(define (op-positive-integer) '(positive-integer))
(: op-between (case-> (-> Positive-Integer Positive-Integer (List 'between Positive-Integer Positive-Integer))
                      (-> Natural Natural (List 'between Natural Natural))
                      (-> Integer Integer (List 'between Integer Integer))))
(define (op-between from to) `(between ,from ,to))
(: op-between-from (case-> (-> (List 'between Positive-Integer Positive-Integer) Positive-Integer)
                           (-> (List 'between Natural Natural) Natural)
                           (-> (List 'between Integer Integer) Integer)))
(define (op-between-from op) (second op))
(: op-between-to (case-> (-> (List 'between Positive-Integer Positive-Integer) Positive-Integer)
                         (-> (List 'between Natural Natural) Natural)
                         (-> (List 'between Integer Integer) Integer)))
(define (op-between-to op) (third op))
(: op-random (-> Positive-Integer (List 'random Positive-Integer)))
(define (op-random bound) `(random ,bound))
(: op-random-bound (-> (List 'random Positive-Integer) Positive-Integer))
(define (op-random-bound op) (second op))

(define-type (Prompt A)
  (case-> (->* (String (List 'choose (-> Any Boolean : #:+ A) (Listof (∩ A Symbol)) (-> (∩ A Symbol) String)))
               ((Listof Symbol)) (∩ Symbol A))
          (->* (String (List 'string)) ((Listof Symbol)) String)
          (->* (String (List 'integer)) ((Listof Symbol)) Integer)
          (->* (String (List 'natural)) ((Listof Symbol)) Natural)
          (->* (String (List 'positive-integer)) ((Listof Symbol)) Positive-Integer)
          (->* (String (List 'between Positive-Integer Positive-Integer)) ((Listof Symbol)) Positive-Integer)
          (->* (String (List 'between Natural Natural)) ((Listof Symbol)) Natural)
          (->* (String (List 'between Integer Integer)) ((Listof Symbol)) Integer)
          (->* (String (List 'random Positive-Integer)) ((Listof Symbol)) Natural)))

(define-type Prompt-Type (U 'choose 'string 'integer 'natural 'positive-integer 'between 'random))
(define-type Prompt-Op (U (List 'choose Procedure (Listof Symbol) Procedure)
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
                               (define-values (value _extra) (p info op))
                               (case (car op)
                                 [(choose) (assert value (cadr op))]
                                 [(between)
                                  (let ([from (second op)] [to (third op)])
                                    (if (and (<= from value) (<= value to))
                                        value
                                        (error 'prompt "between implementation error")))]
                                 [else value]))]
        [else (error 'prompt "called outside of trans")]))

(: prompt-choose (->* (String (Listof Symbol)) ((Listof Symbol)) Symbol))
(define (prompt-choose title choices [tags '()]) (prompt title (op-choose symbol? choices) tags))
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

(define-type Prompt-Implementation
  (case-> (-> Prompt-Info (List 'choose Procedure (Listof Symbol) Procedure)
              (Values Symbol Any))
          (-> Prompt-Info (List 'string) (Values String Any))
          (-> Prompt-Info (List 'integer) (Values Integer Any))
          (-> Prompt-Info (List 'natural) (Values Natural Any))
          (-> Prompt-Info (List 'positive-integer) (Values Positive-Integer Any))
          (-> Prompt-Info (List 'between Positive-Integer Positive-Integer) (Values Positive-Integer Any))
          (-> Prompt-Info (List 'between Natural Natural) (Values Natural Any))
          (-> Prompt-Info (List 'between Integer Integer) (Values Integer Any))
          (-> Prompt-Info (List 'random Positive-Integer) (Values Natural Any))))

(: prompt-result-value (case-> (-> Prompt-Result-Choose Symbol)
                               (-> Prompt-Result-String String)
                               (-> Prompt-Result-Integer Integer)
                               (-> Prompt-Result-Natural Natural)
                               (-> Prompt-Result-Positive-Integer Positive-Integer)
                               (-> Prompt-Result-Between Integer)
                               (-> Prompt-Result-Random Natural)
                               (-> Prompt-Result Prompt-Value)))
(define (prompt-result-value pi) (car (fourth pi)))

(: prompt-result-extra (-> Prompt-Result Any))
(define (prompt-result-extra pi) (cdr (fourth pi)))

(: prompt-result-info (-> Prompt-Result Prompt-Info))
(define (prompt-result-info pi) (third pi))

(define-type Prompt-Result (List 'prompt Prompt-Op Prompt-Info (Pairof Prompt-Value Any)))
(define-type Prompt-Record (Pairof Prompt-Value Any))
(define-predicate prompt-record? Prompt-Record)
(: prompt-record (-> Prompt-Value [#:extra Any] Prompt-Record))
(define (prompt-record val #:extra [prompt-extra #f]) (cons val prompt-extra))
(: prompt-record-value (-> Prompt-Record Prompt-Value))
(define (prompt-record-value rec) (car rec))
(: prompt-record-extra (-> Prompt-Record Any))
(define (prompt-record-extra rec) (cdr rec))

(: prompt-result (-> Prompt-Op Prompt-Info Prompt-Record Prompt-Result))
(define (prompt-result op info rec)
  (list 'prompt op info rec))

(define-type Prompt-Result-Choose (List 'prompt (List 'choose Procedure (Listof Symbol) Procedure)
                                        Symbol
                                        (Pairof Symbol Any)))
(define-type Prompt-Result-String (List 'prompt (List 'string) String (Pairof String Any)))
(define-type Prompt-Result-Integer (List 'prompt (List 'integer) String (Pairof Integer Any)))
(define-type Prompt-Result-Natural (List 'prompt (List 'natural) String (Pairof Natural Any)))
(define-type Prompt-Result-Positive-Integer (List 'prompt (List 'positive-integer) String (Pairof Positive-Integer Any)))
(define-type Prompt-Result-Between  (U (List 'prompt (List 'between Natural Natural) String (Pairof Natural Any))
                                     (List 'prompt (List 'between Positive-Integer Positive-Integer) String (Pairof Positive-Integer Any))
                                     (List 'prompt (List 'between Integer Integer) String (Pairof Integer Any))))
(define-type Prompt-Result-Random (List 'prompt (List 'random Positive-Integer) String (Pairof Natural Any)))
