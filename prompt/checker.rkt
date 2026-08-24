#lang typed/racket

(require "../prompt.rkt")

(provide checker-prompt
         Checker-Prompt-Config checker-prompt-config
         default-checker-prompt-string-values
         default-checker-prompt-integer-values
         default-checker-prompt-natural-values
         default-checker-prompt-positive-integer-values
         default-checker-prompt-range-values
         default-checker-prompt-random-values)

(: default-checker-prompt-string-values (-> Prompt-Info (Pairof String (Listof String))))
(define default-checker-prompt-string-values
  (lambda (_info)
    (error 'prompt "checker does not support string prompt by default. Please configure `string-values` of `checker-prompt-config`")))

(: default-checker-prompt-integer-values (-> Prompt-Info (Pairof Integer (Listof Integer))))
(define default-checker-prompt-integer-values
  (lambda (_info)
    (error 'prompt "checker does not support integer prompt by default. Please configure `integer-values` of `checker-prompt-config`.")))

(: default-checker-prompt-natural-values (-> Prompt-Info (Pairof Natural (Listof Natural))))
(define default-checker-prompt-natural-values
  (lambda (_info)
    (error 'prompt "checker does not support natural prompt by default. Please configure `natural-values` of `checker-prompt-config`.")))

(: default-checker-prompt-positive-integer-values (-> Prompt-Info (Pairof Positive-Integer (Listof Positive-Integer))))
(define default-checker-prompt-positive-integer-values
  (lambda (_info)
    (error 'prompt "checker does not support positive-integer prompt by default. Please configure `positive-integer-values` of `checker-prompt-config`.")))

(: default-checker-prompt-range-values (-> Prompt-Info Integer Integer (U (Pairof Integer (Listof Integer))
                                                                          'ascending
                                                                          'descending)))
(define default-checker-prompt-range-values (lambda (_m _from _to) 'ascending))

(: default-checker-prompt-random-values (-> Prompt-Info Positive-Integer (U (Pairof Natural (Listof Natural))
                                                                            'ascending
                                                                            'descending)))
(define default-checker-prompt-random-values (lambda (_m _n) 'ascending))

(: list->amb (All (S) (-> (-> (-> Prompt-Value) * Prompt-Value) (-> Any Boolean : #:+ S) (Listof (∩ Prompt-Value S)) S)))
(define (list->amb amb p? lst)
  (assert (let loop : Prompt-Value ([lst lst])
            (if (null? lst)
                (amb)
                (amb (thunk (car lst))
                     (thunk (loop (cdr lst))))))
          p?))

(struct %checker-prompt-config ([string-values : (-> Prompt-Info (Pairof String (Listof String)))]
                                [integer-values : (-> Prompt-Info (Pairof Integer (Listof Integer)))]
                                [natural-values : (-> Prompt-Info (Pairof Natural (Listof Natural)))]
                                [positive-integer-values : (-> Prompt-Info (Pairof Positive-Integer (Listof Positive-Integer)))]
                                [range-values : (-> Prompt-Info Integer Integer (U (Pairof Integer (Listof Integer))
                                                                                   'ascending
                                                                                   'descending))]
                                [random-values : (-> Prompt-Info Positive-Integer (U (Pairof Natural (Listof Natural))
                                                                                     'ascending
                                                                                     'descending))])
  #:type-name Checker-Prompt-Config)

(: checker-prompt-config (-> [#:string-values (Option (-> Prompt-Info (Pairof String (Listof String))))]
                             [#:integer-values (Option (-> Prompt-Info (Pairof Integer (Listof Integer))))]
                             [#:natural-values (Option (-> Prompt-Info (Pairof Natural (Listof Natural))))]
                             [#:positive-integer-values (Option (-> Prompt-Info (Pairof Positive-Integer (Listof Positive-Integer))))]
                             [#:range-values (Option (-> Prompt-Info Integer Integer (U (Pairof Integer (Listof Integer))
                                                                                        'ascending
                                                                                        'descending)))]
                             [#:random-values (-> Prompt-Info Positive-Integer (U (Pairof Natural (Listof Natural))
                                                                                  'ascending
                                                                                  'descending))]
                             Checker-Prompt-Config))
(define (checker-prompt-config #:string-values [string-values #f]
                               #:integer-values [integer-values #f]
                               #:natural-values [natural-values #f]
                               #:positive-integer-values [positive-integer-values #f]
                               #:range-values [range-values #f]
                               #:random-values [random-values #f])
  (%checker-prompt-config (or string-values default-checker-prompt-string-values)
                          (or integer-values default-checker-prompt-integer-values)
                          (or natural-values default-checker-prompt-natural-values)
                          (or positive-integer-values default-checker-prompt-positive-integer-values)
                          (or range-values default-checker-prompt-range-values)
                          (or random-values default-checker-prompt-random-values)))


(: checker-prompt (-> (-> (-> Prompt-Value) * Prompt-Value)
                      Checker-Prompt-Config
                      Prompt-Implementation))
(define ((checker-prompt amb config) info op)
  (case (car op)
    [(choose) (checker-choose amb op)]
    [(integer) (values (list->amb amb exact-integer? ((%checker-prompt-config-integer-values config) info)) '())]
    [(natural) (values (list->amb amb natural? ((%checker-prompt-config-natural-values config) info)) '())]
    [(positive-integer) (values (list->amb amb exact-positive-integer? ((%checker-prompt-config-positive-integer-values config) info)) '())]
    [(string) (values (list->amb amb string? ((%checker-prompt-config-string-values config) info)) '())]
    [(range) (checker-range amb config info op)]
    [(random) (checker-random amb config info op)]))

(: checker-choose (-> (-> (-> Prompt-Value) * Prompt-Value)
                      (U (List 'choose Procedure (Listof Symbol))
                         (List 'choose (Listof Symbol)))
                      (Values Symbol Prompt-Attributes)))
(define (checker-choose amb op)
  (let* ([choices (if (procedure? (second op))
                      (third op)
                      (second op))]
         [value (let loop : Prompt-Value ([choices choices])
                  (if (null? choices)
                      (amb)
                      (amb (thunk (car choices))
                           (thunk (loop (cdr choices))))))])
    (values (assert value symbol?) '())))

(: checker-range (case-> (-> (-> (-> Prompt-Value) * Prompt-Value) Checker-Prompt-Config
                             Prompt-Info
                             (List 'range Positive-Integer Positive-Integer) (Values Positive-Integer Prompt-Attributes))
                         (-> (-> (-> Prompt-Value) * Prompt-Value) Checker-Prompt-Config
                             Prompt-Info
                             (List 'range Natural Natural) (Values Natural Prompt-Attributes))
                         (-> (-> (-> Prompt-Value) * Prompt-Value) Checker-Prompt-Config
                             Prompt-Info
                             (List 'range Integer Integer) (Values Integer Prompt-Attributes))))
(define (checker-range amb config info op)
  (let ([from (second op)]
        [to : Integer (third op)])
    (unless (<= from to)
      (error 'prompt "invalid range ~a...~a" from to))
    (values (let ([vals ((%checker-prompt-config-range-values config) info from to)])
              (if (pair? vals)
                  (let ([n (list->amb amb exact-integer? vals)])
                    (unless (and (<= from n) (<= n to))
                      (error 'prompt "must be ~a <= ~a <= ~a" from n to))
                    n)
                  (let ([value : Prompt-Value
                               (case vals
                                 [(ascending) (let loop : Prompt-Value ([i from])
                                                (if (< to i)
                                                    (amb)
                                                    (amb (thunk i)
                                                         (thunk (loop (add1 i))))))]
                                 [(descending) (let loop : Prompt-Value ([i to])
                                                 (if (< i from)
                                                     (amb)
                                                     (amb (thunk i)
                                                          (thunk (loop (sub1 i))))))])])
                    (if (and (exact? value) (integer? value)
                             (<= from value) (<= value to))
                        value
                        (error 'prompt "invalid range value ~a" value)))))
            '())))

(: checker-random (-> (-> (-> Prompt-Value) * Prompt-Value) Checker-Prompt-Config Prompt-Info (List 'random Positive-Integer) (Values Natural Prompt-Attributes)))
(define (checker-random amb config info op)
  (let ([n (second op)])
    (values (let ([vals ((%checker-prompt-config-random-values config) info n)])
              (case vals
                [(ascending) (assert (let loop : Prompt-Value ([i 0])
                                       (if (= i n)
                                           (amb)
                                           (amb (thunk i)
                                                (thunk (loop (add1 i))))))
                                     natural?)]
                [(descending) (assert (let loop : Prompt-Value ([i (sub1 n)])
                                        (if (< i 0)
                                            (amb)
                                            (amb (thunk i)
                                                 (thunk (loop (sub1 i))))))
                                      natural?)]
                [else (let ([r (list->amb amb natural? vals)])
                        (unless (< r n)
                          (error 'prompt "must be ~a < ~a" r n))
                        r)]))
            '())))
