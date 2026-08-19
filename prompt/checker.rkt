#lang typed/racket

(require "../prompt.rkt")

(provide model-checker-prompt
         current-model-checker-string-values
         current-model-checker-integer-values
         current-model-checker-natural-values
         current-model-checker-positive-integer-values
         current-model-checker-range-values
         current-model-checker-random-values)

(: current-model-checker-string-values (Parameterof (-> Prompt-Meta (Pairof String (Listof String)))))
(define current-model-checker-string-values
  (make-parameter (lambda ([_meta : Prompt-Meta]) '("test" " " ""))))

(: current-model-checker-integer-values (Parameterof (-> Prompt-Meta (Pairof Integer (Listof Integer)))))
(define current-model-checker-integer-values (make-parameter
                                              (lambda ([_meta : Prompt-Meta])
                                                '(-1 0 1 2))))

(: current-model-checker-natural-values (Parameterof (-> Prompt-Meta (Pairof Natural (Listof Natural)))))
(define current-model-checker-natural-values (make-parameter (lambda ([_meta : Prompt-Meta])
                                                               '(0 1 2))))

(: current-model-checker-positive-integer-values (Parameterof (-> Prompt-Meta (Pairof Positive-Integer (Listof Positive-Integer)))))
(define current-model-checker-positive-integer-values (make-parameter (lambda ([_meta : Prompt-Meta])
                                                                        '(1 2))))

(: current-model-checker-range-values (Parameterof (-> Prompt-Meta Integer Integer (U (Pairof Integer (Listof Integer))
                                                                                      'ascending
                                                                                      'descending))))
(define current-model-checker-range-values (make-parameter (lambda (_m _from _to) 'ascending)))

(: current-model-checker-random-values (Parameterof (-> Prompt-Meta Positive-Integer (U (Pairof Natural (Listof Natural))
                                                                                        'ascending
                                                                                        'descending))))
(define current-model-checker-random-values (make-parameter (lambda (_m _n) 'ascending)))

(: list->amb (All (S) (-> (-> (-> Prompt-Value) * Prompt-Value) (-> Any Boolean : #:+ S) (Listof (∩ Prompt-Value S)) S)))
(define (list->amb amb p? lst)
  (assert (let loop : Prompt-Value ([lst lst])
            (if (null? lst)
                (amb)
                (amb (thunk (car lst))
                     (thunk (loop (cdr lst))))))
          p?))

(: model-checker-prompt (-> (-> (-> Prompt-Value) * Prompt-Value) Prompt-Implementation))
(define ((model-checker-prompt amb) meta op)
  (case (car op)
    [(choose) (model-checker-choose amb op)]
    [(integer) (values (list->amb amb exact-integer? ((current-model-checker-integer-values) meta)) '())]
    [(natural) (values (list->amb amb natural? ((current-model-checker-natural-values) meta)) '())]
    [(positive-integer) (values (list->amb amb exact-positive-integer? ((current-model-checker-positive-integer-values) meta)) '())]
    [(string) (values (list->amb amb string? ((current-model-checker-string-values) meta)) '())]
    [(range) (model-checker-range amb meta op)]
    [(random) (model-checker-random amb meta op)]))

(: model-checker-choose (-> (-> (-> Prompt-Value) * Prompt-Value)
                            (U (List 'choose Procedure (Listof Symbol))
                               (List 'choose (Listof Symbol)))
                            (Values Symbol Prompt-Attributes)))
(define (model-checker-choose amb op)
  (let* ([choices (if (procedure? (second op))
                      (third op)
                      (second op))]
         [value (let loop : Prompt-Value ([choices choices])
                  (if (null? choices)
                      (amb)
                      (amb (thunk (car choices))
                           (thunk (loop (cdr choices))))))])
    (values (assert value symbol?) '())))

(: model-checker-range (case-> (-> (-> (-> Prompt-Value) * Prompt-Value)
                                   Prompt-Meta
                                   (List 'range Positive-Integer Positive-Integer) (Values Positive-Integer Prompt-Attributes))
                               (-> (-> (-> Prompt-Value) * Prompt-Value)
                                   Prompt-Meta
                                   (List 'range Natural Natural) (Values Natural Prompt-Attributes))
                               (-> (-> (-> Prompt-Value) * Prompt-Value)
                                   Prompt-Meta
                                   (List 'range Integer Integer) (Values Integer Prompt-Attributes))))
(define (model-checker-range amb meta op)
  (let ([from (second op)]
        [to : Integer (third op)])
    (unless (<= from to)
      (error 'model-checker-prompt "invalid range ~a...~a" from to))
    (values (let ([vals ((current-model-checker-range-values) meta from to)])
              (if (pair? vals)
                  (let ([n (list->amb amb exact-integer? vals)])
                    (unless (and (<= from n) (<= n to))
                      (error 'current-model-checker-range-values "must be ~a <= ~a <= ~a" from n to))
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
                        (error 'model-checker-prompt "invalid range value ~a" value)))))
            '())))

(: model-checker-random (-> (-> (-> Prompt-Value) * Prompt-Value) Prompt-Meta (List 'random Positive-Integer) (Values Natural Prompt-Attributes)))
(define (model-checker-random amb meta op)
  (let ([n (second op)])
    (values (let ([vals ((current-model-checker-random-values) meta n)])
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
                          (error 'current-model-checker-random-values "must be ~a < ~a" r n))
                        r)]))

            '())))
