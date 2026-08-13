#lang typed/racket

(require "../prompt.rkt")

(provide model-checker-prompt
         current-model-checker-string-value
         current-model-checker-integer-value
         current-model-checker-natural-value
         current-model-checker-positive-integer-value)

(: current-model-checker-string-value (Parameterof String))
(define current-model-checker-string-value (make-parameter "test"))

(: current-model-checker-integer-value (Parameterof Integer))
(define current-model-checker-integer-value (make-parameter -1))

(: current-model-checker-natural-value (Parameterof Natural))
(define current-model-checker-natural-value (make-parameter 0))

(: current-model-checker-positive-integer-value (Parameterof Positive-Integer))
(define current-model-checker-positive-integer-value (make-parameter 1))

(: model-checker-prompt (-> (-> (-> Prompt-Value) * Prompt-Value) Prompt-Implementation))
(define ((model-checker-prompt amb) _meta op)
  (case (car op)
    [(choose) (model-checker-choose amb op)]
    [(integer) (values (current-model-checker-integer-value) '())]
    [(natural) (values (current-model-checker-natural-value) '())]
    [(positive-integer) (values (current-model-checker-positive-integer-value) '())]
    [(string) (values (current-model-checker-string-value) '())]
    [(range) (model-checker-range amb op)]
    [(random) (model-checker-random amb op)]))

(: model-checker-choose (-> (-> (-> Prompt-Value) * Prompt-Value)
                            (U (List 'choose Procedure (Listof String))
                               (List 'choose (Listof String)))
                            (Values String Prompt-Attributes)))
(define (model-checker-choose amb op)
  (let* ([choices (if (procedure? (second op))
                      (third op)
                      (second op))]
         [value (let loop : Prompt-Value ([choices choices])
                  (if (null? choices)
                      (amb)
                      (amb (thunk (car choices))
                           (thunk (loop (cdr choices))))))])
    (values (assert value string?) '())))

(: model-checker-range (case-> (-> (-> (-> Prompt-Value) * Prompt-Value)
                                   (List 'range Positive-Integer Positive-Integer) (Values Positive-Integer Prompt-Attributes))
                               (-> (-> (-> Prompt-Value) * Prompt-Value)
                                   (List 'range Natural Natural) (Values Natural Prompt-Attributes))
                               (-> (-> (-> Prompt-Value) * Prompt-Value)
                                   (List 'range Integer Integer) (Values Integer Prompt-Attributes))))
(define (model-checker-range amb op)
  (let* ([from (second op)]
         [to : Integer (third op)])
    (unless (<= from to)
      (error 'model-checker-prompt "invalid range ~a...~a" from to))
    (let ([value (let loop : Prompt-Value ([i from])
                   (if (< to i)
                       (amb)
                       (amb (thunk i)
                            (thunk (loop (add1 i))))))])
      (if (and (exact? value) (integer? value)
               (<= from value) (<= value to))
          (values value '())
          (error 'model-checker-prompt "invalid range value ~a" value)))))

(: model-checker-random (-> (-> (-> Prompt-Value) * Prompt-Value) (List 'random Positive-Integer) (Values Natural Prompt-Attributes)))
(define (model-checker-random amb op)
  (let ([n (second op)])
    (values (assert (let loop : Prompt-Value ([i 0])
                      (if (= i n)
                          (amb)
                          (amb (thunk i)
                               (thunk (loop (add1 i))))))
                    natural?)
            '())))
