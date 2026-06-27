#lang typed/racket

(require "../prompt.rkt")

(provide repl-prompt)

(: repl-prompt (All (A) (Prompt A)))
(define (repl-prompt title op)
  (let ([value
         (case (car op)
           [(choose) ((inst repl-choose A) title op)]
           [(integer natural positive) (repl-input-number title op)]
           [(string) (repl-string title op)]
           [(range) (repl-range title op)]
           [(random) (repl-random title op)])])
    value))

(: repl-choose (All (A)
                    (-> String (List 'choose
                                     (-> Any Boolean : #:+ A)
                                     (Listof (U (∩ (U Symbol String) A)
                                                (List (∩ (U Symbol String False) A) String))))
                        (∩ (U Symbol String False) A))))
(define (repl-choose title op)
  (: choice->target (-> (U (∩ (U Symbol String) A) (List (∩ (U Symbol String False) A) String))
                        (∩ (U Symbol String False) A)))
  (define (choice->target c) (if (pair? c) (car c) c))
  (let ([choices (third op)])
    (printf "* ~a\n" title)
    (for ([choice choices]
          [i : Positive-Integer (in-naturals 1)])
      (if (pair? choice)
          (cond [(car choice)
                 => (lambda ([target : (U Symbol String)])
                      (printf "- [~a] ~a: ~a\n" i target (cadr choice)))]
                [else (printf "- [~a] ~a\n" i (cadr choice))])
          (printf "  - [~a] ~a\n" i (choice->target choice))))
    (let retry ()
      (display "? ")
      (let ([n (read)])
        (if (and (exact? n)
                 (positive-integer? n)
                 (<= n (length choices)))
            (choice->target (list-ref choices (sub1 n)))
            (retry))))))

(: repl-input-number (case-> (-> String (List 'integer) Integer)
                             (-> String (List 'natural) Natural)
                             (-> String (List 'positive) Positive-Integer)))
(define (repl-input-number title op)
  (printf "* ~a\n" title)
  (let retry ()
    (printf "? " (car op))
    (let ([value (read)])
      (case (car op)
        [(integer)
         (if (and (exact? value) (integer? value)) value (retry))]
        [(natural)
         (if (and (exact? value) (natural? value)) value (retry))]
        [(positive)
         (if (and (exact? value) (positive-integer? value)) value (retry))]))))

(: repl-string (case-> (-> String (List 'string) String)))
(define (repl-string title op)
  (printf "* ~a\n" title)
  (let retry ()
    (printf "? " (car op))
    (let ([value (read-line)])
      (if (eof-object? value)
          (retry)
          value))))

(: repl-range (case-> (-> String (List 'range 'from Natural 'to Natural) Natural)
                      (-> String (List 'range 'from Integer 'to Integer) Integer)))
(define (repl-range title op)
  (printf "* ~a\n" title)
  (let ([from (third op)]
        [to (fifth op)])
    (let retry ()
      (printf "(~a..~a)? " from to)
      (let ([value (read)])
        (if (and (exact? value) (integer? value)
                 (<= from value) (<= value to))
            value
            (retry))))))

(: repl-random (-> String (List 'random Positive-Integer) Natural))
(define (repl-random title op)
  (printf "* ~a\n" title)
  (let ([r (random (second op))])
    (printf "(random) > ~a\n" r)
    r))
