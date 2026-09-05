#lang typed/racket

(require "../prompt.rkt")

(provide console-prompt)

(: console-prompt Prompt-Implementation)
(define (console-prompt info op)
  (case (car op)
    [(choose) (console-choose info op)]
    [(integer natural positive-integer) (console-input-number info op)]
    [(string) (console-string info op)]
    [(between) (console-between info op)]
    [(random) (console-random info op)]))

(: console-choose (-> Prompt-Info (U (List 'choose Procedure (Listof Symbol) Procedure))
                      (Values Symbol Any)))
(define (console-choose info op)
  (let ([choices (third op)]
        [out (open-output-string)])
    (newline)
    (fprintf out "* ~a\n" (prompt-info-title info))
    (for ([choice choices]
          [i : Positive-Integer (in-naturals 1)])
      (fprintf out "  - [~a] ~a\n" i choice))
    (let ([text (get-output-string out)])
      (display text)
      (let retry ()
        (display "? ")
        (let ([line (read-line)])
          (cond [(eof-object? line) (retry)]
                [(string->number line)
                 => (lambda ([n : Number])
                      (if (and (exact? n)
                               (positive-integer? n)
                               (<= n (length choices)))
                          (values (list-ref choices (sub1 n)) #f)
                          (retry)))]
                [else (retry)]))))))

(: console-input-number (case-> (-> Prompt-Info (List 'integer) (Values Integer Any))
                                (-> Prompt-Info (List 'natural) (Values Natural Any))
                                (-> Prompt-Info (List 'positive-integer) (Values Positive-Integer Any))))
(define (console-input-number info op)
  (newline)
  (printf "* ~a\n" (prompt-info-title info))
  (let retry ()
    (printf "? ")
    (let ([line (read-line)])
      (cond [(eof-object? line) (retry)]
            [(string->number line)
             => (lambda ([value : Number])
                  (case (car op)
                    [(integer)
                     (if (and (exact? value) (integer? value))
                         (values value #f)
                         (retry))]
                    [(natural)
                     (if (and (exact? value) (natural? value))
                         (values value #f)
                         (retry))]
                    [(positive-integer)
                     (if (and (exact? value) (positive-integer? value))
                         (values value #f)
                         (retry))]))]
            [else (retry)]))))

(: console-string (case-> (-> Prompt-Info (List 'string) (Values String Any))))
(define (console-string info op)
  (newline)
  (printf "* ~a\n" (prompt-info-title info))
  (let retry ()
    (printf "? ")
    (let ([value (read-line)])
      (if (or (eof-object? value)
              (regexp-match #rx"^\\s*$" value))
          (retry)
          (values value #f)))))

(: console-between (case-> (-> Prompt-Info (List 'between Positive-Integer Positive-Integer) (Values Positive-Integer Any))
                         (-> Prompt-Info (List 'between Natural Natural) (Values Natural Any))
                         (-> Prompt-Info (List 'between Integer Integer) (Values Integer Any))))
(define (console-between info op)
  (newline)
  (printf "* ~a\n" (prompt-info-title info))
  (let ([from (second op)]
        [to : Integer (third op)])
    (let retry ()
      (printf "(~a..~a)? " from to)
      (let ([line (read-line)])
        (cond [(eof-object? line) (retry)]
              [(string->number line)
               => (lambda ([value : Number])
                    (if (and (exact? value) (integer? value)
                             (<= from value) (<= value to))
                        (values value #f)
                        (retry)))]
              [else (retry)])))))

(: console-random (-> Prompt-Info (List 'random Positive-Integer) (Values Natural Any)))
(define (console-random info op)
  (values (random (second op)) #f))
