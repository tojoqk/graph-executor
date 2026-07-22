#lang typed/racket

(require "../prompt.rkt")

(provide console-prompt
         current-console-random-prompt-display)

(: current-console-random-prompt-display (Parameterof (U 'show 'hide)))
(define current-console-random-prompt-display (make-parameter 'hide))

(: console-prompt Prompt-Implementation)
(define (console-prompt title op)
  (case (car op)
    [(choose) (console-choose title op)]
    [(integer natural positive-integer) (console-input-number title op)]
    [(string) (console-string title op)]
    [(range) (console-range title op)]
    [(random) (console-random title op)]))

(: console-choose (-> String (U (List 'choose Procedure (Listof String))
                                (List 'choose (Listof String)))
                      (Values String Prompt-Attributes)))
(define (console-choose title op)
  (let ([choices (if (procedure? (second op))
                     (third op)
                     (second op))]
        [out (open-output-string)])
    (newline)
    (fprintf out "* ~a\n" title)
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
                          (values (list-ref choices n) '())
                          (retry)))]
                [else (retry)]))))))

(: console-input-number (case-> (-> String (List 'integer) (Values Integer Prompt-Attributes))
                                (-> String (List 'natural) (Values Natural Prompt-Attributes))
                                (-> String (List 'positive-integer) (Values Positive-Integer Prompt-Attributes))))
(define (console-input-number title op)
  (newline)
  (printf "* ~a\n" title)
  (let retry ()
    (printf "? ")
    (let ([line (read-line)])
      (cond [(eof-object? line) (retry)]
            [(string->number line)
             => (lambda ([value : Number])
                  (case (car op)
                    [(integer)
                     (if (and (exact? value) (integer? value))
                         (values value '())
                         (retry))]
                    [(natural)
                     (if (and (exact? value) (natural? value))
                         (values value '())
                         (retry))]
                    [(positive-integer)
                     (if (and (exact? value) (positive-integer? value))
                         (values value '())
                         (retry))]))]
            [else (retry)]))))

(: console-string (case-> (-> String (List 'string) (Values String Prompt-Attributes))))
(define (console-string title op)
  (newline)
  (printf "* ~a\n" title)
  (let retry ()
    (printf "? ")
    (let ([value (read-line)])
      (if (or (eof-object? value)
              (regexp-match #rx"^\\s*$" value))
          (retry)
          (values value '())))))

(: console-range (case-> (-> String (List 'range Positive-Integer Positive-Integer) (Values Positive-Integer Prompt-Attributes))
                         (-> String (List 'range Natural Natural) (Values Natural Prompt-Attributes))
                         (-> String (List 'range Integer Integer) (Values Integer Prompt-Attributes))))
(define (console-range title op)
  (newline)
  (printf "* ~a\n" title)
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
                        (values value '())
                        (retry)))]
              [else (retry)])))))

(: console-random (-> String (List 'random Positive-Integer) (Values Natural Prompt-Attributes)))
(define (console-random title op)
  (let ([r (random (second op))])
    (values (case (current-console-random-prompt-display)
              [(show) (newline)
                      (printf "* ~a\n" title)
                      (printf "(random) > ~a\n" r)
                      r]
              [(hide) r])
            '())))
