#lang typed/racket

(require "../prompt.rkt")

(provide console-prompt
         current-console-random-prompt-display)

(: current-console-random-prompt-display (Parameterof (U 'show 'hide)))
(define current-console-random-prompt-display (make-parameter 'hide))

(: console-prompt Prompt-Implementation)
(define (console-prompt meta op)
  (case (car op)
    [(choose) (console-choose meta op)]
    [(integer natural positive-integer) (console-input-number meta op)]
    [(string) (console-string meta op)]
    [(range) (console-range meta op)]
    [(random) (console-random meta op)]))

(: console-choose (-> Prompt-Meta (U (List 'choose Procedure (Listof String))
                                     (List 'choose (Listof String)))
                      (Values String Prompt-Attributes)))
(define (console-choose meta op)
  (let ([choices (if (procedure? (second op))
                     (third op)
                     (second op))]
        [out (open-output-string)])
    (newline)
    (fprintf out "* ~a\n" (prompt-meta-title meta))
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
                          (values (list-ref choices (sub1 n)) '())
                          (retry)))]
                [else (retry)]))))))

(: console-input-number (case-> (-> Prompt-Meta (List 'integer) (Values Integer Prompt-Attributes))
                                (-> Prompt-Meta (List 'natural) (Values Natural Prompt-Attributes))
                                (-> Prompt-Meta (List 'positive-integer) (Values Positive-Integer Prompt-Attributes))))
(define (console-input-number meta op)
  (newline)
  (printf "* ~a\n" (prompt-meta-title meta))
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

(: console-string (case-> (-> Prompt-Meta (List 'string) (Values String Prompt-Attributes))))
(define (console-string meta op)
  (newline)
  (printf "* ~a\n" (prompt-meta-title meta))
  (let retry ()
    (printf "? ")
    (let ([value (read-line)])
      (if (or (eof-object? value)
              (regexp-match #rx"^\\s*$" value))
          (retry)
          (values value '())))))

(: console-range (case-> (-> Prompt-Meta (List 'range Positive-Integer Positive-Integer) (Values Positive-Integer Prompt-Attributes))
                         (-> Prompt-Meta (List 'range Natural Natural) (Values Natural Prompt-Attributes))
                         (-> Prompt-Meta (List 'range Integer Integer) (Values Integer Prompt-Attributes))))
(define (console-range meta op)
  (newline)
  (printf "* ~a\n" (prompt-meta-title meta))
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

(: console-random (-> Prompt-Meta (List 'random Positive-Integer) (Values Natural Prompt-Attributes)))
(define (console-random meta op)
  (let ([r (random (second op))])
    (values (case (current-console-random-prompt-display)
              [(show) (newline)
                      (printf "* ~a\n" (prompt-meta-title meta))
                      (printf "(random) > ~a\n" r)
                      r]
              [(hide) r])
            '())))
