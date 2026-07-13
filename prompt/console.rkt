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

(: console-choose (-> String (List 'choose
                                   Any
                                   (Listof (U String
                                              (List String String))))
                      Prompt-Info-Choose))
(define (console-choose title op)
  (: choice->target (-> (U String (List String String))
                        String))
  (define (choice->target c) (if (pair? c) (car c) c))
  (let ([choices (third op)]
        [out (open-output-string)])
    (newline)
    (fprintf out "* ~a\n" title)
    (for ([choice choices]
          [i : Positive-Integer (in-naturals 1)])
      (if (pair? choice)
          (cond [(car choice)
                 => (lambda ([target : String])
                      (fprintf out "- [~a] ~a: ~a\n" i (car choice) (cadr choice)))])
          (fprintf out "  - [~a] ~a\n" i (choice->target choice))))
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
                          (prompt-info-choose title
                                              '()
                                              (choice->target (list-ref choices (sub1 n)))
                                              (third op))
                          (retry)))]
                [else (retry)]))))))

(: console-input-number (case-> (-> String (List 'integer) Prompt-Info-Integer)
                                (-> String (List 'natural) Prompt-Info-Natural)
                                (-> String (List 'positive-integer) Prompt-Info-Positive-Integer)))
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
                         (prompt-info-integer title '() value)
                         (retry))]
                    [(natural)
                     (if (and (exact? value) (natural? value))
                         (prompt-info-natural title '() value)
                         (retry))]
                    [(positive-integer)
                     (if (and (exact? value) (positive-integer? value))
                         (prompt-info-positive-integer title '() value)
                         (retry))]))]
            [else (retry)]))))

(: console-string (case-> (-> String (List 'string) Prompt-Info-String)))
(define (console-string title op)
  (newline)
  (printf "* ~a\n" title)
  (let retry ()
    (printf "? ")
    (let ([value (read-line)])
      (if (or (eof-object? value)
              (regexp-match #rx"^\\s*$" value))
          (retry)
          (prompt-info-string title '() value)))))

(: console-range (-> String (List 'range Integer Integer) Prompt-Info-Range))
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
                        (prompt-info-range title '() value from to)
                        (retry)))]
              [else (retry)])))))

(: console-random (-> String (List 'random Positive-Integer) Prompt-Info-Random))
(define (console-random title op)
  (let ([r (random (second op))])
    (prompt-info-random title
                        '()
                        (case (current-console-random-prompt-display)
                          [(show)
                           (newline)
                           (printf "* ~a\n" title)
                           (printf "(random) > ~a\n" r)
                           r]
                          [(hide) r])
                        (second op))))
