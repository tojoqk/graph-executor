#lang typed/racket

(require "../../prompt.rkt")

(provide console-prompt
         current-console-random-prompt-display)

(: current-console-random-prompt-display (Parameterof (U 'show 'hide)))
(define current-console-random-prompt-display (make-parameter 'hide))

(: console-prompt (All (A) (Prompt-Implementation A)))
(define (console-prompt title op)
  (values (case (car op)
            [(choose) ((inst console-choose A) title op)]
            [(integer natural positive) (console-input-number title op)]
            [(string) (console-string title op)]
            [(range) (console-range title op)]
            [(random) (console-random title op)])
          '()))

(: console-choose (All (A)
                       (-> String (List 'choose
                                        (-> Any Boolean : #:+ A)
                                        (Listof (U (∩ String A)
                                                   (List (∩ String A) String))))
                           (∩ String A))))
(define (console-choose title op)
  (: choice->target (-> (U (∩ String A) (List (∩ String A) String))
                        (∩ String A)))
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
                          (choice->target (list-ref choices (sub1 n)))
                          (retry)))]
                [else (retry)]))))))

(: console-input-number (case-> (-> String (List 'integer) Integer)
                             (-> String (List 'natural) Natural)
                             (-> String (List 'positive) Positive-Integer)))
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
                     (if (and (exact? value) (integer? value)) value (retry))]
                    [(natural)
                     (if (and (exact? value) (natural? value)) value (retry))]
                    [(positive)
                     (if (and (exact? value) (positive-integer? value)) value (retry))]))]
            [else (retry)]))))

(: console-string (case-> (-> String (List 'string) String)))
(define (console-string title op)
  (newline)
  (printf "* ~a\n" title)
  (let retry ()
    (printf "? ")
    (let ([value (read-line)])
      (if (or (eof-object? value)
              (regexp-match #rx"^\\s*$" value))
          (retry)
          value))))

(: console-range (case-> (-> String (List 'range Positive-Integer Positive-Integer) Positive-Integer)
                      (-> String (List 'range Natural Natural) Natural)
                      (-> String (List 'range Integer Integer) Integer)))
(define (console-range title op)
  (newline)
  (printf "* ~a\n" title)
  (let ([from (second op)]
        [to (third op)])
    (let retry ()
      (printf "(~a..~a)? " from to)
      (let ([line (read-line)])
        (cond [(eof-object? line) (retry)]
              [(string->number line)
               => (lambda ([value : Number])
                    (if (and (exact? value) (integer? value)
                             (<= from value) (<= value to))
                        value
                        (retry)))]
              [else (retry)])))))

(: console-random (-> String (List 'random Positive-Integer) Natural))
(define (console-random title op)
  (let ([r (random (second op))])
    (case (current-console-random-prompt-display)
      [(show)
       (newline)
       (printf "* ~a\n" title)
       (printf "(random) > ~a\n" r)
       r]
      [(hide) r])))
