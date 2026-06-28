#lang typed/racket

(require "../../prompt.rkt")

(provide repl-prompt)

(: repl-prompt (All (A) (-> (-> String Void) (Prompt A))))
(define ((repl-prompt log-prompt) title op [_ (hash)])
  (define-values (value prompt-text)
    (case (car op)
      [(const) (values (third op) title)]
      [(choose) ((inst repl-choose A) title op)]
      [(integer natural positive) (values (repl-input-number title op) title)]
      [(string) (values (repl-string title op) title)]
      [(range) (values (repl-range title op) title)]
      [(random) (values (repl-random title op) title)]))
  (log-prompt prompt-text)
  value)

(: repl-choose (All (A)
                    (-> String (List 'choose
                                     (-> Any Boolean : #:+ A)
                                     (Listof (U (∩ String A)
                                                (List (∩ String A) String))))
                        (Values (∩ String A) String))))
(define (repl-choose title op)
  (: choice->target (-> (U (∩ String A) (List (∩ String A) String))
                        (∩ String A)))
  (define (choice->target c) (if (pair? c) (car c) c))
  (let ([choices (third op)]
        [out (open-output-string)])
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
        (let ([n (read)])
          (if (and (exact? n)
                   (positive-integer? n)
                   (<= n (length choices)))
              (values (choice->target (list-ref choices (sub1 n))) text)
              (retry)))))))

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
