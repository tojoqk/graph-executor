#lang typed/racket

(provide make-consumer)

(require "amb.rkt")
(require "state.rkt")

(: make-consumer (All (A B C) (-> (Values (-> (-> A) (Listof B) (-> C) (U A C))
                                          (-> B)))))
(define (make-consumer)
  (define-values (call-with-amb-prompt amb) ((inst make-amb A C)))
  (define-values (call-with-state-prompt get set) ((inst make-state A (Listof B))))

  (: call-with-consumer-prompt (-> (-> A) (Listof B) (-> C) (U A C)))
  (define (call-with-consumer-prompt proc lst fail)
    (call-with-amb-prompt
     (lambda ()
       (define-values (x _)
         (call-with-state-prompt proc lst))
       x)
     fail))

  (: consume (-> B))
  (define (consume)
    (let ([lst : (Listof B) (get)])
      (when (null? lst) (amb))
      (set (cdr lst))
      (car lst)))

  (values call-with-consumer-prompt consume))
