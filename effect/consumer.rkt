#lang typed/racket

(provide make-consumer)

(require "state.rkt")

(: make-consumer (All (A B) (-> (Values (-> (-> A) (Listof B) (Values A (Listof B)))
                                        (-> (-> Nothing) B)))))
(define (make-consumer)
  (define-values (call-with-state-prompt get set) ((inst make-state A (Listof B))))

  (: call-with-consumer-prompt (-> (-> A) (Listof B) (Values A (Listof B))))
  (define (call-with-consumer-prompt proc lst)
    (call-with-state-prompt proc lst))

  (: consume (-> (-> Nothing) B))
  (define (consume fail)
    (let ([lst : (Listof B) (get)])
      (if (null? lst)
          (fail)
          (begin (set (cdr lst))
                 (car lst)))))

  (values call-with-consumer-prompt consume))
