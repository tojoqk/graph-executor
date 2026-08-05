#lang typed/racket

(provide make-consumer)

(require "state.rkt")

(: make-consumer (All (A B) (-> (Values (-> (-> A) (Listof B) (Values A (Listof B)))
                                        (-> (-> Nothing) B)))))
(define (make-consumer)
  (define-values (call-with-state get set) ((inst make-state A (Listof B))))

  (: call-with-consumer (-> (-> A) (Listof B) (Values A (Listof B))))
  (define (call-with-consumer proc lst)
    (call-with-state proc lst))

  (: consume (-> (-> Nothing) B))
  (define (consume fail)
    (let ([lst : (Listof B) (get)])
      (if (null? lst)
          (fail)
          (begin (set (cdr lst))
                 (car lst)))))

  (values call-with-consumer consume))
