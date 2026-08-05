#lang typed/racket

(provide make-consumer)

(require "state.rkt")

(: make-consumer (All (A B) (-> (Values (-> (-> B) (Listof A) (Pairof (Listof A) B))
                                        (-> (-> Nothing) A)))))
(define (make-consumer)
  (define-values (call-with-state get set) ((inst make-state (Listof A) B)))

  (: call-with-consumer (-> (-> B) (Listof A) (Pairof (Listof A) B)))
  (define (call-with-consumer proc lst)
    (call-with-state proc lst))

  (: consume (-> (-> Nothing) A))
  (define (consume fail)
    (let ([lst : (Listof A) (get)])
      (if (null? lst)
          (fail)
          (begin (set (cdr lst))
                 (car lst)))))

  (values call-with-consumer consume))
