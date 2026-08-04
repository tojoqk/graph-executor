#lang typed/racket

(provide make-emitter)

(: make-emitter (All (A B) (-> (Values (-> (-> A) (Values A (Listof B)))
                                       (-> B Void)))))
(define (make-emitter)
  (: emitter-tag (Prompt-Tagof (Pairof A (Listof B))
                               (-> (-> (Pairof A (Listof B)))
                                   (Pairof A (Listof B)))))
  (define emitter-tag (make-continuation-prompt-tag 'emitter))

  (: call-with-emitter-prompt (-> (-> A) (Values A (Listof B))))
  (define (call-with-emitter-prompt proc)
    (let ([p (call-with-continuation-prompt (lambda () `(,(proc)))  emitter-tag)])
      (values (car p) (cdr p))))

  (: emit (-> B Void))
  (define (emit str)
    (call-with-composable-continuation
     (lambda ([k : (-> (Pairof A (Listof B)))])
       (let ([k (lambda () (call-with-continuation-prompt k emitter-tag))])
         (abort-current-continuation
          emitter-tag
          (lambda () : (Pairof A (Listof B))
            (let ([p : (Pairof A (Listof B)) (k)])
              `(,(car p) ,@(cons str (cdr p))))))))
     emitter-tag)
    (void))

  (values call-with-emitter-prompt emit))
