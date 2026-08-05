#lang typed/racket

(provide make-emitter)

(: make-emitter (All (A B) (-> (Values (-> (-> B) (Pairof (Listof A) B))
                                       (-> A Void)))))
(define (make-emitter)
  (: emitter-tag (Prompt-Tagof (Pairof (Listof A) B)
                               (-> (-> (Pairof (Listof A) B))
                                   (Pairof (Listof A) B))))
  (define emitter-tag (make-continuation-prompt-tag 'emitter))

  (: call-with-emitter (-> (-> B) (Pairof (Listof A) B)))
  (define (call-with-emitter proc)
    (call-with-continuation-prompt (lambda () `(() . ,(proc))) emitter-tag))

  (: emit (-> A Void))
  (define (emit str)
    (call-with-composable-continuation
     (lambda ([k : (-> (Pairof (Listof A) B))])
       (let ([k (lambda () (call-with-continuation-prompt k emitter-tag))])
         (abort-current-continuation
          emitter-tag
          (lambda () : (Pairof (Listof A) B)
            (let ([p : (Pairof (Listof A) B) (k)])
              `(,(cons str (car p)) . ,(cdr p)))))))
     emitter-tag)
    (void))

  (values call-with-emitter emit))
