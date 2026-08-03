#lang typed/racket

(require "./emitter.rkt")

(provide make-amb)

(: make-amb (All (A B) (-> (Values (-> (-> (Values (Option A) B))
                                       (-> B)
                                       (Values (Option A) B))
                                   (-> (Listof (-> A)) A)))))
(define (make-amb)
  (: amb-tag (Prompt-Tagof (Option A)
                           (-> (-> (Option A))
                               (Option A))))
  (define amb-tag (make-continuation-prompt-tag 'amb))

  (define-values (call-with-emitter-prompt emit)
    ((inst make-emitter (Option A) B)))

  (: call-with-amb-prompt (-> (-> (Values (Option A) B))
                              (-> B)
                              (Values (Option A) B)))
  (define (call-with-amb-prompt proc default)
    (define-values (x ctx)
      (call-with-emitter-prompt
       (lambda () : (Option A)
         (call-with-continuation-prompt
          (lambda ()
            (define-values (x ctx) (proc))
            (emit ctx)
            x)
          amb-tag))))
    (if (null? ctx)
        (values x (default))
        (values x (first ctx))))

  (: amb (-> (Listof (-> A)) A))
  (define (amb xs)
    (call-with-composable-continuation
     (lambda ([k : (-> A (Option A))])
       (let ([k (lambda ([n : A])
                  (call-with-continuation-prompt (lambda () (k n)) amb-tag))])
         (abort-current-continuation
          amb-tag
          (lambda () : (Option A)
            (let loop ([xs xs])
              (cond [(null? xs) #f]
                    [(k ((car xs))) => identity]
                    [else (loop (cdr xs))]))))))
     amb-tag))

  (values call-with-amb-prompt amb))
