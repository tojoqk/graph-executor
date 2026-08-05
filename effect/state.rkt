#lang typed/racket

(provide make-state)

(: make-state (All (A B) (-> (Values (-> (-> B) A (Pairof A B))
                                     (-> A)
                                     (-> A Void)))))
(define (make-state)
  (: state-tag (Prompt-Tagof (-> A (Values A B))
                             (-> (-> (-> A (Values A B)))
                                 (-> A (Values A B)))))
  (define state-tag (make-continuation-prompt-tag 'state))

  (: call-with-state (-> (-> B) A (Pairof A B)))
  (define (call-with-state proc st)
    (define f
      (call-with-continuation-prompt
       (lambda ()
         (let ([x (proc)])
           (lambda ([st : A]) (values st x))))
       state-tag))
    (define-values (result-st x) (f st))
    (cons result-st x))

  (: get (-> A))
  (define (get)
    (call-with-composable-continuation
     (lambda ([k : (-> A (-> A (Values A B)))])
       (let ([k (lambda ([st : A])
                  (call-with-continuation-prompt (lambda () (k st)) state-tag))])
         (abort-current-continuation
          state-tag
          (lambda () : (-> A (Values A B))
            (lambda ([st : A])
              ((k st) st))))))
     state-tag))

  (: set (-> A Void))
  (define (set st)
    (call-with-composable-continuation
     (lambda ([k : (-> (-> A (Values A B)))])
       (let ([k (lambda ()
                  (call-with-continuation-prompt k state-tag))])
         (abort-current-continuation
          state-tag
          (lambda () : (-> A (Values A B))
            (let ([f (k)])
              (lambda (_) (f st)))))))
     state-tag)
    (void))

  (values call-with-state get set))
