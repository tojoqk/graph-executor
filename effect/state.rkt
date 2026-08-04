#lang typed/racket

(provide make-state)

(: make-state (All (A B) (-> (Values (-> (-> A) B (Values A B))
                                     (-> B)
                                     (-> B Void)))))
(define (make-state)
  (: state-tag (Prompt-Tagof (-> B (Values A B))
                             (-> (-> (-> B (Values A B)))
                                 (-> B (Values A B)))))
  (define state-tag (make-continuation-prompt-tag 'state))

  (: call-with-state-prompt (-> (-> A) B (Values A B)))
  (define (call-with-state-prompt proc st)
    (define f
      (call-with-continuation-prompt
       (lambda ()
         (let ([x (proc)])
           (lambda ([st : B]) (values x st))))
       state-tag))
    (f st))

  (: get (-> B))
  (define (get)
    (call-with-composable-continuation
     (lambda ([k : (-> B (-> B (Values A B)))])
       (let ([k (lambda ([st : B])
                  (call-with-continuation-prompt (lambda () (k st)) state-tag))])
         (abort-current-continuation
          state-tag
          (lambda () : (-> B (Values A B))
            (lambda ([st : B])
              ((k st) st))))))
     state-tag))

  (: set (-> B Void))
  (define (set st)
    (call-with-composable-continuation
     (lambda ([k : (-> (-> B (Values A B)))])
       (let ([k (lambda ()
                  (call-with-continuation-prompt k state-tag))])
         (abort-current-continuation
          state-tag
          (lambda () : (-> B (Values A B))
            (let ([f (k)])
              (lambda (_) (f st)))))))
     state-tag)
    (void))

  (values call-with-state-prompt get set))
