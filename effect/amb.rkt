#lang typed/racket

(provide make-amb)

(: make-amb (All (A) (-> (Values (All (B C)
                                      (case-> (-> (-> B) (Option B))
                                              (-> (-> B) (-> C) (U B C))))
                                 (-> (-> A) * A)
                                 (-> Nothing)))))
(define (make-amb)
  (: amb-tag (Prompt-Tagof (Option (List A))
                           (-> (-> (Option (List A))) (Option (List A)))))
  (define amb-tag (make-continuation-prompt-tag 'amb))

  (: call-with-amb (All (B C)
                        (case-> (-> (-> B) (Option B))
                                (-> (-> B) (-> C) (U B C)))))
  (define call-with-amb
    (case-lambda
      [(proc fail)
       (let/cc return : (U B C)
         (%call-with-amb (thunk (return (proc))) (thunk (return (fail))))
         (error 'make-amb "invalid implementation error"))]
      [(proc)
       (let/cc return : (Option B)
         (%call-with-amb (thunk (return (proc))) (thunk (return #f)))
         (error 'make-amb "invalid implementation error"))]))

  (: %call-with-amb (-> (-> A) (-> Nothing) A))
  (define (%call-with-amb proc fail)
    (cond [(call-with-continuation-prompt (lambda () (list (proc))) amb-tag) => car]
          [else (fail)]))

  (: amb (-> (-> A) * A))
  (define (amb . xs)
    (car
     (call-with-composable-continuation
      (lambda ([k : (-> (List A) (Option (List A)))])
        (let ([k (lambda ([x : (List A)])
                   (call-with-continuation-prompt (lambda () (k x)) amb-tag))])
          (abort-current-continuation
           amb-tag
           (lambda () : (Option (List A))
             (let loop ([xs xs])
               (cond [(null? xs) #f]
                     [(k (list ((car xs)))) => identity]
                     [else (loop (cdr xs))]))))))
      amb-tag)))

  (: amb-fail (-> Nothing))
  (define (amb-fail)
    (abort-current-continuation amb-tag (lambda () #f)))

  (values call-with-amb amb amb-fail))
