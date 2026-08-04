#lang typed/racket

(provide make-amb)

(: make-amb (All (A B) (-> (Values (case-> (-> (-> A) (Option A))
                                           (-> (-> A) (-> B) (U A B)))
                                   (-> (-> A) * A)
                                   (-> Nothing)))))
(define (make-amb)
  (: amb-tag (Prompt-Tagof (Option (List A))
                           (-> (-> (Option (List A))) (Option (List A)))))
  (define amb-tag (make-continuation-prompt-tag 'amb))

  (: call-with-amb-prompt (case-> (-> (-> A) (Option A))
                                  (-> (-> A) (-> B) (U A B))))
  (define call-with-amb-prompt
    (case-lambda
      [(proc)
       (cond [(call-with-continuation-prompt (lambda () (list (proc))) amb-tag) => car]
             [else #f])]
      [(proc fail)
       (cond [(call-with-continuation-prompt (lambda () (list (proc))) amb-tag) => car]
             [else (fail)])]))

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

  (values call-with-amb-prompt amb amb-fail))
