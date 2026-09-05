#lang typed/racket

(provide Message message current-message
         Message-Result message-result message-result-message)

(define-type Message (-> Any Void))

(: current-message (Parameterof (Option Message)))
(define current-message (make-parameter #f))

(: message Message)
(define (message obj)
  (cond [(current-message) => (lambda ([msg : Message]) (msg obj))]
        [else (error 'message "called outside of trans")]))

(define-type Message-Result (List 'message Any))
(: message-result (-> Any Message-Result))
(define (message-result x)
  (list 'message x))

(: message-result-message (-> Message-Result Any))
(define (message-result-message x) (second x))
