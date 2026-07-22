#lang typed/racket

(provide Message message current-message
         Message-Info message-info-message)

(define-type Message (-> Any Void))

(: current-message (Parameterof (Option Message)))
(define current-message (make-parameter #f))

(: message Message)
(define (message obj)
  (cond [(current-message) => (lambda ([msg : Message]) (msg obj))]
        [else (error 'message "called outside of trans")]))

(define-type Message-Info (List 'message Any))

(: message-info-message (-> Message-Info Any))
(define (message-info-message x) (second x))
