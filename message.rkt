#lang typed/racket

(provide Message message current-message
         Message-Info (struct-out message-info))

(define-type Message (-> Any Void))

(: current-message (Parameterof (Option Message)))
(define current-message (make-parameter #f))

(: message Message)
(define (message obj)
  (cond [(current-message) => (lambda ([msg : Message]) (msg obj))]
        [else (error 'message "called outside of trans")]))

(struct message-info ([message : Any])
  #:type-name Message-Info
  #:transparent)
