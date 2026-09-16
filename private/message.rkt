#lang typed/racket

(provide Message message current-message message-without-trans
         Message-Result message-result message-result-message)

(define-type Message (-> Any Void))

(: current-message (Parameterof Message))
(define current-message (make-parameter displayln))

(: message-without-trans Message)
(define (message-without-trans _obj)
  (error 'message "called outside of trans"))

(: message Message)
(define (message obj)
  ((current-message) obj))

(define-type Message-Result (List 'message Any))
(: message-result (-> Any Message-Result))
(define (message-result x)
  (list 'message x))

(: message-result-message (-> Message-Result Any))
(define (message-result-message x) (second x))
