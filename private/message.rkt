#lang typed/racket

(module+ test
  (require typed/rackunit))

(provide Message message current-message message-without-trans
         Message-Result message-result message-result-content
         message-content)

(define-type Message-Content (U String (Promise Any)))
(define-type Message (-> Message-Content Void))

(: message-content (-> Message-Content Any))
(define (message-content content)
  (if (promise? content)
      (force content)
      content))

(module+ test
  (check-equal? (message-content "test") "test")
  (check-equal? (message-content (delay 'test)) 'test))

(: current-message (Parameterof Message))
(define current-message (make-parameter (compose displayln message-content)))

(: message-without-trans Message)
(define (message-without-trans _obj)
  (error 'message "called outside of trans"))

(: message Message)
(define (message obj)
  ((current-message) obj))

(define-type Message-Result (List 'message Message-Content))
(: message-result (-> Message-Content Message-Result))
(define (message-result x)
  (list 'message x))

(: message-result-content (-> Message-Result Any))
(define (message-result-content x) (message-content (second x)))
