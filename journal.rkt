#lang typed/racket

(provide Journal Journal-Entry journal-undo)

(require "prompt.rkt")

(define-type Journal-Entry (List* (U 'auto 'choose)
                                  (Pairof String Prompt-Attributes)
                                  (Listof (Pairof Prompt-Value Prompt-Attributes))))
(define-type Journal (Listof Journal-Entry))

(: journal-undo (-> Journal Journal))
(define (journal-undo j)
  (cond [(memf (lambda ([e : Journal-Entry]) (symbol=? (car e) 'choose)) j) => cdr]
        [else '()]))
