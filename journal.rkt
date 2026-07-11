#lang typed/racket

(provide Journal Journal-Entry)

(require "prompt.rkt")

(define-type Journal-Entry (Pairof (Pairof String Prompt-Attributes)
                                   (Listof (Pairof Prompt-Value Prompt-Attributes))))
(define-type Journal (Listof Journal-Entry))
