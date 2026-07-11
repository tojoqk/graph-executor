#lang typed/racket

(provide Journal Prompt-Logger make-prompt-logger prompt-logger-log! prompt-logger->entry)

(require "prompt.rkt")

(define-type Journal (Listof (Pairof (Pairof String Prompt-Attributes)
                                     (Listof (Pairof Prompt-Value Prompt-Attributes)))))
(define-type Prompt-Logger (Pairof (Pairof String Prompt-Attributes) (Boxof (Listof (Pairof Prompt-Value Prompt-Attributes)))))

(: make-prompt-logger (-> String Prompt-Attributes Prompt-Logger))
(define (make-prompt-logger edge-name attrs)
  (cons ((inst cons String Prompt-Attributes) edge-name attrs)
        ((inst box (Listof (Pairof Prompt-Value Prompt-Attributes))) '())))

(: prompt-logger-log! (-> Prompt-Logger Prompt-Value Prompt-Attributes Void))
(define (prompt-logger-log! logger val attrs)
  (let ([b (cdr logger)])
    (set-box! b (cons ((inst cons Prompt-Value Prompt-Attributes) val attrs) (unbox b)))))

(: prompt-logger->entry (-> Prompt-Logger
                            (Pairof (Pairof String Prompt-Attributes)
                                    (Listof (Pairof Prompt-Value Prompt-Attributes)))))
(define (prompt-logger->entry logger)
  (cons (car logger) (unbox (cdr logger))))
