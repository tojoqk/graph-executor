#lang typed/racket

(provide Journal journal? Journal-Entry journal-undo
         Journal-Logger make-journal-logger
         journal-logger-prompt-log!
         journal-logger->journal-entry)

(require "prompt.rkt")

(define-type Journal-Entry (List* (U 'auto 'choose)
                                  (Pairof String Prompt-Attributes)
                                  (Listof (Pairof Prompt-Value Prompt-Attributes))))
(define-type Journal (Listof Journal-Entry))
(define-predicate journal? Journal)

(: journal-undo (-> Journal Journal))
(define (journal-undo j)
  (cond [(memf (lambda ([e : Journal-Entry]) (symbol=? (car e) 'choose)) j) => cdr]
        [else '()]))

(define-type Journal-Logger
  (List (U (List 'auto (List String))
           (List 'choose (Pairof String Prompt-Attributes)))
        (Boxof (Listof (Pairof Prompt-Value Prompt-Attributes)))))

(: make-journal-logger (case-> (-> 'auto String Journal-Logger)
                               (-> 'choose String Prompt-Attributes Journal-Logger)))

(define make-journal-logger
  (case-lambda
    [(_ name)
     (list (list 'auto (list name))
           (box '()))]
    [(_ name attrs)
     (list (list 'choose (cons name attrs))
           (box '()))]))

(: journal-logger-prompt-log! (-> Journal-Logger
                                  Prompt-Value
                                  Prompt-Attributes
                                  Void))
(define (journal-logger-prompt-log! logger val attrs)
  (let ([bx (second logger)])
    (set-box! bx (cons `(,val ,@attrs) (unbox bx)))))

(: journal-logger->journal-entry (-> Journal-Logger Journal-Entry))
(define (journal-logger->journal-entry logger)
  (list* (first (first logger))
         (second (first logger))
         (unbox (second logger))))
