#lang typed/racket

(provide Journal journal? Journal-Entry journal-undo
         auto-journal-entry choose-journal-entry)

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

(: auto-journal-entry (-> String (Listof (Pairof Prompt-Value Prompt-Attributes)) Journal-Entry))
(define (auto-journal-entry name prompt-value)
  `(auto (,name) ,@prompt-value))

(: choose-journal-entry (-> String Prompt-Attributes (Listof (Pairof Prompt-Value Prompt-Attributes)) Journal-Entry))
(define (choose-journal-entry name attrs prompt-values)
  `(choose (,name ,@attrs) ,@prompt-values))
