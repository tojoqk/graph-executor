#lang typed/racket

(provide Journal-Entry journal-entry? journal-undo
         (rename-out [journal-entry* journal-entry])
         journal-entry-edge-mode journal-entry-edge-name journal-entry-edge-extra journal-entry-prompt-records)

(require "prompt.rkt")

(struct journal-entry ([edge-mode : (U 'auto 'choose)]
                       [edge-name : String]
                       [edge-extra : Any]
                       [prompt-records : (Listof Prompt-Record)])
  #:prefab #:type-name Journal-Entry)

(: journal-entry* (-> (U 'auto 'choose) String [#:edge-extra Any] [#:prompt-records (Listof Prompt-Record)] Journal-Entry))
(define (journal-entry* mode name #:edge-extra [extra #f] #:prompt-records [prompt-records '()])
  (journal-entry mode name extra prompt-records))

(: journal-undo (-> (Listof Journal-Entry) (Listof Journal-Entry)))
(define (journal-undo j)
  (cond [(memf (lambda ([e : Journal-Entry]) (symbol=? (journal-entry-edge-mode e) 'choose)) j) => cdr]
        [else '()]))
