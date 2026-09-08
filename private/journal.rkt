#lang typed/racket

(provide Journal journal?
         Journal-Entry journal-entry? (rename-out [auto* auto] [auto*? auto?] [choice* choice] [choice*? choice?])
         journal-entry-edge-mode journal-entry-edge-name journal-entry-edge-extra journal-entry-prompt-records
         journal-undo)

(require "prompt.rkt")

(define-type Journal (Listof Journal-Entry))
(define-predicate journal? Journal)

(: journal-undo (-> Journal Journal))
(define (journal-undo j)
  (cond [(memf (lambda ([e : Journal-Entry]) (symbol=? (journal-entry-edge-mode e) 'choice)) j) => cdr]
        [else '()]))

(struct auto ([edge-name : String]
              [prompt-records : (Listof Prompt-Record)])
  #:prefab #:type-name Auto-Journal-Entry)

(: auto* (-> String [#:prompt-records (Listof Prompt-Record)] Auto-Journal-Entry))
(define (auto* name #:prompt-records [prompt-records '()])
  (auto name prompt-records))

(struct choice ([edge-name : String]
                [edge-extra : Any]
                [prompt-records : (Listof Prompt-Record)])
  #:prefab #:type-name Choice-Journal-Entry)

(define-predicate auto*? Auto-Journal-Entry)

(: choice* (-> String [#:edge-extra Any] [#:prompt-records (Listof Prompt-Record)] Choice-Journal-Entry))
(define (choice* name #:edge-extra [extra #f] #:prompt-records [prompt-records '()])
  (choice name extra prompt-records))

(define-predicate choice*? Choice-Journal-Entry)

(define-type Journal-Entry (U Auto-Journal-Entry Choice-Journal-Entry))
(define-predicate journal-entry? Journal-Entry)

(: journal-entry-edge-name (-> Journal-Entry String))
(define (journal-entry-edge-name e)
  (cond [(auto*? e) (auto-edge-name e)]
        [(choice*? e) (choice-edge-name e)]))

(: journal-entry-edge-mode (-> Journal-Entry (U 'auto 'choice)))
(define (journal-entry-edge-mode e)
  (cond [(auto*? e) 'auto]
        [(choice*? e) 'choice]))

(: journal-entry-edge-extra (-> Journal-Entry Any))
(define (journal-entry-edge-extra e)
  (cond [(auto*? e) #f]
        [(choice*? e) (choice-edge-extra e)]))

(: journal-entry-prompt-records (-> Journal-Entry (Listof Prompt-Record)))
(define (journal-entry-prompt-records e)
  (cond [(auto*? e) (auto-prompt-records e)]
        [(choice*? e) (choice-prompt-records e)]))
