#lang typed/racket

(provide Journal-Entry journal-entry? (rename-out [auto* auto] [auto*? auto?] [choice* choice] [choice*? choice?])
         journal-entry-edge-mode journal-entry-edge-name journal-entry-edge-extra journal-entry-prompt-records
         journal-undo)

(require "prompt.rkt")

(: journal-undo (-> (Listof Journal-Entry) (Listof Journal-Entry)))
(define (journal-undo j)
  (cond [(memf (lambda ([e : Journal-Entry]) (symbol=? (journal-entry-edge-mode e) 'choice)) j) => cdr]
        [else '()]))

(struct auto ([edge-name : String]
              [prompt-records : (Listof Prompt-Record)])
  #:prefab #:type-name Journal-Entry-Auto)

(: auto* (-> String [#:prompt-records (Listof Prompt-Record)] Journal-Entry-Auto))
(define (auto* name #:prompt-records [prompt-records '()])
  (auto name prompt-records))

(struct choice ([edge-name : String]
                [edge-extra : Any]
                [prompt-records : (Listof Prompt-Record)])
  #:prefab #:type-name Journal-Entry-Choice)

(define-predicate auto*? Journal-Entry-Auto)

(: choice* (-> String [#:edge-extra Any] [#:prompt-records (Listof Prompt-Record)] Journal-Entry-Choice))
(define (choice* name #:edge-extra [extra #f] #:prompt-records [prompt-records '()])
  (choice name extra prompt-records))

(define-predicate choice*? Journal-Entry-Choice)

(define-type Journal-Entry (U Journal-Entry-Auto Journal-Entry-Choice))
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
