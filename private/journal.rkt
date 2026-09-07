#lang typed/racket

(provide Journal-Entry journal-entry? (rename-out [auto* auto] [auto*? auto?] [choose* choose] [choose*? choose?])
         journal-entry-edge-mode journal-entry-edge-name journal-entry-edge-extra journal-entry-prompt-records
         journal-undo)

(require "prompt.rkt")

(: journal-undo (-> (Listof Journal-Entry) (Listof Journal-Entry)))
(define (journal-undo j)
  (cond [(memf (lambda ([e : Journal-Entry]) (symbol=? (journal-entry-edge-mode e) 'choose)) j) => cdr]
        [else '()]))

(struct auto ([edge-name : String]
              [prompt-records : (Listof Prompt-Record)])
  #:prefab #:type-name Journal-Entry-Auto)

(: auto* (-> String [#:prompt-records (Listof Prompt-Record)] Journal-Entry-Auto))
(define (auto* name #:prompt-records [prompt-records '()])
  (auto name prompt-records))

(struct choose ([edge-name : String]
                [edge-extra : Any]
                [prompt-records : (Listof Prompt-Record)])
  #:prefab #:type-name Journal-Entry-Choose)

(define-predicate auto*? Journal-Entry-Auto)

(: choose* (-> String [#:edge-extra Any] [#:prompt-records (Listof Prompt-Record)] Journal-Entry-Choose))
(define (choose* name #:edge-extra [extra #f] #:prompt-records [prompt-records '()])
  (choose name extra prompt-records))

(define-predicate choose*? Journal-Entry-Choose)

(define-type Journal-Entry (U Journal-Entry-Auto Journal-Entry-Choose))
(define-predicate journal-entry? Journal-Entry)

(: journal-entry-edge-name (-> Journal-Entry String))
(define (journal-entry-edge-name e)
  (cond [(auto*? e) (auto-edge-name e)]
        [(choose*? e) (choose-edge-name e)]))

(: journal-entry-edge-mode (-> Journal-Entry (U 'auto 'choose)))
(define (journal-entry-edge-mode e)
  (cond [(auto*? e) 'auto]
        [(choose*? e) 'choose]))

(: journal-entry-edge-extra (-> Journal-Entry Any))
(define (journal-entry-edge-extra e)
  (cond [(auto*? e) #f]
        [(choose*? e) (choose-edge-extra e)]))

(: journal-entry-prompt-records (-> Journal-Entry (Listof Prompt-Record)))
(define (journal-entry-prompt-records e)
  (cond [(auto*? e) (auto-prompt-records e)]
        [(choose*? e) (choose-prompt-records e)]))
