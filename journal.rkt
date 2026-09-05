#lang typed/racket

(provide Journal journal journal? Journal-Entry journal-entry? journal-undo
         (rename-out [journal-entry* journal-entry])
         journal-entry-edge-mode journal-entry-edge-name journal-entry-edge-attributes journal-entry-prompt-records)

(require "prompt.rkt")

(define-type Edge-Attributes (Listof (Pairof Symbol (U String Symbol Integer))))

(struct journal-entry ([edge-mode : (U 'auto 'choose)]
                       [edge-name : String]
                       [edge-attributes : Edge-Attributes]
                       [prompt-records : (Listof Prompt-Record)])
  #:prefab #:type-name Journal-Entry)

(: journal-entry* (-> (U 'auto 'choose) String [#:edge-attributes Prompt-Attributes] [#:prompt-records (Listof Prompt-Record)] Journal-Entry))
(define (journal-entry* mode name #:edge-attributes [attrs '()] #:prompt-records [prompt-records '()])
  (journal-entry mode name attrs prompt-records))

(define-type Journal (Listof Journal-Entry))
(define-predicate journal? Journal)

(: journal (-> Journal-Entry * Journal))
(define (journal . es) (reverse es))

(: journal-undo (-> Journal Journal))
(define (journal-undo j)
  (cond [(memf (lambda ([e : Journal-Entry]) (symbol=? (journal-entry-edge-mode e) 'choose)) j) => cdr]
        [else '()]))
