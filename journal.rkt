#lang typed/racket

(provide Journal journal journal? Journal-Entry journal-entry? journal-undo
         auto-journal-entry choose-journal-entry
         journal-entry-edge-mode journal-entry-edge-name journal-entry-edge-attributes journal-entry-prompt-records)

(require "prompt.rkt")

(define-type Journal-Entry (List* (U 'auto 'choose)
                                  (Pairof String Prompt-Attributes)
                                  (Listof (Pairof Prompt-Value Prompt-Attributes))))
(define-type Journal (Listof Journal-Entry))
(define-predicate journal? Journal)
(define-predicate journal-entry? Journal-Entry)

(: journal (-> Journal-Entry * Journal))
(define (journal . es)
  (reverse es))

(: journal-undo (-> Journal Journal))
(define (journal-undo j)
  (cond [(memf (lambda ([e : Journal-Entry]) (symbol=? (car e) 'choose)) j) => cdr]
        [else '()]))

(: auto-journal-entry (-> String [#:prompt-records (Listof Prompt-Record)] Journal-Entry))
(define (auto-journal-entry name #:prompt-records [prompt-records '()])
  `(auto (,name) ,@prompt-records))

(: choose-journal-entry (-> String [#:edge-attributes Prompt-Attributes] [#:prompt-records (Listof Prompt-Record)] Journal-Entry))
(define (choose-journal-entry name #:edge-attributes [attrs '()] #:prompt-records [prompt-records '()])
  `(choose (,name ,@attrs) ,@prompt-records))

(: journal-entry-edge-mode (-> Journal-Entry (U 'choose 'auto)))
(define (journal-entry-edge-mode x) (car x))

(: journal-entry-edge-name (-> Journal-Entry String))
(define (journal-entry-edge-name x) (caadr x))

(: journal-entry-edge-attributes (-> Journal-Entry Prompt-Attributes))
(define (journal-entry-edge-attributes x) (cdadr x))

(: journal-entry-prompt-records (-> Journal-Entry (Listof Prompt-Record)))
(define (journal-entry-prompt-records x) (cddr x))
