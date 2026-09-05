#lang typed/racket

(require "graph.rkt")
(require "prompt.rkt")
(require "message.rkt")
(require "journal.rkt")

(provide Node-Record Edge-Record Auto-Edge-Record Choose-Edge-Record
         node-record auto-edge-record choose-edge-record
         History Record
         record-events record-node record-edge record-node-prompt record-choices record-attributes
         history->journal)

(define-type Event (U Message-Result Prompt-Result))
(define-type (Node-Record S) (List 'node (Listof Event) (Node S)))
(define-type (Edge-Record S) (U (Auto-Edge-Record S) (Choose-Edge-Record S)))
(define-type (Auto-Edge-Record S) (List 'auto (Listof Event) (Edge S)))
(define-type (Choose-Edge-Record S) (List 'choose (Listof Event) (Edge S) String (Pairof (Edge S) (Listof (Edge S))) Prompt-Attributes))

(: node-record (All (S) (-> (Listof Event) (Node S) (Node-Record S))))
(define (node-record es n)
  (list 'node es n))

(: auto-edge-record (All (S) (-> (Listof Event) (Edge S) (Auto-Edge-Record S))))
(define (auto-edge-record es e)
  (list 'auto es e))

(: choose-edge-record (All (S) (-> (Listof Event) (Edge S) String (Pairof (Edge S) (Listof (Edge S))) Prompt-Attributes (Choose-Edge-Record S))))
(define (choose-edge-record es e pmt edges attrs)
  (list 'choose es e pmt edges attrs))

(: record-events (All (S) (-> (U (Node-Record S) (Auto-Edge-Record S) (Choose-Edge-Record S)) (Listof Event))))
(define (record-events r) (second r))

(: record-node (All (S) (-> (Node-Record S) (Node S))))
(define (record-node r) (third r))

(: record-edge (All (S) (-> (U (Auto-Edge-Record S) (Choose-Edge-Record S)) (Edge S))))
(define (record-edge r) (third r))

(: record-node-prompt (All (S) (-> (Choose-Edge-Record S) String)))
(define (record-node-prompt r) (fourth r))

(: record-choices (All (S) (-> (Choose-Edge-Record S) (Pairof (Edge S) (Listof (Edge S))))))
(define (record-choices r) (fifth r))

(: record-attributes (All (S) (-> (Choose-Edge-Record S) Prompt-Attributes)))
(define (record-attributes r) (sixth r))

(define-type (Record S) (U (Node-Record S)
                           (Auto-Edge-Record S)
                           (Choose-Edge-Record S)))

(define-type (History S) (Listof (Record S)))

(: history->journal (All (S) (-> (History S) (Listof Journal-Entry))))
(define (history->journal h)
  (: prompt-values (-> (Listof (U Prompt-Result Message-Result))
                       (Listof (Pairof Prompt-Value Prompt-Attributes))))
  (define (prompt-values xs)
    (filter-map (lambda ([x : (U Prompt-Result Message-Result)])
                  (case (first x)
                    [(prompt) (fourth x)]
                    [(message) #f]))
                xs))
  (if (null? h)
      '()
      (let ([hn (car h)]
            [he (if (null? (cdr h))
                    (error 'history->journal "invalid history")
                    (cadr h))])
        (if (and (symbol=? (car hn) 'node)
                 (or (symbol=? (car he) 'auto)
                     (symbol=? (car he) 'choose)))
            (let ([attrs (if (symbol=? (car he) 'choose)
                             (record-attributes he)
                             '())])
              (cons (journal-entry (car he) (edge-name (record-edge he))
                                   #:edge-attributes attrs
                                   #:prompt-records (append (prompt-values (record-events hn))
                                                            (prompt-values (record-events he))))
                    (history->journal (cddr h))))
            (error 'history->journal "invalid history")))))
