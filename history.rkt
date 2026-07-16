#lang typed/racket

(require "graph.rkt")
(require "prompt.rkt")
(require "message.rkt")
(require "journal.rkt")

(provide (except-out (struct-out history-item) history-item?)
         History-Node (except-out (struct-out history-node) history-node?)
         History-Edge (except-out (struct-out history-edge) history-edge?)
         History-Auto (except-out (struct-out history-auto) history-auto?)
         History-Choose (except-out (struct-out history-choose) history-choose?)
         History History-Record history-record-node history-record-type
         history->journal)

(struct (T S) history-item ([events : (Listof (U Message-Info Prompt-Info))])
  #:transparent
  #:type-name History-Item)

(struct (T S) history-node history-item ([node : (Node T S)])
  #:type-name History-Node
  #:transparent)
(struct (T S) history-edge history-item ([edge : (Edge T S)])
  #:type-name History-Edge
  #:transparent)
(struct (T S) history-auto history-edge ()
  #:type-name History-Auto
  #:transparent)
(struct (T S) history-choose history-edge ([prompt : String]
                                           [items : (Pairof (Edge T S) (Listof (Edge T S)))]
                                           [attributes : Prompt-Attributes])
  #:type-name History-Choose
  #:transparent)

(define-type (History-Record T S) (U (Pairof 'node (History-Node T S))
                                     (Pairof 'auto (History-Auto T S))
                                     (Pairof 'choose (History-Choose T S))))

(define-type (History T S) (Listof (History-Record T S)))

(: history-record-node (All (T S) (-> (History-Record T S) (Node T S))))
(define (history-record-node rec)
  (case (car rec)
    [(node) (history-node-node (cdr rec))]
    [(auto choose) (edge-dom (history-edge-edge (cdr rec)))]))

(: history-record-type (All (T S) (-> (History-Record T S) T)))
(define (history-record-type rec)
  (node-type (history-record-node rec)))

(: history->journal (All (T S) (-> (History T S) Journal)))
(define (history->journal h)
  (: prompt-values (-> (Listof (U Prompt-Info Message-Info))
                       (Listof (Pairof Prompt-Value Prompt-Attributes))))
  (define (prompt-values xs)
    (filter-map (lambda ([x : (U Prompt-Info Message-Info)])
                  (if (prompt-info? x)
                      `(,(prompt-info-value x) ,@(prompt-info-attributes x))
                      #f))
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
                             (history-choose-attributes (cdr he))
                             '())])
              (cons `((,(edge-name (history-edge-edge (cdr he))) ,@attrs)
                      ,@(append (prompt-values (history-item-events (cdr hn)))
                                (prompt-values (history-item-events (cdr he)))))
                    (history->journal (cddr h))))
            (error 'history->journal "invalid history")))))
