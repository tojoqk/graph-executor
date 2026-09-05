#lang typed/racket

(require "graph.rkt")
(require "prompt.rkt")
(require "message.rkt")
(require "journal.rkt")

(provide Node-Record node-record node-record? node-record-node-id node-record-node-info node-record-events
         Edge-Record (rename-out [edge-record*? edge-record?]) edge-record edge-record-edge-id edge-record-edge-info edge-record-events
         Auto-Edge-Record auto-edge-record? auto-edge-record
         Choose-Edge-Record choose-edge-record? choose-edge-record choose-edge-record-prompt choose-edge-record-choices choose-edge-record-extra
         Trace Record
         trace->journal)

(define-type Event (U Message-Result Prompt-Result))
(struct node-record ([node-id : Symbol]
                     [node-info : Node-Info]
                     [events : (Listof Event)])
  #:transparent
  #:type-name Node-Record)
(struct edge-record ([edge-id : Symbol]
                     [edge-info : Edge-Info]
                     [events : (Listof Event)])
  #:transparent)
(struct auto-edge-record edge-record ()
  #:transparent
  #:type-name Auto-Edge-Record)
(struct choose-edge-record edge-record ([prompt : String]
                                        [choices : (Listof Edge-Info)]
                                        [extra : Any])
  #:transparent
  #:type-name Choose-Edge-Record)

(define-type Edge-Record (U Auto-Edge-Record Choose-Edge-Record))
(define-predicate edge-record*? Edge-Record)
(define-type Record (U Node-Record Edge-Record))
(define-type Trace (Listof Record))

(: trace->journal  (-> Trace (Listof Journal-Entry)))
[define (trace->journal t)
  (: prompt-values (-> (Listof (U Prompt-Result Message-Result))
                       (Listof (Pairof Prompt-Value Any))))
  (define (prompt-values xs)
    (filter-map (lambda ([x : (U Prompt-Result Message-Result)])
                  (case (first x)
                    [(prompt) (fourth x)]
                    [(message) #f]))
                xs))
  (if (null? t)
      '()
      (let ([tn (car t)]
            [te (if (null? (cdr t))
                    (error 'trace->journal "invalid trace")
                    (cadr t))])
        (if (and (node-record? tn) (or (auto-edge-record? te) (choose-edge-record? te)))
            (let ([mode (edge-info-mode (edge-record-edge-info te))])
              (case mode
                [(auto choose) (cons (journal-entry mode
                                                    (edge-info-name (edge-record-edge-info te))
                                                    #:edge-extra (and (choose-edge-record? te)
                                                                      (choose-edge-record-extra te))
                                                    #:prompt-records (append (prompt-values (node-record-events tn))
                                                                             (prompt-values (edge-record-events te))))
                                     (trace->journal (cddr t)))]
                [(annotation) (error 'trace->journal "invalid trace (unexpected edge mode: ~a" mode)]))
            (error 'trace->journal "invalid trace"))))]
