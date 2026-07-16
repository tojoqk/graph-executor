#lang typed/racket

(require "graph.rkt")
(require "prompt.rkt")
(require "message.rkt")
(require "history.rkt")
(require "journal.rkt")

(provide Event-Logger make-event-logger event-logger-prompt-log! event-logger-message-log!
         event-logger->history-edge event-logger->history-node
         event-logger->journal-entry)

(define-type (Event-Logger T S)
  (List (U (List 'auto (Edge T S))
           (List 'choose (Edge T S)
                 String
                 (Pairof (Edge T S) (Listof (Edge T S)))
                 Prompt-Attributes))
        (Boxof (Listof (U Message-Info Prompt-Info)))
        (Node T S)
        (Boxof (Listof (U Message-Info Prompt-Info)))))

(: make-event-logger (All (T S)
                          (case->
                           (-> (Edge T S) (Node T S) (Event-Logger T S))
                           (-> (Edge T S)
                               String
                               (Pairof (Edge T S) (Listof (Edge T S)))
                               Prompt-Attributes
                               (Node T S)
                               (Event-Logger T S)))))
(define make-event-logger
  (case-lambda
    [(e n)
     (list (list 'auto e) (box '()) n (box '()))]
    [(e pmt edges attrs n)
     (list (list 'choose e pmt edges attrs)
           (box '())
           n
           (box '()))]))

(: event-logger-prompt-log! (All (T S)
                                 (-> (Event-Logger T S)
                                     (U 'node 'edge)
                                     Prompt-Info
                                     Void)))
(define (event-logger-prompt-log! logger type val)
  (let ([bx (case type
              [(edge) (second logger)]
              [(node) (fourth logger)])])
    (set-box! bx (cons val (unbox bx)))))

(: event-logger-message-log! (All (T S)
                                 (-> (Event-Logger T S)
                                     (U 'node 'edge)
                                     Any
                                     Void)))
(define (event-logger-message-log! logger type val)
  (let ([bx (case type
              [(edge) (second logger)]
              [(node) (fourth logger)])])
    (set-box! bx (cons (message-info val) (unbox bx)))))

(: event-logger->history-edge (All (T S) (-> (Event-Logger T S)
                                             (U (Pairof 'auto (History-Auto T S))
                                                (Pairof 'choose (History-Choose T S))))))
(define (event-logger->history-edge logger)
  (case (caar logger)
    [(auto)
     (let ([e (second (car logger))]
           [bx (second logger)])
       (cons 'auto (history-auto (unbox bx) e)))]
    [(choose)
     (let ([e (second (car logger))]
           [pmt (third (car logger))]
           [items (fourth (car logger))]
           [attrs (fifth (car logger))]
           [bx (second logger)])
       (cons 'choose (history-choose (unbox bx) e pmt items attrs)))]))

(: event-logger->history-node (All (T S) (-> (Event-Logger T S)
                                             (Pairof 'node (History-Node T S)))))
(define (event-logger->history-node logger)
  (let ([n (third logger)]
        [bx (fourth logger)])
    (cons 'node (history-node (unbox bx) n))))

(: event-logger->journal-entry (All (T S) (-> (Event-Logger T S) Journal-Entry)))
(define (event-logger->journal-entry logger)
  (let ([e (second (first logger))]
        [e-bx (second logger)]
        [n-bx (fourth logger)])
    (: prompt-values (-> (Listof (U Prompt-Info Message-Info))
                         (Listof (Pairof Prompt-Value Prompt-Attributes))))
    (define (prompt-values xs)
      (filter-map (lambda ([x : (U Prompt-Info Message-Info)])
                    (if (prompt-info? x)
                        `(,(prompt-info-value x) ,@(prompt-info-attributes x))
                        #f))
                  xs))
    (case (caar logger)
      [(auto)
       `(auto (,(edge-name e))
              ,@(append (prompt-values (unbox n-bx))
                        (prompt-values (unbox e-bx))))]
      [(choose)
       (let ([attrs (fifth (car logger))])
         `(choose (,(edge-name e) ,@attrs)
                  ,@(append (prompt-values (unbox n-bx))
                            (prompt-values (unbox e-bx)))))])))
