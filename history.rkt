#lang typed/racket

(require "graph.rkt")
(require "prompt.rkt")
(require "message.rkt")
(require "journal.rkt")

(provide History-Record-Node History-Record-Edge History-Record-Auto History-Record-Choose
         History History-Record
         history-record-events history-record-node
         history-record-edge history-record-title history-record-choices history-record-attributes
         history->journal
         History-Logger make-history-logger
         history-logger-prompt-log! history-logger-message-log!
         history-logger->history-record-edge history-logger->history-record-node)

(define-type History-Record-Event (U Message-Info Prompt-Info))
(define-type (History-Record-Node T S) (List 'node (Listof History-Record-Event) (Node T S)))
(define-type (History-Record-Edge T S) (U (History-Record-Auto T S) (History-Record-Choose T S)))
(define-type (History-Record-Auto T S) (List 'auto (Listof History-Record-Event) (Edge T S)))
(define-type (History-Record-Choose T S) (List 'choose (Listof History-Record-Event) (Edge T S) String (Pairof (Edge T S) (Listof (Edge T S))) Prompt-Attributes))

(: history-record-events (All (T S) (-> (U (History-Record-Node T S) (History-Record-Auto T S) (History-Record-Choose T S)) (Listof History-Record-Event))))
(define (history-record-events r) (second r))

(: history-record-node (All (T S) (-> (History-Record-Node T S) (Node T S))))
(define (history-record-node r) (third r))

(: history-record-edge (All (T S) (-> (U (History-Record-Auto T S) (History-Record-Choose T S)) (Edge T S))))
(define (history-record-edge r) (third r))

(: history-record-title (All (T S) (-> (History-Record-Choose T S) String)))
(define (history-record-title r) (fourth r))

(: history-record-choices (All (T S) (-> (History-Record-Choose T S) (Pairof (Edge T S) (Listof (Edge T S))))))
(define (history-record-choices r) (fifth r))

(: history-record-attributes (All (T S) (-> (History-Record-Choose T S) Prompt-Attributes)))
(define (history-record-attributes r) (sixth r))

(define-type (History-Record T S) (U (History-Record-Node T S)
                                     (History-Record-Auto T S)
                                     (History-Record-Choose T S)))

(define-type (History T S) (Listof (History-Record T S)))

(: history->journal (All (T S) (-> (History T S) Journal)))
(define (history->journal h)
  (: prompt-values (-> (Listof (U Prompt-Info Message-Info))
                       (Listof (Pairof Prompt-Value Prompt-Attributes))))
  (define (prompt-values xs)
    (filter-map (lambda ([x : (U Prompt-Info Message-Info)])
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
                             (history-record-attributes he)
                             '())])
              (cons `(,(car he) (,(edge-name (history-record-edge he)) ,@attrs)
                                ,@(append (prompt-values (history-record-events hn))
                                          (prompt-values (history-record-events he))))
                    (history->journal (cddr h))))
            (error 'history->journal "invalid history")))))

(define-type (History-Logger T S)
  (List (U (List 'auto (Edge T S))
           (List 'choose (Edge T S)
                 String
                 (Pairof (Edge T S) (Listof (Edge T S)))
                 Prompt-Attributes))
        (Boxof (Listof (U Message-Info Prompt-Info)))
        (Node T S)
        (Boxof (Listof (U Message-Info Prompt-Info)))))

(: make-history-logger (All (T S)
                            (case->
                             (-> 'auto (Edge T S) (Node T S) (History-Logger T S))
                             (-> 'choose
                                 (Edge T S)
                                 String
                                 (Pairof (Edge T S) (Listof (Edge T S)))
                                 Prompt-Attributes
                                 (Node T S)
                                 (History-Logger T S)))))
(define make-history-logger
  (case-lambda
    [(_ e n) (list (list 'auto e) (box '()) n (box '()))]
    [(_ e pmt edges attrs n) (list (list 'choose e pmt edges attrs) (box '()) n (box '()))]))

(: history-logger-prompt-log! (All (T S)
                                      (-> (History-Logger T S)
                                          (U 'node 'edge)
                                          String
                                          Prompt-Op
                                          Prompt-Value
                                          Prompt-Attributes
                                          Void)))
(define (history-logger-prompt-log! logger type title op val attrs)
  (let ([bx (case type
              [(edge) (second logger)]
              [(node) (fourth logger)])])
    (set-box! bx (cons `(prompt ,op ,title (,val ,@attrs)) (unbox bx)))))

(: history-logger-message-log! (All (T S)
                                    (-> (History-Logger T S)
                                        (U 'node 'edge)
                                        Any
                                        Void)))
(define (history-logger-message-log! logger type val)
  (let ([bx (case type
              [(edge) (second logger)]
              [(node) (fourth logger)])])
    (set-box! bx (cons (list 'message val) (unbox bx)))))

(: history-logger->history-record-edge (All (T S) (-> (History-Logger T S)
                                                      (History-Record-Edge T S))))
(define (history-logger->history-record-edge logger)
  (case (caar logger)
    [(auto)
     (let ([e (second (car logger))]
           [bx (second logger)])
       (list 'auto (unbox bx) e))]
    [(choose)
     (let ([e (second (car logger))]
           [pmt (third (car logger))]
           [items (fourth (car logger))]
           [attrs (fifth (car logger))]
           [bx (second logger)])
       (list 'choose (unbox bx) e pmt items attrs))]))

(: history-logger->history-record-node (All (T S) (-> (History-Logger T S) (History-Record-Node T S))))
(define (history-logger->history-record-node logger)
  (let ([n (third logger)]
        [bx (fourth logger)])
    (list 'node (unbox bx) n)))
