#lang typed/racket

(require "graph.rkt")
(require "prompt.rkt")
(require "message.rkt")

(provide Prompt-Info
         (rename-out [prompt-info?? prompt-info?])
         (except-out (struct-out prompt-info) prompt-info? make-prompt-info)
         Prompt-Info-Choose (struct-out prompt-info-choose)
         Prompt-Info-String (struct-out prompt-info-string)
         Prompt-Info-Integer (struct-out prompt-info-integer)
         Prompt-Info-Natural (struct-out prompt-info-natural)
         Prompt-Info-Positive (struct-out prompt-info-positive)
         Prompt-Info-Range prompt-info-range?
         prompt-info-range-value prompt-info-range-minimum prompt-info-range-maximum
         Prompt-Info-Range-Integer (struct-out prompt-info-range-integer)
         Prompt-Info-Range-Natural (struct-out prompt-info-range-natural)
         Prompt-Info-Range-Positive (struct-out prompt-info-range-positive)
         Prompt-Info-Random (struct-out prompt-info-random)
         Message-Info (struct-out message-info)

         (except-out (struct-out history-item) history-item?)
         History-Node (except-out (struct-out history-node) history-node?)
         History-Edge (except-out (struct-out history-edge) history-edge?)
         History-Auto (except-out (struct-out history-auto) history-auto?)
         History-Choose (except-out (struct-out history-choose) history-choose?)
         History)

(define-type Prompt-Info (U Prompt-Info-Choose
                            Prompt-Info-String
                            Prompt-Info-Integer
                            Prompt-Info-Natural
                            Prompt-Info-Positive
                            Prompt-Info-Range
                            Prompt-Info-Random))

(define-predicate prompt-info?? Prompt-Info)

(struct prompt-info ([title : String]
                     [attributes : Prompt-Attributes])
  #:constructor-name make-prompt-info
  #:transparent)
(struct prompt-info-choose prompt-info ([value : String]
                                        [items : (Listof (U (List String String) String))])
  #:type-name Prompt-Info-Choose
  #:transparent)
(struct prompt-info-string prompt-info ([value : String])
  #:type-name Prompt-Info-String
  #:transparent)
(struct prompt-info-integer prompt-info ([value : Integer])
  #:type-name Prompt-Info-Integer
  #:transparent)
(struct prompt-info-natural prompt-info ([value : Natural])
  #:type-name Prompt-Info-Natural
  #:transparent)
(struct prompt-info-positive prompt-info ([value : Positive-Integer])
  #:type-name Prompt-Info-Positive
  #:transparent)
(struct prompt-info-range-integer prompt-info ([value : Integer]
                                               [maximum : Integer]
                                               [minimum : Integer])
  #:type-name Prompt-Info-Range-Integer
  #:transparent)
(struct prompt-info-range-natural prompt-info ([value : Natural]
                                               [maximum : Natural]
                                               [minimum : Natural])
  #:type-name Prompt-Info-Range-Natural
  #:transparent)
(struct prompt-info-range-positive prompt-info ([value : Positive-Integer]
                                                [maximum : Positive-Integer]
                                                [minimum : Positive-Integer])
  #:type-name Prompt-Info-Range-Positive
  #:transparent)

(define-type Prompt-Info-Range (U Prompt-Info-Range-Integer
                                  Prompt-Info-Range-Natural
                                  Prompt-Info-Range-Positive))
(define-predicate prompt-info-range? Prompt-Info-Range)

(: prompt-info-range-value (-> Prompt-Info-Range Integer))
(define (prompt-info-range-value x)
  (cond [(prompt-info-range-integer? x) (prompt-info-range-integer-value x)]
        [(prompt-info-range-natural? x) (prompt-info-range-natural-value x)]
        [(prompt-info-range-positive? x) (prompt-info-range-positive-value x)]))

(: prompt-info-range-minimum (-> Prompt-Info-Range Integer))
(define (prompt-info-range-minimum x)
  (cond [(prompt-info-range-integer? x) (prompt-info-range-integer-minimum x)]
        [(prompt-info-range-natural? x) (prompt-info-range-natural-minimum x)]
        [(prompt-info-range-positive? x) (prompt-info-range-positive-minimum x)]))

(: prompt-info-range-maximum (-> Prompt-Info-Range Integer))
(define (prompt-info-range-maximum x)
  (cond [(prompt-info-range-integer? x) (prompt-info-range-integer-maximum x)]
        [(prompt-info-range-natural? x) (prompt-info-range-natural-maximum x)]
        [(prompt-info-range-positive? x) (prompt-info-range-positive-maximum x)]))

(struct prompt-info-random prompt-info ([value : Natural]
                                        [bound : Positive-Integer])
  #:type-name Prompt-Info-Random
  #:transparent)

(struct message-info ([message : Any])
  #:type-name Message-Info
  #:transparent)

(struct (T S) history-item ([graph : (Graph T S)]
                            [events : (Listof (U Message-Info Prompt-Info))])
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
(struct (T S) history-choose history-edge ([items : (Pairof (Edge T S) (Listof (Edge T S)))]
                                           [attributes : Prompt-Attributes])
  #:type-name History-Choose
  #:transparent)

(define-type (History T S) (Listof (U (Pairof 'node (History-Node T S))
                                      (Pairof 'auto (History-Auto T S))
                                      (Pairof 'choose (History-Choose T S)))))

(: history-edge-node (All (T S) (-> (History-Edge T S) (Node T S))))
(define (history-edge-node item)
  (edge-dom (history-edge-edge item)))
