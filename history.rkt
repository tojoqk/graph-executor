#lang typed/racket

(require "graph.rkt")
(require "prompt.rkt")
(require "message.rkt")

(provide (except-out (struct-out history-item) history-item?)
         History-Node (except-out (struct-out history-node) history-node?)
         History-Edge (except-out (struct-out history-edge) history-edge?)
         History-Auto (except-out (struct-out history-auto) history-auto?)
         History-Choose (except-out (struct-out history-choose) history-choose?)
         History History-Record)

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
(struct (T S) history-choose history-edge ([items : (Pairof (Edge T S) (Listof (Edge T S)))]
                                           [attributes : Prompt-Attributes])
  #:type-name History-Choose
  #:transparent)

(define-type (History-Record T S) (U (Pairof 'node (History-Node T S))
                                     (Pairof 'auto (History-Auto T S))
                                     (Pairof 'choose (History-Choose T S))))

(define-type (History T S) (Listof (History-Record T S)))

(: history-edge-node (All (T S) (-> (History-Edge T S) (Node T S))))
(define (history-edge-node item)
  (edge-dom (history-edge-edge item)))
