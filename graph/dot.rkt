#lang typed/racket

(require "../graph.rkt")
(require racket/hash)

(provide make-dot-bridge make-dot-edge edge-dot-minlen)

(: make-dot-bridge (All (T1 S1 T2 S2)
                        (-> String
                            [#:mode (Option EdgeMode)]
                            #:dom (Node T1 S1)
                            #:cod (Node T2 S2)
                            [#:desc (Option String)]
                            [#:when (Option (-> S1 Any))]
                            #:trans (-> S1 S2)
                            [#:priority (Option Integer)]
                            [#:weight (Option Exact-Positive-Integer)]
                            [#:dot-minlen (Option Natural)]
                            [#:attributes (Immutable-HashTable Symbol Any)]
                            (Bridge T1 S1 T2 S2))))
(define (make-dot-bridge name
                     #:mode [mode #f]
                     #:dom dom
                     #:cod cod
                     #:desc [desc #f]
                     #:when [when #f]
                     #:trans tr
                     #:priority [priority #f]
                     #:weight [weight #f]
                     #:dot-minlen [dot-minlen #f]
                     #:attributes [attrs ((inst hash Symbol Any))])
  ((inst make-bridge T1 S1 T2 S2) name
                                  #:mode mode
                                  #:dom dom
                                  #:cod cod
                                  #:desc desc
                                  #:when when
                                  #:trans (or tr (inst identity S))
                                  #:priority priority
                                  #:weight weight
                                  #:attributes (hash-union attrs
                                                           (hash 'dot-minlen dot-minlen))))

(: make-dot-edge (All (T S)
                      (-> String
                          [#:mode (Option EdgeMode)]
                          #:dom (Node T S)
                          #:cod (Node T S)
                          [#:desc (Option String)]
                          [#:when (Option (-> S Any))]
                          [#:trans (Option (-> S S))]
                          [#:priority (Option Integer)]
                          [#:weight (Option Exact-Positive-Integer)]
                          [#:dot-minlen (Option Natural)]
                          [#:attributes (Immutable-HashTable Symbol Any)]
                          (Edge T S))))
(define (make-dot-edge name
                   #:mode [mode #f]
                   #:dom dom
                   #:cod cod
                   #:desc [desc #f]
                   #:when [when #f]
                   #:trans [tr #f]
                   #:priority [priority #f]
                   #:weight [weight #f]
                   #:dot-minlen [dot-minlen #f]
                   #:attributes [attrs ((inst hash Symbol Any))])
  ((inst make-dot-bridge T S T S) name
                              #:mode mode
                              #:dom dom
                              #:cod cod
                              #:desc desc
                              #:when when
                              #:trans (or tr (inst identity S))
                              #:priority priority
                              #:weight weight
                              #:dot-minlen dot-minlen
                              #:attributes attrs))

(: edge-dot-minlen (All (T S) (-> (Edge T S) Natural)))
(define (edge-dot-minlen e)
  (cond [(hash-ref (edge-attributes e) 'dot-minlen #f)
         => (lambda (x)
              (if (natural? x)
                  x
                  1))]
        [else 1]))
