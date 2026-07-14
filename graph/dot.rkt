#lang typed/racket

(require "../graph.rkt")
(require racket/hash)

(provide make-dot-bridge make-dot-edge edge-dot-minlen)

(: make-dot-bridge (All (T S)
                        (-> String
                            [#:mode (Option EdgeMode)]
                            [#:half? Boolean]
                            #:dom (Node T S)
                            #:cod (Node Any Any)
                            [#:desc (Option String)]
                            [#:when (Option (-> S Any))]
                            #:trans (-> S Any)
                            [#:priority (Option Integer)]
                            [#:weight (Option Exact-Positive-Integer)]
                            [#:dot-minlen (Option Natural)]
                            [#:attributes (Immutable-HashTable Symbol Any)]
                            (Bridge T S))))
(define (make-dot-bridge name
                         #:mode [mode #f]
                         #:half? [half? #f]
                         #:dom dom
                         #:cod cod
                         #:desc [desc #f]
                         #:when [when #f]
                         #:trans tr
                         #:priority [priority #f]
                         #:weight [weight #f]
                         #:dot-minlen [dot-minlen #f]
                         #:attributes [attrs ((inst hash Symbol Any))])
  ((inst make-bridge* T S) #:name name
                           #:mode mode
                           #:half? half?
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
                          [#:half? Boolean]
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
                       #:half? [half? #f]
                       #:dom dom
                       #:cod cod
                       #:desc [desc #f]
                       #:when [when #f]
                       #:trans [tr #f]
                       #:priority [priority #f]
                       #:weight [weight #f]
                       #:dot-minlen [dot-minlen #f]
                       #:attributes [attrs ((inst hash Symbol Any))])
  ((inst make-edge* T S) #:name name
                         #:mode mode
                         #:half? half?
                         #:dom dom
                         #:cod cod
                         #:desc desc
                         #:when when
                         #:trans (or tr (inst identity S))
                         #:priority priority
                         #:weight weight
                         #:attributes (hash-union attrs
                                                    (hash 'dot-minlen dot-minlen))))

(: edge-dot-minlen (All (T S) (-> (Edge T S) Natural)))
(define (edge-dot-minlen e)
  (cond [(hash-ref (edge-attributes e) 'dot-minlen #f)
         => (lambda (x)
              (if (natural? x)
                  x
                  1))]
        [else 1]))
