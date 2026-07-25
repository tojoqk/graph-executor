#lang typed/racket

(require "../graph.rkt")
(require racket/hash)

(provide dot-bridge dot-edge edge-dot-minlen)

(: dot-bridge (All (T S)
                   (-> String
                       [#:mode (Option EdgeMode)]
                       [#:half? Boolean]
                       #:from (Node T S)
                       #:to (Node Any Any)
                       [#:desc (Option String)]
                       [#:when (Option (Code (-> S Any)))]
                       #:trans (Code (-> S Any))
                       [#:priority (Option Integer)]
                       [#:weight (Option Exact-Positive-Integer)]
                       [#:dot-minlen (Option Natural)]
                       [#:attributes (Immutable-HashTable Symbol Any)]
                       (Bridge T S))))
(define (dot-bridge name
                    #:mode [mode #f]
                    #:half? [half? #f]
                    #:from from
                    #:to to
                    #:desc [desc #f]
                    #:when [when #f]
                    #:trans tr
                    #:priority [priority #f]
                    #:weight [weight #f]
                    #:dot-minlen [dot-minlen #f]
                    #:attributes [attrs ((inst hash Symbol Any))])
  ((inst make-bridge T S) #:name name
                          #:mode mode
                          #:half? half?
                          #:from from
                          #:to to
                          #:desc desc
                          #:when when
                          #:trans (or tr (inst identity S))
                          #:priority priority
                          #:weight weight
                          #:attributes (hash-union attrs
                                                   (hash 'dot-minlen dot-minlen))))

(: dot-edge (All (T S)
                 (-> String
                     [#:mode (Option EdgeMode)]
                     [#:half? Boolean]
                     #:from (Node T S)
                     #:to (Node T S)
                     [#:desc (Option String)]
                     [#:when (Option (Code (-> S Any)))]
                     [#:trans (Option (Code (-> S S)))]
                     [#:priority (Option Integer)]
                     [#:weight (Option Exact-Positive-Integer)]
                     [#:dot-minlen (Option Natural)]
                     [#:attributes (Immutable-HashTable Symbol Any)]
                     (Edge T S))))
(define (dot-edge name
                  #:mode [mode #f]
                  #:half? [half? #f]
                  #:from from
                  #:to to
                  #:desc [desc #f]
                  #:when [when #f]
                  #:trans [tr #f]
                  #:priority [priority #f]
                  #:weight [weight #f]
                  #:dot-minlen [dot-minlen #f]
                  #:attributes [attrs ((inst hash Symbol Any))])
  ((inst make-edge T S) #:name name
                        #:mode mode
                        #:half? half?
                        #:from from
                        #:to to
                        #:desc desc
                        #:when when
                        #:trans (or tr (make-code #f (inst identity S)))
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
