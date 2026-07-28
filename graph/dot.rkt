#lang typed/racket

(require "../graph.rkt")
(require racket/hash)

(provide dot-bridge dot-edge edge-dot-minlen)

(: dot-bridge (All (S)
                   (-> String
                       [#:mode (Option EdgeMode)]
                       [#:half? Boolean]
                       #:from (Node S)
                       #:to (Node Any)
                       [#:desc (Option String)]
                       [#:when (Option (Code (-> S Any)))]
                       #:trans (Code (-> S Any))
                       [#:priority (Option Integer)]
                       [#:weight (Option Exact-Positive-Integer)]
                       [#:dot-minlen (Option Natural)]
                       [#:attributes (Immutable-HashTable Symbol Any)]
                       (Bridge S))))
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
  ((inst make-bridge S) #:name name
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

(: dot-edge (All (S)
                 (-> String
                     [#:mode (Option EdgeMode)]
                     [#:half? Boolean]
                     #:from (Node S)
                     #:to (Node S)
                     [#:desc (Option String)]
                     [#:when (Option (Code (-> S Any)))]
                     [#:trans (Option (Code (-> S S)))]
                     [#:priority (Option Integer)]
                     [#:weight (Option Exact-Positive-Integer)]
                     [#:dot-minlen (Option Natural)]
                     [#:attributes (Immutable-HashTable Symbol Any)]
                     (Edge S))))
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
  ((inst make-edge S) #:name name
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

(: edge-dot-minlen (All (S) (-> (Edge S) Natural)))
(define (edge-dot-minlen e)
  (cond [(hash-ref (edge-attributes e) 'dot-minlen #f)
         => (lambda (x)
              (if (natural? x)
                  x
                  1))]
        [else 1]))
