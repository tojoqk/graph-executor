#lang typed/racket

(provide Node node-maker
         node-graph-id node-id node-name node-type node-desc node-trans
         Condition make-condition
         condition-desc condition-proc
         Trans make-trans
         trans-desc trans-proc
         Edge make-edge make-bridge
         edge-name edge-mode edge-dom edge-cod edge-desc edge-when edge-trans edge-priority edge-weight
         Graph make-graph
         graph-id graph-name graph-edges graph-bridges)

(struct (T S) node ([graph-id : Symbol]
                    [id : Symbol]
                    [name : String]
                    [type : T]
                    [desc : (Option String)]
                    [trans : (Trans S S)])
  #:transparent
  #:type-name Node)

(: node-maker (All (T S)
                   (-> Symbol
                       (-> String
                           #:type T
                           [#:desc (Option String)]
                           [#:trans (Option (Trans S S))]
                           (Node T S)))))
(define ((node-maker g) name #:type type #:desc [desc #f] #:trans [tr #f])
  (node g (gensym) name type desc (or tr (make-trans (inst identity S)))))

(struct (S) condition ([proc : (-> S Any)]
                       [desc : (Option String)])
  #:type-name Condition)

(: make-condition (All (S) (-> (-> S Any) [#:desc (Option String)]
                               (Condition S))))
(define (make-condition proc #:desc [desc #f])
  (condition proc desc))

(struct (S1 S2) trans ([proc : (-> S1 S2)]
                       [desc : (Option String)])
  #:type-name Trans)

(: make-trans (All (S1 S2) (-> (-> S1 S2) [#:desc (Option String)]
                               (Trans S1 S2))))
(define (make-trans proc #:desc [desc #f])
  (trans proc desc))

(struct (T1 S1 T2 S2) edge ([name : String]
                            [mode : (U 'auto 'choose)]
                            [dom : (Node T1 S1)]
                            [cod : (Node T2 S2)]
                            [desc : (Option String)]
                            [when : (Condition S1)]
                            [trans : (Trans S1 S2)]
                            [priority : Integer]
                            [weight : Exact-Positive-Integer])
  #:type-name Edge)

(: make-edge* (All (T1 S1 T2 S2)
                   (-> String
                       #:mode (U 'auto 'choose)
                       #:dom (Node T1 S1)
                       #:cod (Node T2 S2)
                       [#:desc (Option String)]
                       [#:when (Option (Condition S1))]
                       #:trans (Trans S1 S2)
                       [#:priority (Option Integer)]
                       [#:weight (Option Exact-Positive-Integer)]
                       (Edge T1 S1 T2 S2))))
(define (make-edge* name
                    #:mode mode
                    #:dom dom
                    #:cod cod
                    #:desc [desc #f]
                    #:when [when #f]
                    #:trans tr
                    #:priority [priority #f]
                    #:weight [weight #f])
  (edge name mode dom cod
        desc
        (or when (make-condition (const #t)))
        tr
        (or priority 1)
        (or weight 1)))

(: make-edge (All (T S)
                  (-> String
                      #:mode (U 'auto 'choose)
                      #:dom (Node T S)
                      #:cod (Node T S)
                      [#:desc (Option String)]
                      [#:when (Option (Condition S))]
                      [#:trans (Option (Trans S S))]
                      [#:priority (Option Integer)]
                      [#:weight (Option Exact-Positive-Integer)]
                      (Edge T S T S))))
(define (make-edge name
                   #:mode mode
                   #:dom dom
                   #:cod cod
                   #:desc [desc #f]
                   #:when [when #f]
                   #:trans [tr #f]
                   #:priority [priority #f]
                   #:weight [weight #f])
  ((inst make-edge* T S T S) name
                             #:mode mode
                             #:dom dom
                             #:cod cod
                             #:desc desc
                             #:when when
                             #:trans (or tr (make-trans (inst identity S)))
                             #:priority priority
                             #:weight weight))

(: make-bridge (All (T S)
                    (-> String
                        #:mode (U 'auto 'choose)
                        #:dom (Node T S)
                        #:cod (Node Any Any)
                        [#:desc (Option String)]
                        [#:when (Option (Condition S))]
                        #:trans (Trans S Any)
                        [#:priority (Option Integer)]
                        [#:weight (Option Exact-Positive-Integer)]
                        (Edge T S Any Any))))
(define (make-bridge name
                   #:mode mode
                   #:dom dom
                   #:cod cod
                   #:desc [desc #f]
                   #:when [when #f]
                   #:trans tr
                   #:priority [priority #f]
                   #:weight [weight #f])
  ((inst make-edge* T S Any Any) name
                             #:mode mode
                             #:dom dom
                             #:cod cod
                             #:desc desc
                             #:when when
                             #:trans tr
                             #:priority priority
                             #:weight weight))

(struct (T S) graph ([id : Symbol]
                     [name : String]
                     [desc : (Option String)]
                     [edges : (Listof (Edge T S T S))]
                     [bridges : (Listof (Edge T S Any Any))])
  #:type-name Graph)

(: make-graph (All (T S)
                   (-> Symbol
                       String
                       [#:desc (Option String)]
                       [#:edges (Option (Listof (Edge T S T S)))]
                       [#:bridges (Option (Listof (Edge T S Any Any)))]
                       (Graph T S))))
(define (make-graph sym
                    name
                    #:desc [desc #f]
                    #:edges [edges #f]
                    #:bridges [bridges #f])
  (graph sym name desc (or edges '()) (or bridges '())))
