#lang typed/racket

(provide current-seen-ids
         Node node-maker
         node-graph-id node-id node-name node-type node-desc node-trans
         Condition make-condition
         condition-desc condition-proc
         Trans make-trans
         trans-desc trans-proc
         Bridge Edge make-bridge make-edge
         edge-name edge-mode edge-dom edge-cod edge-desc edge-when edge-trans edge-priority edge-weight
         Graph* OpenGraph Graph make-graph* make-open-graph make-graph
         graph-id graph-name graph-edges graph-bridges)

(: current-seen-ids (Parameterof (Setof Symbol)))
(define current-seen-ids (make-parameter ((inst set Symbol))))

(: make-graph-id (-> String Symbol))
(define (make-graph-id graph-name)
  (string->symbol (format "~a_~a" (string-length graph-name) graph-name)))

(: make-node-id (-> String String Symbol))
(define (make-node-id graph-name node-name)
  (string->symbol (format "~a_~a_~a_~a"
                          (string-length graph-name) graph-name
                          (string-length node-name) node-name)))

(: make-edge-id (All (T1 S1 T2 S2) (-> String (Node T1 S1) (Node T2 S2) Symbol)))
(define (make-edge-id edge-name dom cod)
  (let ([dom-id (symbol->string (node-id dom))]
        [cod-id (symbol->string (node-id cod))])
    (string->symbol (format "~a_~a_~a_~a_~a_~a"
                            (string-length dom-id) dom-id
                            (string-length cod-id) cod-id
                            (string-length edge-name) edge-name))))

(struct (T S) node ([graph-id : Symbol]
                    [id : Symbol]
                    [name : String]
                    [type : T]
                    [desc : (Option String)]
                    [trans : (Trans S S)])
  #:transparent
  #:type-name Node)

(: node-maker (All (T S)
                   (-> String
                       (-> String
                           #:type T
                           [#:desc (Option String)]
                           [#:trans (Option (Trans S S))]
                           (Node T S)))))
(define ((node-maker graph-name) name #:type type #:desc [desc #f] #:trans [tr #f])
  (let ([graph-id (make-graph-id graph-name)]
        [node-id (make-node-id graph-name name)])
    (cond [(set-member? (current-seen-ids) node-id)
           (error "node-maker: duplicate ID" node-id)]
          [else (current-seen-ids (set-add (current-seen-ids) node-id))])
    (node graph-id node-id name type desc (or tr (make-trans (inst identity S))))))

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

(struct (T1 S1 T2 S2) edge ([id : Symbol]
                            [name : String]
                            [mode : (U 'auto 'choose)]
                            [dom : (Node T1 S1)]
                            [cod : (Node T2 S2)]
                            [desc : (Option String)]
                            [when : (Condition S1)]
                            [trans : (Trans S1 S2)]
                            [priority : Integer]
                            [weight : Exact-Positive-Integer])
  #:type-name Bridge)

(define-type (Edge T S) (Bridge T S T S))

(: make-bridge (All (T1 S1 T2 S2)
                    (-> String
                        #:mode (U 'auto 'choose)
                        #:dom (Node T1 S1)
                        #:cod (Node T2 S2)
                        [#:desc (Option String)]
                        [#:when (Option (Condition S1))]
                        #:trans (Trans S1 S2)
                        [#:priority (Option Integer)]
                        [#:weight (Option Exact-Positive-Integer)]
                        (Bridge T1 S1 T2 S2))))
(define (make-bridge name
                     #:mode mode
                     #:dom dom
                     #:cod cod
                     #:desc [desc #f]
                     #:when [when #f]
                     #:trans tr
                     #:priority [priority #f]
                     #:weight [weight #f])
  (let ([edge-id (make-edge-id name dom cod)])
    (cond [(set-member? (current-seen-ids) edge-id)
           (error "node-maker: duplicate ID" edge-id)]
          [else (current-seen-ids (set-add (current-seen-ids) edge-id))])
    (edge edge-id
          name mode dom cod
          desc
          (or when (make-condition (const #t)))
          tr
          (or priority 1)
          (or weight 1))))

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
                      (Edge T S))))
(define (make-edge name
                   #:mode mode
                   #:dom dom
                   #:cod cod
                   #:desc [desc #f]
                   #:when [when #f]
                   #:trans [tr #f]
                   #:priority [priority #f]
                   #:weight [weight #f])
  ((inst make-bridge T S T S) name
                              #:mode mode
                              #:dom dom
                              #:cod cod
                              #:desc desc
                              #:when when
                              #:trans (or tr (make-trans (inst identity S)))
                              #:priority priority
                              #:weight weight))

(struct (T1 S1 T2 S2) graph ([id : Symbol]
                             [name : String]
                             [desc : (Option String)]
                             [edges : (Listof (Edge T1 S1))]
                             [bridges : (Listof (Bridge T1 S1 T2 S2))])
  #:type-name Graph*)

(define-type (Graph T S) (Graph* T S T S))
(define-type (OpenGraph T S) (Graph* T S Any Any))

(: make-graph* (All (T1 S1 T2 S2)
                    (-> String
                        [#:desc (Option String)]
                        [#:edges (Option (Listof (Edge T1 S1)))]
                        [#:bridges (Option (Listof (Bridge T1 S1 T2 S2)))]
                        (Graph* T1 S1 T2 S2))))
(define (make-graph* name
                     #:desc [desc #f]
                     #:edges [edges #f]
                     #:bridges [bridges #f])
  (let ([graph-id (make-graph-id name)])
    (cond [(set-member? (current-seen-ids) graph-id)
           (error "node-maker: duplicate ID" graph-id)]
          [else (current-seen-ids (set-add (current-seen-ids) graph-id))])
    (graph (make-graph-id name) name desc (or edges '()) (or bridges '()))))

(: make-open-graph (All (T S)
                        (-> String
                            [#:desc (Option String)]
                            [#:edges (Option (Listof (Edge T S)))]
                            [#:bridges (Option (Listof (Bridge T S Any Any)))]
                            (OpenGraph T S))))
(define (make-open-graph name
                         #:desc [desc #f]
                         #:edges [edges #f]
                         #:bridges [bridges #f])
  ((inst make-graph* T S Any Any) name
                                  #:desc desc
                                  #:edges edges
                                  #:bridges bridges))

(: make-graph (All (T S)
                   (-> String
                       [#:desc (Option String)]
                       [#:edges (Option (Listof (Edge T S)))]
                       [#:bridges (Option (Listof (Bridge T S T S)))]
                       (Graph T S))))
(define (make-graph name
                    #:desc [desc #f]
                    #:edges [edges #f]
                    #:bridges [bridges #f])
  ((inst make-graph* T S T S) name
                              #:desc desc
                              #:edges edges
                              #:bridges bridges))
