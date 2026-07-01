#lang typed/racket

(provide current-seen-ids current-node-prompt
         Node node-maker* node-maker
         node-graph-id node-graph-name node-id node-name node-type node-desc node-trans node-prompt node-attributes
         Bridge Edge EdgeMode make-bridge* make-bridge make-edge* make-edge
         edge-id edge-name edge-mode edge-dom edge-cod edge-desc edge-when edge-trans edge-priority edge-weight edge-attributes
         OpenGraph Graph make-open-graph make-graph
         graph-id graph-name graph-parent-id graph-parent-name graph-desc graph-edges graph-bridges)

(: current-seen-ids (Parameterof (Setof Symbol)))
(define current-seen-ids (make-parameter ((inst set Symbol))))

(: current-node-prompt (Parameterof String))
(define current-node-prompt (make-parameter "Choose:"))

(: make-graph-id (-> String Symbol))
(define (make-graph-id graph-name)
  (string->symbol (format "[~a]~a" (string-length graph-name) graph-name)))

(: make-node-id (-> String String Symbol))
(define (make-node-id graph-name node-name)
  (string->symbol (format "[~a]~a[~a]~a"
                          (string-length graph-name) graph-name
                          (string-length node-name) node-name)))

(: make-edge-id (All (T S) (-> String (Node T S) Symbol)))
(define (make-edge-id edge-name dom)
  (let ([dom-id (symbol->string (node-id dom))])
    (string->symbol (format "~a[~a]~a"
                            dom-id
                            (string-length edge-name)
                            edge-name))))

(struct (T S) node ([graph-id : Symbol]
                    [graph-name : String]
                    [id : Symbol]
                    [name : String]
                    [type : T]
                    [desc : (Option String)]
                    [trans : (-> S S)]
                    [prompt : String]
                    [attributes : (Immutable-HashTable Symbol Any)])
  #:transparent
  #:type-name Node)

(: node-maker* (All (T S)
                    (-> String
                        (-> String
                            #:type T
                            [#:desc (Option String)]
                            [#:trans (Option (-> S S))]
                            [#:prompt (Option String)]
                            [#:attributes (Immutable-HashTable Symbol Any)]
                            (Node T S)))))
(define ((node-maker* graph-name) name #:type type #:desc [desc #f] #:trans [tr #f] #:prompt [pmt #f] #:attributes [attrs ((inst hash Symbol Any))])
  (let ([graph-id (make-graph-id graph-name)]
        [node-id (make-node-id graph-name name)])
    (cond [(set-member? (current-seen-ids) node-id)
           (error "node-maker*: duplicate ID" node-id)]
          [else (current-seen-ids (set-add (current-seen-ids) node-id))])
    (node graph-id graph-name node-id name type desc (or tr (inst identity S)) (or pmt (current-node-prompt)) attrs)))

(: node-maker (All (T S)
                   (-> String
                       (-> String
                           #:type T
                           [#:desc (Option String)]
                           [#:trans (Option (-> S S))]
                           [#:prompt (Option String)]
                           (Node T S)))))
(define ((node-maker graph-name) name #:type type #:desc [desc #f] #:trans [tr #f] #:prompt [pmt #f])
  (((inst node-maker* T S) graph-name) name #:type type #:desc desc #:trans tr #:prompt pmt #:attributes ((inst hash Symbol Any))))
(define-type EdgeMode (U 'auto 'choose 'annotation))

(struct (T1 S1 T2 S2) edge ([id : Symbol]
                            [name : String]
                            [mode : EdgeMode]
                            [dom : (Node T1 S1)]
                            [cod : (Node T2 S2)]
                            [desc : (Option String)]
                            [when : (-> S1 Any)]
                            [trans : (-> S1 S2)]
                            [priority : Integer]
                            [weight : Exact-Positive-Integer]
                            [attributes : (Immutable-HashTable Symbol Any)])
  #:transparent
  #:type-name Bridge)

(define-type (Edge T S) (Bridge T S T S))

(: make-bridge* (All (T1 S1 T2 S2)
                     (-> String
                         [#:mode (Option EdgeMode)]
                         #:dom (Node T1 S1)
                         #:cod (Node T2 S2)
                         [#:desc (Option String)]
                         [#:when (Option (-> S1 Any))]
                         #:trans (-> S1 S2)
                         [#:priority (Option Integer)]
                         [#:weight (Option Exact-Positive-Integer)]
                         [#:attributes (Immutable-HashTable Symbol Any)]
                         (Bridge T1 S1 T2 S2))))
(define (make-bridge* name
                      #:mode [mode #f]
                      #:dom dom
                      #:cod cod
                      #:desc [desc #f]
                      #:when [when #f]
                      #:trans tr
                      #:priority [priority #f]
                      #:weight [weight #f]
                      #:attributes [attrs ((inst hash Symbol Any))])
  (let ([edge-id (make-edge-id name dom)])
    (cond [(set-member? (current-seen-ids) edge-id)
           (error "make-edge, make-bridge*: duplicate ID" edge-id)]
          [else (current-seen-ids (set-add (current-seen-ids) edge-id))])
    (edge edge-id
          name (or mode 'choose) dom cod
          desc
          (or when (const #t))
          tr
          (or priority 0)
          (or weight 1)
          attrs)))

(: make-bridge (All (T1 S1 T2 S2)
                    (-> String
                        [#:mode (Option EdgeMode)]
                        #:dom (Node T1 S1)
                        #:cod (Node T2 S2)
                        [#:desc (Option String)]
                        [#:when (Option (-> S1 Any))]
                        #:trans (-> S1 S2)
                        [#:priority (Option Integer)]
                        [#:weight (Option Exact-Positive-Integer)]
                        (Bridge T1 S1 T2 S2))))
(define (make-bridge name
                     #:mode [mode #f]
                     #:dom dom
                     #:cod cod
                     #:desc [desc #f]
                     #:when [when #f]
                     #:trans tr
                     #:priority [priority #f]
                     #:weight [weight #f])
  ((inst make-bridge* T1 S1 T2 S2) name
                                   #:mode mode
                                   #:dom dom
                                   #:cod cod
                                   #:desc desc
                                   #:when when
                                   #:trans (or tr (inst identity S))
                                   #:priority priority
                                   #:weight weight
                                   #:attributes ((inst hash Symbol Any))))

(: make-edge* (All (T S)
                   (-> String
                       [#:mode (Option EdgeMode)]
                       #:dom (Node T S)
                       #:cod (Node T S)
                       [#:desc (Option String)]
                       [#:when (Option (-> S Any))]
                       [#:trans (Option (-> S S))]
                       [#:priority (Option Integer)]
                       [#:weight (Option Exact-Positive-Integer)]
                       [#:attributes (Immutable-HashTable Symbol Any)]
                       (Edge T S))))
(define (make-edge* name
                    #:mode [mode #f]
                    #:dom dom
                    #:cod cod
                    #:desc [desc #f]
                    #:when [when #f]
                    #:trans [tr #f]
                    #:priority [priority #f]
                    #:weight [weight #f]
                    #:attributes [attrs ((inst hash Symbol Any))])
  ((inst make-bridge* T S T S) name
                               #:mode mode
                               #:dom dom
                               #:cod cod
                               #:desc desc
                               #:when when
                               #:trans (or tr (inst identity S))
                               #:priority priority
                               #:weight weight
                               #:attributes attrs))

(: make-edge (All (T S)
                  (-> String
                      [#:mode (Option EdgeMode)]
                      #:dom (Node T S)
                      #:cod (Node T S)
                      [#:desc (Option String)]
                      [#:when (Option (-> S Any))]
                      [#:trans (Option (-> S S))]
                      [#:priority (Option Integer)]
                      [#:weight (Option Exact-Positive-Integer)]
                      (Edge T S))))
(define (make-edge name
                   #:mode [mode #f]
                   #:dom dom
                   #:cod cod
                   #:desc [desc #f]
                   #:when [when #f]
                   #:trans [tr #f]
                   #:priority [priority #f]
                   #:weight [weight #f])
  ((inst make-edge* T S) name
                         #:mode mode
                         #:dom dom
                         #:cod cod
                         #:desc desc
                         #:when when
                         #:trans (or tr (inst identity S))
                         #:priority priority
                         #:weight weight
                         #:attributes ((inst hash Symbol Any))))

(struct (T1 S1 T2 S2) graph ([id : Symbol]
                             [name : String]
                             [parent-id : (Option Symbol)]
                             [parent-name : (Option String)]
                             [desc : (Option String)]
                             [edges : (Listof (Edge T1 S1))]
                             [bridges : (Listof (Bridge T1 S1 T2 S2))])
  #:transparent
  #:type-name OpenGraph)

(define-type (Graph T S) (OpenGraph T S T S))

(: make-open-graph (All (T1 S1 T2 S2)
                        (-> String
                            [#:parent-name (Option String)]
                            [#:desc (Option String)]
                            [#:edges (Option (Listof (Edge T1 S1)))]
                            [#:bridges (Option (Listof (Bridge T1 S1 T2 S2)))]
                            (OpenGraph T1 S1 T2 S2))))
(define (make-open-graph name
                         #:parent-name [parent-name #f]
                         #:desc [desc #f]
                         #:edges [edges #f]
                         #:bridges [bridges #f])
  (let ([graph-id (make-graph-id name)])
    (cond [(set-member? (current-seen-ids) graph-id)
           (error "make-graph*: duplicate ID" graph-id)]
          [else (current-seen-ids (set-add (current-seen-ids) graph-id))])
    (graph (make-graph-id name) name
           (and parent-name (make-graph-id parent-name)) parent-name
           desc (or edges '()) (or bridges '()))))

(: make-graph (All (T S)
                   (-> String
                       [#:parent-name (Option String)]
                       [#:desc (Option String)]
                       [#:edges (Option (Listof (Edge T S)))]
                       [#:bridges (Option (Listof (Bridge T S T S)))]
                       (Graph T S))))
(define (make-graph name
                    #:parent-name [parent-name #f]
                    #:desc [desc #f]
                    #:edges [edges #f]
                    #:bridges [bridges #f])
  ((inst make-open-graph T S T S) name
                                  #:parent-name parent-name
                                  #:desc desc
                                  #:edges edges
                                  #:bridges bridges))
