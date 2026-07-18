#lang typed/racket

(provide Code (rename-out [code* code]) make-code
         current-seen-ids current-node-prompt
         Node AnyNode make-node (rename-out [node* node])
         node-graph-id node-graph-name node-id node-name node-type node-desc node-trans node-trans-sexp node-prompt node-prompt-sexp node-attributes
         any-node
         Edge AnyEdge Bridge EdgeMode make-edge make-bridge (rename-out [edge* edge] [bridge* bridge])
         edge-id edge-name edge-mode edge-half? edge-dom edge-cod edge-desc edge-when edge-when-sexp edge-trans edge-trans-sexp edge-priority edge-weight edge-attributes
         any-bridge any-edge
         Graph AnyGraph OpenGraph (rename-out [graph* graph]) (rename-out [open-graph* open-graph])
         graph-id graph-name graph-parent-id graph-parent-name graph-desc graph-edges
         any-graph)

(struct (A) code ([sexp : Sexp]
                  [value : A])
  #:transparent
  #:constructor-name make-code
  #:type-name Code)

(define-syntax code*
  (syntax-rules ()
    [(_ expr) (make-code 'expr expr)]))

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
                    [trans-code : (Code (-> S S))]
                    [prompt-code : (Code (-> S String))]
                    [attributes : (Immutable-HashTable Symbol Any)])
  #:transparent
  #:type-name Node)

(: node-trans (All (T S) (-> (Node T S) (-> S S))))
(define (node-trans n)
  (code-value (node-trans-code n)))

(: node-trans-sexp (All (T S) (-> (Node T S) Sexp)))
(define (node-trans-sexp n)
  (code-sexp (node-trans-code n)))

(: node-prompt (All (T S) (-> (Node T S) (-> S String))))
(define (node-prompt n)
  (code-value (node-prompt-code n)))

(: node-prompt-sexp (All (T S) (-> (Node T S) Sexp)))
(define (node-prompt-sexp n)
  (code-sexp (node-prompt-code n)))

(define-type AnyNode (Node Any Any))

(: make-node (All (T S)
                  (-> #:graph-name String
                      #:name String
                      #:type T
                      #:desc (Option String)
                      #:trans (Option (Code (-> S S)))
                      #:prompt (Option (U String (Code (-> S String))))
                      #:attributes (Immutable-HashTable Symbol Any)
                      (Node T S))))
(define (make-node #:graph-name graph-name #:name name #:type type #:desc desc #:trans tr #:prompt pmt #:attributes attrs)
  (let ([graph-id (make-graph-id graph-name)]
        [node-id (make-node-id graph-name name)])
    (cond [(set-member? (current-seen-ids) node-id)
           (error "node: duplicate ID" node-id)]
          [else (current-seen-ids (set-add (current-seen-ids) node-id))])
    (node graph-id graph-name node-id name type desc
          (or tr (make-code #f identity))
          (cond [(not pmt) (make-code #f (const (current-node-prompt)))]
                [(string? pmt) (make-code pmt (const pmt))]
                [else pmt])
          attrs)))

(: node* (All (T S)
              (-> String
                  (-> String
                      #:type T
                      [#:desc (Option String)]
                      [#:trans (Option (Code (-> S S)))]
                      [#:prompt (Option (U String (Code (-> S String))))]
                      (Node T S)))))
(define ((node* graph-name) name #:type type #:desc [desc #f] #:trans [tr #f] #:prompt [pmt #f])
  ((inst make-node T S) #:graph-name graph-name #:name name #:type type #:desc desc
                        #:trans tr
                        #:prompt pmt
                        #:attributes ((inst hash Symbol Any))))

(: any-node (All (T S) (-> (-> Any Any : #:+ S) (-> (Node T S) AnyNode))))
(define ((any-node p?) n)
  (struct-copy node n
               [trans-code (make-code (node-trans-sexp n)
                                      (lambda ([x : Any]) ((node-trans n) (assert x p?))))]
               [prompt-code (make-code (node-prompt-sexp n)
                                       (lambda ([x : Any]) ((node-prompt n) (assert x p?))))]))

(define-type EdgeMode (U 'auto 'choose 'annotation))

(struct (T S) edge ([id : Symbol]
                    [name : String]
                    [mode : EdgeMode]
                    [half? : Boolean]
                    [dom : (Node T S)]
                    [cod : (Node T S)]
                    [desc : (Option String)]
                    [when-code : (Code (-> S Any))]
                    [trans-code : (Code (-> S S))]
                    [priority : Integer]
                    [weight : Exact-Positive-Integer]
                    [attributes : (Immutable-HashTable Symbol Any)])
  #:transparent
  #:type-name Edge)

(: edge-trans (All (T S) (-> (Edge T S) (-> S S))))
(define (edge-trans e)
  (code-value (edge-trans-code e)))

(: edge-trans-sexp (All (T S) (-> (Edge T S) Sexp)))
(define (edge-trans-sexp e)
  (code-sexp (edge-trans-code e)))

(: edge-when (All (T S) (-> (Edge T S) (-> S Any))))
(define (edge-when e)
  (code-value (edge-when-code e)))

(: edge-when-sexp (All (T S) (-> (Edge T S) Sexp)))
(define (edge-when-sexp e)
  (code-sexp (edge-when-code e)))


(struct (T S) bridge ([id : Symbol]
                      [name : String]
                      [mode : EdgeMode]
                      [half? : Boolean]
                      [dom : (Node T S)]
                      [cod : (Node Any Any)]
                      [desc : (Option String)]
                      [when-code : (Code (-> S Any))]
                      [trans-code : (Code (-> S Any))]
                      [priority : Integer]
                      [weight : Exact-Positive-Integer]
                      [attributes : (Immutable-HashTable Symbol Any)])
  #:transparent
  #:type-name Bridge)

(: bridge-trans (All (T S) (-> (Bridge T S) (-> S Any))))
(define (bridge-trans e)
  (code-value (bridge-trans-code e)))

(: bridge-trans-sexp (All (T S) (-> (Bridge T S) Sexp)))
(define (bridge-trans-sexp e)
  (code-sexp (bridge-trans-code e)))

(: bridge-when (All (T S) (-> (Bridge T S) (-> S Any))))
(define (bridge-when e)
  (code-value (bridge-when-code e)))

(: bridge-when-sexp (All (T S) (-> (Bridge T S) Sexp)))
(define (bridge-when-sexp e)
  (code-sexp (bridge-when-code e)))


(define-type AnyEdge (Edge Any Any))

(: make-generic-edge* (All (T S)
                           (case-> (-> 'edge
                                       #:name String
                                       #:mode (Option EdgeMode)
                                       #:half? Boolean
                                       #:dom (Node T S)
                                       #:cod (Node T S)
                                       #:desc (Option String)
                                       #:when (Option (Code (-> S Any)))
                                       #:trans (Code (-> S S))
                                       #:priority (Option Integer)
                                       #:weight (Option Exact-Positive-Integer)
                                       #:attributes (Immutable-HashTable Symbol Any)
                                       (Edge T S))
                                   (-> 'bridge
                                       #:name String
                                       #:mode (Option EdgeMode)
                                       #:half? Boolean
                                       #:dom (Node T S)
                                       #:cod (Node Any Any)
                                       #:desc (Option String)
                                       #:when (Option (Code (-> S Any)))
                                       #:trans (Code (-> S Any))
                                       #:priority (Option Integer)
                                       #:weight (Option Exact-Positive-Integer)
                                       #:attributes (Immutable-HashTable Symbol Any)
                                       (Bridge T S)))))
(define (make-generic-edge* type
                            #:name name
                            #:mode mode
                            #:half? half?
                            #:dom dom
                            #:cod cod
                            #:desc desc
                            #:when when
                            #:trans tr
                            #:priority priority
                            #:weight weight
                            #:attributes attrs)
  (let ([edge-id (make-edge-id name dom)])
    (cond [(set-member? (current-seen-ids) edge-id)
           (error "edge, bridge: duplicate ID" edge-id)]
          [else (current-seen-ids (set-add (current-seen-ids) edge-id))])
    ((case type [(edge) edge] [(bridge) bridge])
     edge-id
     name (or mode 'choose)
     half?
     dom cod
     desc
     (or when (make-code #f (const #t)))
     tr
     (or priority 0)
     (or weight 1)
     attrs)))

(: make-bridge (All (T S)
                    (-> #:name String
                        #:mode (Option EdgeMode)
                        #:half?  Boolean
                        #:dom (Node T S)
                        #:cod (Node Any Any)
                        #:desc (Option String)
                        #:when (Option (Code (-> S Any)))
                        #:trans (Code (-> S Any))
                        #:priority (Option Integer)
                        #:weight (Option Exact-Positive-Integer)
                        #:attributes (Immutable-HashTable Symbol Any)
                        (Bridge T S))))
(define (make-bridge #:name name
                     #:mode mode
                     #:half? half?
                     #:dom dom
                     #:cod cod
                     #:desc desc
                     #:when when
                     #:trans tr
                     #:priority priority
                     #:weight weight
                     #:attributes attrs)
  ((inst make-generic-edge* T S) 'bridge
                                 #:name name
                                 #:mode mode
                                 #:half? half?
                                 #:dom dom
                                 #:cod cod
                                 #:desc desc
                                 #:when when
                                 #:trans tr
                                 #:priority priority
                                 #:weight weight
                                 #:attributes attrs))

(: bridge* (All (T S)
                (-> String
                    [#:mode (Option EdgeMode)]
                    [#:half Boolean]
                    #:dom (Node T S)
                    #:cod (Node Any Any)
                    [#:desc (Option String)]
                    [#:when (Option (Code (-> S Any)))]
                    #:trans (Code (-> S Any))
                    [#:priority (Option Integer)]
                    [#:weight (Option Exact-Positive-Integer)]
                    (Bridge T S))))
(define (bridge* name
                 #:mode [mode #f]
                 #:half [half? #f]
                 #:dom dom
                 #:cod cod
                 #:desc [desc #f]
                 #:when [when #f]
                 #:trans tr
                 #:priority [priority #f]
                 #:weight [weight #f])
  ((inst make-bridge T S) #:name name
                          #:mode mode
                          #:half? half?
                          #:dom dom
                          #:cod cod
                          #:desc desc
                          #:when when
                          #:trans (or tr (inst identity S))
                          #:priority priority
                          #:weight weight
                          #:attributes ((inst hash Symbol Any))))

(: make-edge (All (T S)
                  (-> #:name String
                      #:mode (Option EdgeMode)
                      #:half? Boolean
                      #:dom (Node T S)
                      #:cod (Node T S)
                      #:desc (Option String)
                      #:when (Option (Code (-> S Any)))
                      #:trans (Option (Code (-> S S)))
                      #:priority (Option Integer)
                      #:weight (Option Exact-Positive-Integer)
                      #:attributes (Immutable-HashTable Symbol Any)
                      (Edge T S))))
(define (make-edge #:name name
                   #:mode mode
                   #:half? half?
                   #:dom dom
                   #:cod cod
                   #:desc desc
                   #:when when
                   #:trans tr
                   #:priority priority
                   #:weight weight
                   #:attributes attrs)
  ((inst make-generic-edge* T S) 'edge
                                 #:name name
                                 #:mode mode
                                 #:half? half?
                                 #:dom dom
                                 #:cod cod
                                 #:desc desc
                                 #:when when
                                 #:trans (or tr (make-code #f (inst identity S)))
                                 #:priority priority
                                 #:weight weight
                                 #:attributes attrs))

(: edge* (All (T S)
              (-> String
                  [#:mode (Option EdgeMode)]
                  [#:half? Boolean]
                  #:dom (Node T S)
                  #:cod (Node T S)
                  [#:desc (Option String)]
                  [#:when (Option (Code (-> S Any)))]
                  [#:trans (Option (Code (-> S S)))]
                  [#:priority (Option Integer)]
                  [#:weight (Option Exact-Positive-Integer)]
                  (Edge T S))))
(define (edge* name
               #:mode [mode #f]
               #:half? [half? #f]
               #:dom dom
               #:cod cod
               #:desc [desc #f]
               #:when [when #f]
               #:trans [tr #f]
               #:priority [priority #f]
               #:weight [weight #f])
  ((inst make-edge T S) #:name name
                        #:mode mode
                        #:half? half?
                        #:dom dom
                        #:cod cod
                        #:desc desc
                        #:when when
                        #:trans (or tr (make-code #f (inst identity S)))
                        #:priority priority
                        #:weight weight
                        #:attributes ((inst hash Symbol Any))))

(: any-bridge (All (T S) (-> (-> Any Any : #:+ S)
                             (-> (Bridge T S) (Edge Any Any)))))
(define ((any-bridge p?) b)
  (edge (bridge-id b)
        (bridge-name b)
        (bridge-mode b)
        (bridge-half? b)
        ((any-node p?) (bridge-dom b))
        ((inst bridge-cod T S) b)
        (bridge-desc b)
        (make-code (bridge-when-sexp b) (lambda (x) ((bridge-when b) (assert x p?))))
        (make-code (bridge-trans-sexp b) (lambda (x) ((bridge-trans b) (assert x p?))))
        (bridge-priority b)
        (bridge-weight b)
        (bridge-attributes b)))

(: any-edge (All (T S) (-> (-> Any Any : #:+ S)
                           (-> (Edge T S)
                               AnyEdge))))
(define ((any-edge p?) e)
  (struct-copy edge e
               [dom ((any-node p?) (edge-dom e))]
               [cod ((any-node p?) (edge-cod e))]
               [trans-code (make-code (edge-trans-sexp e)
                                      (lambda (x) ((edge-trans e) (assert x p?))))]
               [when-code (make-code (edge-when-sexp e)
                                     (lambda (x) ((edge-when e) (assert x p?))))]))

(struct (T S) graph ([id : Symbol]
                     [name : String]
                     [parent-id : (Option Symbol)]
                     [parent-name : (Option String)]
                     [desc : (Option String)]
                     [edges : (Listof (Edge T S))])
  #:transparent
  #:type-name Graph)

(define-type AnyGraph (Graph Any Any))

(: graph* (All (T S) (-> String
                         [#:parent-name (Option String)]
                         [#:desc (Option String)]
                         [#:edges (Option (Listof (Edge T S)))]
                         (Graph T S))))
(define (graph* name
                #:parent-name [parent-name #f]
                #:desc [desc #f]
                #:edges [edges #f])
  (let ([graph-id (make-graph-id name)])
    (cond [(set-member? (current-seen-ids) graph-id)
           (error "graph: duplicate ID" graph-id)]
          [else (current-seen-ids (set-add (current-seen-ids) graph-id))])
    (graph (make-graph-id name) name
           (and parent-name (make-graph-id parent-name)) parent-name
           desc
           (or edges '()))))

(struct (T S) open-graph ([graph : (Graph T S)]
                          [bridges : (Listof (Bridge T S))])
  #:transparent
  #:type-name OpenGraph)

(: open-graph* (All (T S)
                    (-> String
                        [#:parent-name (Option String)]
                        [#:desc (Option String)]
                        [#:edges (Option (Listof (Edge T S)))]
                        [#:bridges (Option (Listof (Bridge T S)))]
                        (OpenGraph T S))))
(define (open-graph* name
                     #:parent-name [parent-name #f]
                     #:desc [desc #f]
                     #:edges [edges #f]
                     #:bridges [bridges #f])
  (open-graph ((inst graph* T S) name
                                 #:parent-name parent-name
                                 #:desc desc
                                 #:edges edges)
              (or bridges '())))

(: any-graph (All (T S) (-> (-> Any Any : #:+ S)
                            (-> (U (Graph T S) (OpenGraph T S)) AnyGraph))))
(define ((any-graph p?) g)
  (if (open-graph? g)
      (struct-copy graph (open-graph-graph g)
                   [edges (append (map (any-edge p?) (graph-edges (open-graph-graph g)))
                                  (map (any-bridge p?) (open-graph-bridges g)))])
      (struct-copy graph g
                   [edges (map (any-edge p?) (graph-edges g))])))
