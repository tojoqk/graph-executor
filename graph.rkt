#lang typed/racket

(provide Code code make-code
         current-graph-used-ids current-node-prompt
         Node AnyNode make-node (rename-out [node* node])
         node-graph-id node-graph-name node-id node-name node-type node-tags node-desc node-trans node-trans-sexp node-prompt node-prompt-sexp node-node-options
         Node-Option (struct-out node-option)
         Node-Info node-node-info node-info-name node-info-type node-info-tags node-info-desc
         any-node
         Edge AnyEdge Bridge EdgeMode edge? (rename-out [edge* edge] [bridge* bridge])
         edge-id edge-name edge-mode edge-half? edge-from edge-to edge-desc edge-when edge-when-sexp edge-trans edge-trans-sexp edge-priority edge-edge-options
         Edge-Option (struct-out edge-option)
         any-bridge any-edge
         Graph AnyGraph OpenGraph (rename-out [graph* graph]) (rename-out [open-graph* open-graph])
         graph-id graph-name graph-parent-id graph-parent-name graph-desc graph-edges
         any-graph)

(struct (A) %code ([sexp : Sexp]
                   [value : A])
  #:transparent
  #:constructor-name make-code
  #:type-name Code)

(define-syntax code
  (syntax-rules ()
    [(_ expr) (make-code 'expr expr)]))

(: current-graph-used-ids (Parameterof (Setof Symbol)))
(define current-graph-used-ids (make-parameter ((inst set Symbol))))

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

(: make-edge-id (All (S) (-> String (Node S) Symbol)))
(define (make-edge-id edge-name from)
  (let ([from-id (symbol->string (node-id from))])
    (string->symbol (format "~a[~a]~a"
                            from-id
                            (string-length edge-name)
                            edge-name))))

(struct node-info ([name : String]
                   [type : Symbol]
                   [tags : (Listof Symbol)]
                   [desc : (Option String)])
  #:type-name Node-Info)

(struct node-option ()
  #:type-name Node-Option)

(struct (S) node ([graph-id : Symbol]
                  [graph-name : String]
                  [id : Symbol]
                  [node-info : Node-Info]
                  [trans-code : (Code (-> S S))]
                  [prompt-code : (Code (-> S String))]
                  [node-options : (Listof Node-Option)])
  #:transparent
  #:type-name Node)

(: node-name (All (S) (-> (Node S) String)))
(define (node-name n)
  (node-info-name (node-node-info n)))

(: node-type (All (S) (-> (Node S) Symbol)))
(define (node-type n)
  (node-info-type (node-node-info n)))

(: node-tags (All (S) (-> (Node S) (Listof Symbol))))
(define (node-tags n)
  (node-info-tags (node-node-info n)))

(: node-desc (All (S) (-> (Node S) (Option String))))
(define (node-desc n)
  (node-info-desc (node-node-info n)))

(: node-trans (All (S) (-> (Node S) (-> S S))))
(define (node-trans n)
  (%code-value (node-trans-code n)))

(: node-trans-sexp (All (S) (-> (Node S) Sexp)))
(define (node-trans-sexp n)
  (%code-sexp (node-trans-code n)))

(: node-prompt (All (S) (-> (Node S) (-> S String))))
(define (node-prompt n)
  (%code-value (node-prompt-code n)))

(: node-prompt-sexp (All (S) (-> (Node S) Sexp)))
(define (node-prompt-sexp n)
  (%code-sexp (node-prompt-code n)))

(define-type AnyNode (Node Any))

(: make-node (All (S)
                  (-> #:graph-name String
                      #:name String
                      #:type Symbol
                      #:tags (Listof Symbol)
                      #:desc (Option String)
                      #:trans (Option (Code (-> S S)))
                      #:prompt (Option (U String (Code (-> S String))))
                      #:options (Listof Node-Option)
                      (Node S))))
(define (make-node #:graph-name graph-name #:name name #:type type #:tags tags #:desc desc #:trans tr #:prompt pmt #:options opts)
  (let ([graph-id (make-graph-id graph-name)]
        [node-id (make-node-id graph-name name)])
    (cond [(set-member? (current-graph-used-ids) node-id)
           (error "node: duplicate ID" node-id)]
          [else (current-graph-used-ids (set-add (current-graph-used-ids) node-id))])
    (node graph-id graph-name node-id
          (node-info name type tags desc)
          (or tr (make-code #f identity))
          (cond [(not pmt) (make-code #f (const (current-node-prompt)))]
                [(string? pmt) (make-code pmt (const pmt))]
                [else (make-code (%code-sexp pmt)
                                 (lambda ([s : S])
                                   ((%code-value pmt) s)))])
          opts)))

(: node* (-> String
             (All (S T)
                  (-> String
                      #:type (∩ T Symbol)
                      [#:tags (Listof Symbol)]
                      [#:desc (Option String)]
                      [#:trans (Option (Code (-> S S)))]
                      [#:prompt (Option (U String (Code (-> S String))))]
                      [#:options (Listof Node-Option)]
                      (Node S)))))
(define ((node* graph-name) name #:type type #:tags [tags '()] #:desc [desc #f] #:trans [tr #f] #:prompt [pmt #f] #:options [options '()])
  (make-node #:graph-name graph-name #:name name #:type type #:tags tags #:desc desc
             #:trans tr
             #:prompt pmt
             #:options options))

(: any-node (All (S) (-> (-> Any Any : #:+ S) (-> (Node S) AnyNode))))
(define ((any-node p?) n)
  (struct-copy node n
               [trans-code (make-code (node-trans-sexp n)
                                      (lambda ([x : Any]) ((node-trans n) (assert x p?))))]
               [prompt-code (make-code (node-prompt-sexp n)
                                       (lambda ([x : Any]) ((node-prompt n) (assert x p?))))]))

(define-type EdgeMode (U 'auto 'choose 'annotation))

(struct edge-option ()
  #:type-name Edge-Option)

(struct (S) edge ([id : Symbol]
                  [name : String]
                  [mode : EdgeMode]
                  [half? : Boolean]
                  [from : (Node S)]
                  [to : (Node S)]
                  [desc : (Option String)]
                  [when-code : (Code (-> S Any))]
                  [trans-code : (Code (-> S S))]
                  [priority : Integer]
                  [edge-options : (Listof Edge-Option)])
  #:transparent
  #:type-name Edge)

(: edge-trans (All (S) (-> (Edge S) (-> S S))))
(define (edge-trans e)
  (%code-value (edge-trans-code e)))

(: edge-trans-sexp (All (S) (-> (Edge S) Sexp)))
(define (edge-trans-sexp e)
  (%code-sexp (edge-trans-code e)))

(: edge-when (All (S) (-> (Edge S) (-> S Any))))
(define (edge-when e)
  (%code-value (edge-when-code e)))

(: edge-when-sexp (All (S) (-> (Edge S) Sexp)))
(define (edge-when-sexp e)
  (%code-sexp (edge-when-code e)))


(struct (S) bridge ([id : Symbol]
                    [name : String]
                    [mode : EdgeMode]
                    [half? : Boolean]
                    [from : (Node S)]
                    [to : (Node Any)]
                    [desc : (Option String)]
                    [when-code : (Code (-> S Any))]
                    [trans-code : (Code (-> S Any))]
                    [priority : Integer]
                    [edge-options : (Listof Edge-Option)])
  #:transparent
  #:type-name Bridge)

(: bridge-trans (All (S) (-> (Bridge S) (-> S Any))))
(define (bridge-trans e)
  (%code-value (bridge-trans-code e)))

(: bridge-trans-sexp (All (S) (-> (Bridge S) Sexp)))
(define (bridge-trans-sexp e)
  (%code-sexp (bridge-trans-code e)))

(: bridge-when (All (S) (-> (Bridge S) (-> S Any))))
(define (bridge-when e)
  (%code-value (bridge-when-code e)))

(: bridge-when-sexp (All (S) (-> (Bridge S) Sexp)))
(define (bridge-when-sexp e)
  (%code-sexp (bridge-when-code e)))

(define-type AnyEdge (Edge Any))

(: make-generic-edge* (All (S)
                           (case-> (-> 'edge
                                       #:name String
                                       #:mode (Option EdgeMode)
                                       #:half? Boolean
                                       #:from (Node S)
                                       #:to (Node S)
                                       #:desc (Option String)
                                       #:when (Option (Code (-> S Any)))
                                       #:trans (Code (-> S S))
                                       #:priority (Option Integer)
                                       #:options (Listof Edge-Option)
                                       (Edge S))
                                   (-> 'bridge
                                       #:name String
                                       #:mode (Option EdgeMode)
                                       #:half? Boolean
                                       #:from (Node S)
                                       #:to (Node Any)
                                       #:desc (Option String)
                                       #:when (Option (Code (-> S Any)))
                                       #:trans (Code (-> S Any))
                                       #:priority (Option Integer)
                                       #:options (Listof Edge-Option)
                                       (Bridge S)))))
(define (make-generic-edge* type
                            #:name name
                            #:mode mode
                            #:half? half?
                            #:from from
                            #:to to
                            #:desc desc
                            #:when when
                            #:trans tr
                            #:priority priority
                            #:options opts)
  (let ([edge-id (make-edge-id name from)])
    (cond [(set-member? (current-graph-used-ids) edge-id)
           (error "edge, bridge: duplicate ID" edge-id)]
          [else (current-graph-used-ids (set-add (current-graph-used-ids) edge-id))])
    ((case type [(edge) edge] [(bridge) bridge])
     edge-id
     name (or mode 'choose)
     half?
     from to
     desc
     (or when (make-code #f (const #t)))
     tr
     (or priority 0)
     opts)))

(: bridge* (All (S)
                (-> String
                    [#:mode (Option EdgeMode)]
                    [#:half Boolean]
                    #:from (Node S)
                    #:to (Node Any)
                    [#:desc (Option String)]
                    [#:when (Option (Code (-> S Any)))]
                    #:trans (Code (-> S Any))
                    [#:priority (Option Integer)]
                    [#:options (Listof Edge-Option)]
                    (Bridge S))))
(define (bridge* name
                 #:mode [mode #f]
                 #:half [half? #f]
                 #:from from
                 #:to to
                 #:desc [desc #f]
                 #:when [when #f]
                 #:trans tr
                 #:priority [priority #f]
                 #:options [opts '()])
  ((inst make-generic-edge* S) 'bridge
                               #:name name
                               #:mode mode
                               #:half? half?
                               #:from from
                               #:to to
                               #:desc desc
                               #:when when
                               #:trans (or tr (inst identity S))
                               #:priority priority
                               #:options opts))

(: edge* (All (S)
              (-> String
                  [#:mode (Option EdgeMode)]
                  [#:half? Boolean]
                  #:from (Node S)
                  #:to (Node S)
                  [#:desc (Option String)]
                  [#:when (Option (Code (-> S Any)))]
                  [#:trans (Option (Code (-> S S)))]
                  [#:priority (Option Integer)]
                  [#:options (Listof Edge-Option)]
                  (Edge S))))
(define (edge* name
               #:mode [mode #f]
               #:half? [half? #f]
               #:from from
               #:to to
               #:desc [desc #f]
               #:when [when #f]
               #:trans [tr #f]
               #:priority [priority #f]
               #:options [opts '()])
  ((inst make-generic-edge* S) 'edge
                               #:name name
                               #:mode mode
                               #:half? half?
                               #:from from
                               #:to to
                               #:desc desc
                               #:when when
                               #:trans (or tr (make-code #f (inst identity S)))
                               #:priority priority
                               #:options opts))

(: any-bridge (All (S) (-> (-> Any Any : #:+ S)
                           (-> (Bridge S) (Edge Any)))))
(define ((any-bridge p?) b)
  (edge (bridge-id b)
        (bridge-name b)
        (bridge-mode b)
        (bridge-half? b)
        ((any-node p?) (bridge-from b))
        ((inst bridge-to S) b)
        (bridge-desc b)
        (make-code (bridge-when-sexp b) (lambda (x) ((bridge-when b) (assert x p?))))
        (make-code (bridge-trans-sexp b) (lambda (x) ((bridge-trans b) (assert x p?))))
        (bridge-priority b)
        (bridge-edge-options b)))

(: any-edge (All (S) (-> (-> Any Any : #:+ S)
                         (-> (Edge S)
                             AnyEdge))))
(define ((any-edge p?) e)
  (struct-copy edge e
               [from ((any-node p?) (edge-from e))]
               [to ((any-node p?) (edge-to e))]
               [trans-code (make-code (edge-trans-sexp e)
                                      (lambda (x) ((edge-trans e) (assert x p?))))]
               [when-code (make-code (edge-when-sexp e)
                                     (lambda (x) ((edge-when e) (assert x p?))))]))

(struct (S) graph ([id : Symbol]
                   [name : String]
                   [parent-id : (Option Symbol)]
                   [parent-name : (Option String)]
                   [desc : (Option String)]
                   [edges : (Listof (Edge S))])
  #:transparent
  #:type-name Graph)

(define-type AnyGraph (Graph Any))

(: graph* (All (S) (-> String
                       [#:parent-name (Option String)]
                       [#:desc (Option String)]
                       [#:edges (Option (Listof (Edge S)))]
                       (Graph S))))
(define (graph* name
                #:parent-name [parent-name #f]
                #:desc [desc #f]
                #:edges [edges #f])
  (let ([graph-id (make-graph-id name)])
    (cond [(set-member? (current-graph-used-ids) graph-id)
           (error "graph: duplicate ID" graph-id)]
          [else (current-graph-used-ids (set-add (current-graph-used-ids) graph-id))])
    (graph (make-graph-id name) name
           (and parent-name (make-graph-id parent-name)) parent-name
           desc
           (or edges '()))))

(struct (S) open-graph ([graph : (Graph S)]
                        [bridges : (Listof (Bridge S))])
  #:transparent
  #:type-name OpenGraph)

(: open-graph* (All (S)
                    (-> String
                        [#:parent-name (Option String)]
                        [#:desc (Option String)]
                        [#:edges (Option (Listof (Edge S)))]
                        [#:bridges (Option (Listof (Bridge S)))]
                        (OpenGraph S))))
(define (open-graph* name
                     #:parent-name [parent-name #f]
                     #:desc [desc #f]
                     #:edges [edges #f]
                     #:bridges [bridges #f])
  (open-graph ((inst graph* S) name
                               #:parent-name parent-name
                               #:desc desc
                               #:edges edges)
              (or bridges '())))

(: any-graph (All (S) (-> (-> Any Any : #:+ S)
                          (-> (U (Graph S) (OpenGraph S)) AnyGraph))))
(define ((any-graph p?) g)
  (if (open-graph? g)
      (struct-copy graph (open-graph-graph g)
                   [edges (append (map (any-edge p?) (graph-edges (open-graph-graph g)))
                                  (map (any-bridge p?) (open-graph-bridges g)))])
      (struct-copy graph g
                   [edges (map (any-edge p?) (graph-edges g))])))
