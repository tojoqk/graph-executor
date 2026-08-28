#lang typed/racket

(provide Code code make-code Code-Expr
         Code-Sexp code-sexp code-sexp? code-sexp-sexp
         Code-Text code-text code-text? code-text-text
         current-graph-used-ids current-node-prompt
         Node node-maker
         node-graph-id node-graph-name node-id node-name node-type node-tags node-desc node-trans node-trans-code-expr node-before-code-expr node-after-code-expr node-prompt node-prompt-code-expr node-node-options
         Node-Option (struct-out node-option)
         Node-Info node-info? node-node-info node-info-name node-info-type node-info-tags node-info-desc
         any-node
         Edge Bridge EdgeMode edge? make-edge make-bridge
         edge-id edge-name edge-mode edge-half? edge-from edge-to edge-desc edge-when edge-when-code-expr edge-trans edge-trans-code-expr edge-before-code-expr edge-after-code-expr edge-priority edge-edge-options
         Edge-Option (struct-out edge-option)
         any-bridge any-edge
         Graph OpenGraph make-graph make-open-graph
         graph-id graph-name graph-parent-id graph-parent-name graph-desc graph-edges
         any-graph)

(struct code-sexp ([sexp : Sexp])
  #:transparent
  #:type-name Code-Sexp)

(struct code-text ([text : String])
  #:transparent
  #:type-name Code-Text)

(define-type Code-Expr (U Code-Sexp Code-Text))

(struct (A) %code ([code-expr : (Option Code-Expr)]
                   [value : A])
  #:transparent
  #:type-name Code)

(define-syntax code
  (syntax-rules ()
    [(_ expr) (%code (code-sexp 'expr) expr)]))

(: make-code (All (A) (-> Code-Expr A (Code A))))
(define (make-code expr value)
  (%code expr value))

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
                  [before-code-expr : (Option Code-Expr)]
                  [after-code-expr : (Option Code-Expr)]
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

(: node-trans-code-expr (All (S) (-> (Node S) (Option Code-Expr))))
(define (node-trans-code-expr n)
  (%code-code-expr (node-trans-code n)))

(: node-prompt (All (S) (-> (Node S) (-> S String))))
(define (node-prompt n)
  (%code-value (node-prompt-code n)))

(: node-prompt-code-expr (All (S) (-> (Node S) (Option Code-Expr))))
(define (node-prompt-code-expr n)
  (%code-code-expr (node-prompt-code n)))

(: %make-node (All (S)
                  (-> #:graph-name String
                      #:name String
                      #:type Symbol
                      #:tags (Listof Symbol)
                      #:desc (Option String)
                      #:trans (Option (Code (-> S S)))
                      #:before (Option (Code (-> S Any)))
                      #:after (Option (Code (-> S Any)))
                      #:prompt (Option (U String (Code (-> S String))))
                      #:options (Listof Node-Option)
                      (Node S))))
(define (%make-node #:graph-name graph-name #:name name #:type type #:tags tags #:desc desc #:trans tr #:before before #:after after #:prompt pmt #:options opts)
  (let ([graph-id (make-graph-id graph-name)]
        [node-id (make-node-id graph-name name)])
    (cond [(set-member? (current-graph-used-ids) node-id)
           (error "node: duplicate ID" node-id)]
          [else (current-graph-used-ids (set-add (current-graph-used-ids) node-id))])
    (node graph-id graph-name node-id
          (node-info name type tags desc)
          (let ([tr (or tr (%code #f identity))])
            (cond [before (cond [after (let ([before-proc (%code-value before)]
                                             [after-proc (%code-value after)]
                                             [trans-proc (%code-value tr)])
                                         (%code (%code-code-expr tr)
                                                (lambda ([st : S])
                                                  (before-proc st)
                                                  (let ([new-st (trans-proc st)])
                                                    (after-proc new-st)
                                                    new-st))))]
                                [else (%code (%code-code-expr tr)
                                             (let ([before-proc (%code-value before)]
                                                   [trans-proc (%code-value tr)])
                                               (lambda ([st : S])
                                                 (before-proc st)
                                                 (trans-proc st))))])]
                  [after (%code (%code-code-expr tr)
                                (let ([after-proc (%code-value after)]
                                      [trans-proc (%code-value tr)])
                                  (lambda ([st : S])
                                    (let ([new-st (trans-proc st)])
                                      (after-proc new-st)
                                      new-st))))]
                  [else tr]))
          (and before (%code-code-expr before))
          (and after (%code-code-expr after))
          (cond [(not pmt) (%code #f (const (current-node-prompt)))]
                [(string? pmt) (%code (code-sexp pmt) (const pmt))]
                [else (%code (%code-code-expr pmt)
                             (lambda ([s : S])
                               ((%code-value pmt) s)))])
          opts)))

(: node-maker (-> String
                  (All (S T)
                       (-> String
                           #:type (∩ T Symbol)
                           [#:tags (Listof Symbol)]
                           [#:desc (Option String)]
                           [#:trans (Option (Code (-> S S)))]
                           [#:before (Option (Code (-> S Any)))]
                           [#:after (Option (Code (-> S Any)))]
                           [#:prompt (Option (U String (Code (-> S String))))]
                           [#:options (Listof Node-Option)]
                           (Node S)))))
(define ((node-maker graph-name) name #:type type #:tags [tags '()] #:desc [desc #f] #:trans [tr #f] #:before [before #f] #:after [after #f] #:prompt [pmt #f] #:options [options '()])
  (%make-node #:graph-name graph-name #:name name #:type type #:tags tags #:desc desc
              #:trans tr #:before before #:after after
              #:prompt pmt
              #:options options))

(: any-node (All (S) (-> (-> Any Any : #:+ S) (-> (Node S) (Node Any)))))
(define ((any-node p?) n)
  (struct-copy node n
               [trans-code (%code (node-trans-code-expr n)
                                  (lambda ([x : Any]) ((node-trans n) (assert x p?))))]
               [prompt-code (%code (node-prompt-code-expr n)
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
                  [before-code-expr : (Option Code-Expr)]
                  [after-code-expr : (Option Code-Expr)]
                  [priority : Integer]
                  [edge-options : (Listof Edge-Option)])
  #:transparent
  #:type-name Edge)

(: edge-trans (All (S) (-> (Edge S) (-> S S))))
(define (edge-trans e)
  (%code-value (edge-trans-code e)))

(: edge-trans-code-expr (All (S) (-> (Edge S) (Option Code-Expr))))
(define (edge-trans-code-expr e)
  (%code-code-expr (edge-trans-code e)))

(: edge-when (All (S) (-> (Edge S) (-> S Any))))
(define (edge-when e)
  (%code-value (edge-when-code e)))

(: edge-when-code-expr (All (S) (-> (Edge S) (Option Code-Expr))))
(define (edge-when-code-expr e)
  (%code-code-expr (edge-when-code e)))


(struct (S) bridge ([id : Symbol]
                    [name : String]
                    [mode : EdgeMode]
                    [half? : Boolean]
                    [from : (Node S)]
                    [to : (Node Any)]
                    [desc : (Option String)]
                    [when-code : (Code (-> S Any))]
                    [trans-code : (Code (-> S Any))]
                    [before-code-expr : (Option Code-Expr)]
                    [after-code-expr : (Option Code-Expr)]
                    [priority : Integer]
                    [edge-options : (Listof Edge-Option)])
  #:transparent
  #:type-name Bridge)

(: bridge-trans (All (S) (-> (Bridge S) (-> S Any))))
(define (bridge-trans e)
  (%code-value (bridge-trans-code e)))

(: bridge-trans-code-expr (All (S) (-> (Bridge S) (Option Code-Expr))))
(define (bridge-trans-code-expr e)
  (%code-code-expr (bridge-trans-code e)))

(: bridge-when (All (S) (-> (Bridge S) (-> S Any))))
(define (bridge-when e)
  (%code-value (bridge-when-code e)))

(: bridge-when-code-expr (All (S) (-> (Bridge S) (Option Code-Expr))))
(define (bridge-when-code-expr e)
  (%code-code-expr (bridge-when-code e)))

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
                                       #:before (Option (Code (-> S Any)))
                                       #:after (Option (Code (-> S Any)))
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
                                       #:before (Option (Code (-> S Any)))
                                       #:after (Option (Code (-> Any Any)))
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
                            #:before before
                            #:after after
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
     (or when (%code #f (const #t)))
     (cond [before (cond [after (let ([before-proc (%code-value before)]
                                      [after-proc (%code-value after)]
                                      [trans-proc (%code-value tr)])
                                  (%code (%code-code-expr tr)
                                         (lambda ([st : S])
                                           (before-proc st)
                                           (let ([new-st (trans-proc st)])
                                             (after-proc new-st)
                                             new-st))))]
                         [else (%code (%code-code-expr tr)
                                      (let ([before-proc (%code-value before)]
                                            [trans-proc (%code-value tr)])
                                        (lambda ([st : S])
                                          (before-proc st)
                                          (trans-proc st))))])]
           [after (%code (%code-code-expr tr)
                         (let ([after-proc (%code-value after)]
                               [trans-proc (%code-value tr)])
                           (lambda ([st : S])
                             (let ([new-st (trans-proc st)])
                               (after-proc new-st)
                               new-st))))]
           [else tr])
     (and before (%code-code-expr before))
     (and after (%code-code-expr after))
     (or priority 0)
     opts)))

(: make-bridge (All (S)
                    (-> String
                        [#:mode (Option EdgeMode)]
                        [#:half? Boolean]
                        #:from (Node S)
                        #:to (Node Any)
                        [#:desc (Option String)]
                        [#:when (Option (Code (-> S Any)))]
                        #:trans (Code (-> S Any))
                        [#:before (Option (Code (-> S Any)))]
                        [#:after (Option (Code (-> Any Any)))]
                        [#:priority (Option Integer)]
                        [#:options (Listof Edge-Option)]
                        (Bridge S))))
(define (make-bridge name
                     #:mode [mode #f]
                     #:half? [half? #f]
                     #:from from
                     #:to to
                     #:desc [desc #f]
                     #:when [when #f]
                     #:trans tr
                     #:before [before #f]
                     #:after [after #f]
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
                               #:before before
                               #:after after
                               #:priority priority
                               #:options opts))

(: make-edge (All (S)
                  (-> String
                      [#:mode (Option EdgeMode)]
                      [#:half? Boolean]
                      #:from (Node S)
                      #:to (Node S)
                      [#:desc (Option String)]
                      [#:when (Option (Code (-> S Any)))]
                      [#:trans (Option (Code (-> S S)))]
                      [#:before (Option (Code (-> S Any)))]
                      [#:after (Option (Code (-> S Any)))]
                      [#:priority (Option Integer)]
                      [#:options (Listof Edge-Option)]
                      (Edge S))))
(define (make-edge name
                   #:mode [mode #f]
                   #:half? [half? #f]
                   #:from from
                   #:to to
                   #:desc [desc #f]
                   #:when [when #f]
                   #:trans [tr #f]
                   #:before [before #f]
                   #:after [after #f]
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
                               #:trans (or tr (%code #f (inst identity S)))
                               #:before before
                               #:after after
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
        (%code (bridge-when-code-expr b) (lambda (x) ((bridge-when b) (assert x p?))))
        (%code (bridge-trans-code-expr b) (lambda (x) ((bridge-trans b) (assert x p?))))
        (bridge-before-code-expr b)
        (bridge-after-code-expr b)
        (bridge-priority b)
        (bridge-edge-options b)))

(: any-edge (All (S) (-> (-> Any Any : #:+ S)
                         (-> (Edge S)
                             (Edge Any)))))
(define ((any-edge p?) e)
  (struct-copy edge e
               [from ((any-node p?) (edge-from e))]
               [to ((any-node p?) (edge-to e))]
               [trans-code (%code (edge-trans-code-expr e)
                                  (lambda (x) ((edge-trans e) (assert x p?))))]
               [when-code (%code (edge-when-code-expr e)
                                 (lambda (x) ((edge-when e) (assert x p?))))]))

(struct (S) graph ([id : Symbol]
                   [name : String]
                   [parent-id : (Option Symbol)]
                   [parent-name : (Option String)]
                   [desc : (Option String)]
                   [edges : (Listof (Edge S))])
  #:transparent
  #:type-name Graph)

(: make-graph (All (S) (-> String
                           [#:parent-name (Option String)]
                           [#:desc (Option String)]
                           [#:edges (Option (Listof (Edge S)))]
                           (Graph S))))
(define (make-graph name
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

(: make-open-graph (All (S)
                        (-> String
                            [#:parent-name (Option String)]
                            [#:desc (Option String)]
                            [#:edges (Option (Listof (Edge S)))]
                            [#:bridges (Option (Listof (Bridge S)))]
                            (OpenGraph S))))
(define (make-open-graph name
                         #:parent-name [parent-name #f]
                         #:desc [desc #f]
                         #:edges [edges #f]
                         #:bridges [bridges #f])
  (open-graph ((inst make-graph S) name
                                   #:parent-name parent-name
                                   #:desc desc
                                   #:edges edges)
              (or bridges '())))

(: any-graph (All (S) (-> (-> Any Any : #:+ S)
                          (-> (U (Graph S) (OpenGraph S)) (Graph Any)))))
(define ((any-graph p?) g)
  (if (open-graph? g)
      (struct-copy graph (open-graph-graph g)
                   [edges (append (map (any-edge p?) (graph-edges (open-graph-graph g)))
                                  (map (any-bridge p?) (open-graph-bridges g)))])
      (struct-copy graph g
                   [edges (map (any-edge p?) (graph-edges g))])))
