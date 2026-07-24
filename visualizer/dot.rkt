#lang typed/racket

(require "../graph.rkt")
(require "../graph/dot.rkt")
(require "../private/visualizer.rkt")
(require "../history.rkt")
(require typed/racket/draw typed/pict)

(provide DotWriter dot-writer write-dot render-dot
         dot-current-node? dot-visited-node? dot-visited-edge?
         DotConfig make-dot-config
         DotGlobalConfig make-dot-global-config
         DotNodeConfig make-dot-node-config
         DotEdgeConfig make-dot-edge-config
         DotNodeShape DotNodeStyle
         DotArrowShape DotEdgeStyle
         current-dot-fontname current-dot-fontsize current-dot-dpi current-dot-rankdir
         current-dot-node-config
         current-dot-current-node-config
         current-dot-visited-node-config
         current-dot-auto-edge-config
         current-dot-visited-auto-edge-config
         current-dot-choose-edge-config
         current-dot-visited-choose-edge-config
         current-dot-annotation-edge-config)

(struct (T S) graph-config ([global : DotGlobalConfig]
                            [node : (-> (Node T S) DotNodeConfig DotNodeConfig)]
                            [edge-node : (-> (Edge T S) DotNodeConfig DotNodeConfig)]
                            [edge : (-> (Edge T S) DotEdgeConfig DotEdgeConfig)])
  #:type-name DotConfig)

(: make-dot-config (All (T S)
                    (-> [#:global (Option DotGlobalConfig)]
                        [#:node (Option (-> (Node T S) DotNodeConfig DotNodeConfig))]
                        [#:edge-node (Option (-> (Edge T S) DotNodeConfig DotNodeConfig))]
                        [#:edge (Option (-> (Edge T S) DotEdgeConfig DotEdgeConfig))]
                        (DotConfig T S))))
(define (make-dot-config #:global [global #f]
                     #:node [node #f]
                     #:edge-node [edge-node #f]
                     #:edge [edge #f])
  (: node-default (-> (Node T S) DotNodeConfig))
  (define (node-default n)
    (cond [(and (dot-current-node? n)
                (current-dot-current-node-config))
           => identity]
          [(and (dot-visited-node? n)
                (current-dot-visited-node-config))
           => identity]
          [else
           (current-dot-node-config)]))
  (: edge-default (-> (Edge T S) DotEdgeConfig))
  (define (edge-default e)
    (let ([mode (edge-mode e)])
      (cond [(eq? mode 'auto)
             (cond
               [(and (dot-visited-edge? e)
                     (current-dot-visited-auto-edge-config))
                => identity]
               [else (current-dot-auto-edge-config)])]
            [(eq? mode 'choose)
             (cond [(and (dot-visited-edge? e)
                         (current-dot-visited-choose-edge-config))
                    => identity]
                   [else (current-dot-choose-edge-config)])]
            [(eq? mode 'annotation) (current-dot-annotation-edge-config)])))
  ((inst graph-config T S) (or global (make-dot-global-config))
                           (lambda ([n : (Node T S)] [_ : DotNodeConfig])
                             (if node
                                 (node n (node-default n))
                                 (node-default n)))
                           (lambda ([e : (Edge T S)] [_ : DotNodeConfig])
                             (if edge-node
                                 (edge-node e (current-dot-edge-node-config))
                                 (current-dot-edge-node-config)))
                           (lambda ([e : (Edge T S)] [_ : DotEdgeConfig])
                             (let ([c (if edge
                                          (edge e (edge-default e))
                                          (edge-default e))])
                               (if (edge-dot-minlen e)
                                   (struct-copy edge-config c [minlen (edge-dot-minlen e)])
                                   c)))))

(define-type Rankdir (U 'TB 'LR 'BT 'RL))

(struct global-config ([fontname : String]
                       [fontsize : Positive-Integer]
                       [rankdir : Rankdir]
                       [dpi : Positive-Integer])
  #:type-name DotGlobalConfig)

(: make-dot-global-config (-> [#:fontname (Option String)]
                              [#:fontsize (Option Positive-Integer)]
                              [#:rankdir (Option Rankdir)]
                              [#:dpi (Option Positive-Integer)]
                              DotGlobalConfig))
(define (make-dot-global-config #:fontname [fontname #f]
                                #:fontsize [fontsize #f]
                                #:rankdir [rankdir #f]
                                #:dpi [dpi #f])
  (global-config (or fontname (current-dot-fontname))
                 (or fontsize (current-dot-fontsize))
                 (or rankdir (current-dot-rankdir))
                 (or dpi (current-dot-dpi))))

(: current-dot-fontname (Parameterof String))
(define current-dot-fontname (make-parameter "Times-Roman"))

(: current-dot-fontsize (Parameterof Positive-Integer))
(define current-dot-fontsize (make-parameter 14))

(: current-dot-rankdir (Parameterof Rankdir))
(define current-dot-rankdir (make-parameter 'TB))

(: current-dot-dpi (Parameterof Positive-Integer))
(define current-dot-dpi (make-parameter 96))

(struct node-config ([shape : DotNodeShape]
                     [style : (Listof DotNodeStyle)]
                     [color : String]
                     [fillcolor : String])
  #:type-name DotNodeConfig)

(: make-dot-node-config (-> [#:shape (Option DotNodeShape)]
                            [#:style (Option (Listof DotNodeStyle))]
                            [#:color (Option String)]
                            [#:fillcolor (Option String)]
                            DotNodeConfig))
(define (make-dot-node-config #:shape [shape #f]
                              #:style [style #f]
                              #:color [color #f]
                              #:fillcolor [fillcolor #f])
  (node-config (or shape 'ellipse)
               (or style '())
               (or color "black")
               (or fillcolor "white")))

(struct edge-config ([arrowhead : (U DotArrowShape String)]
                     [arrowtail : (U DotArrowShape String)]
                     [style : (Listof DotEdgeStyle)]
                     [color : String]
                     [minlen : Natural])
  #:type-name DotEdgeConfig)

(: make-dot-edge-config (-> [#:arrowhead (Option DotArrowShape)]
                            [#:arrowtail (Option DotArrowShape)]
                            [#:style (Option (Listof DotEdgeStyle))]
                            [#:color (Option String)]
                            [#:minlen (Option Natural)]
                            DotEdgeConfig))
(define (make-dot-edge-config #:arrowhead [arrowhead #f]
                              #:arrowtail [arrowtail #f]
                              #:style [style #f]
                              #:color [color #f]
                              #:minlen [minlen #f])
  (edge-config (or arrowhead 'normal)
               (or arrowtail 'normal)
               (or style '())
               (or color "black")
               (or minlen 1)))

(define-type DotNodeShape
  (U 'box
     'polygon
     'ellipse
     'oval
     'circle
     'point
     'egg
     'triangle
     'plaintext
     'plain
     'diamond
     'trapezium
     'parallelogram
     'house
     'pentagon
     'hexagon
     'septagon
     'octagon
     'doublecircle
     'doubleoctagon
     'tripleoctagon
     'invtriangle
     'invtrapezium
     'invhouse
     'Mdiamond
     'Msquare
     'Mcircle
     'rect
     'rectangle
     'square
     'star
     'none
     'underline
     'cylinder
     'note
     'tab
     'folder
     'box3d
     'component))

(define-type DotNodeStyle
  (U 'dashed
     'dotted
     'solid
     'invis
     'bold
     'filled
     'striped
     'wedged
     'diagonals
     'rounded))

(define-type DotEdgeStyle
  (U 'dashed
     'dotted
     'solid
     'invis
     'bold
     'tapered))

(define-type ClusterStyle
  (U 'filled
     'striped
     'rounded))

(define-type DotArrowShape
  (U 'box 'lbox 'rbox 'obox 'olbox 'orbox
     'crow 'lcrow 'rcrow
     'diamond 'ldiamond 'rdiamond 'odiamond 'oldiamond 'ordiamond
     'dot 'odot
     'inv 'linv 'rinv 'oinv 'olinv 'orinv
     'none
     'normal 'lnormal 'rnormal 'onormal 'olnormal 'ornormal
     'tee 'ltee 'rtee
     'vee 'lvee 'rvee
     'curve 'lcurve 'rcurve 'icurve 'licurve 'ricurve))

(: current-dot-node-config (Parameterof DotNodeConfig))
(define current-dot-node-config
  (make-parameter (make-dot-node-config #:shape 'box #:style '(filled rounded))))

(: current-dot-visited-node-config (Parameterof (Option DotNodeConfig)))
(define current-dot-visited-node-config
  (make-parameter (make-dot-node-config #:shape 'box #:style '(filled rounded)
                                        #:fillcolor "gray")))

(: current-dot-current-node-config (Parameterof (Option DotNodeConfig)))
(define current-dot-current-node-config
  (make-parameter (make-dot-node-config #:shape 'box #:style '(filled rounded)
                                        #:fillcolor "yellow")))

(: current-dot-edge-node-config (Parameterof DotNodeConfig))
(define current-dot-edge-node-config
  (make-parameter (make-dot-node-config #:shape 'plaintext)))

(: current-dot-auto-edge-config (Parameterof DotEdgeConfig))
(define current-dot-auto-edge-config
  (make-parameter (make-dot-edge-config #:color "red")))

(: current-dot-visited-auto-edge-config (Parameterof (Option DotEdgeConfig)))
(define current-dot-visited-auto-edge-config
  (make-parameter (make-dot-edge-config #:color "orange")))

(: current-dot-choose-edge-config (Parameterof DotEdgeConfig))
(define current-dot-choose-edge-config
  (make-parameter (make-dot-edge-config #:color "blue")))

(: current-dot-visited-choose-edge-config (Parameterof (Option DotEdgeConfig)))
(define current-dot-visited-choose-edge-config
  (make-parameter (make-dot-edge-config #:color "cyan")))

(: current-dot-annotation-edge-config (Parameterof DotEdgeConfig))
(define current-dot-annotation-edge-config
  (make-parameter (make-dot-edge-config #:style '(dashed) #:color "black")))

(: show-sexp (-> Sexp String))
(define (show-sexp x)
  (let ([out (open-output-string)])
    (print x out 1)
    (get-output-string out)))

(struct %dot-writer ([proc : (-> Output-Port Void)])
  #:type-name DotWriter)

(: write-dot (->* (DotWriter) (Output-Port) Void))
(define (write-dot x [port (current-output-port)])
  ((%dot-writer-proc x) port))

(: dot-writer (All (T S) (-> (Listof (Graph T S)) (Node T S)
                             [#:config (DotConfig T S)]
                             [#:history (History T S)]
                             DotWriter)))
(define (dot-writer gs node
                    #:config [config ((inst make-dot-config T S))]
                    #:history [h '()])
  (%dot-writer
   (lambda ([port : Output-Port])
     (%write-dot gs node #:config config #:history h #:port port))))

(: %write-dot (All (T S) (-> (Listof (Graph T S)) (Node T S)
                             #:config (DotConfig T S)
                             #:port Output-Port
                             #:history (History T S)
                             Void)))
(define (%write-dot gs node
                    #:config config
                    #:port port
                    #:history h)
  (parameterize ([current-visited-ids (history->visited-ids h)]
                 [current-node-id (history->current-node-id h)])
    (let ([visnodes (reachable-visnodes gs node)])
      (displayln (format "digraph G {") port)
      (fprintf port "  graph [rankdir=~a,dpi=~a]\n"
               (dot-string (symbol->string
                            (global-config-rankdir
                             (graph-config-global config))))
               (dot-string (number->string
                            (global-config-dpi (graph-config-global config)))))
      (fprintf port "  fontname=~a\n"
               (dot-string (global-config-fontname (graph-config-global config))))
      (fprintf port "  fontsize=~a\n"
               (dot-string (number->string
                            (global-config-fontsize (graph-config-global config)))))

      (: display-visnodes (-> (Nested-Graphs T S) Void))
      (define (display-visnodes g)
        (fprintf port "subgraph ~a {\n" (dot-string (string-append
                                                     "cluster_"
                                                     (symbol->string (graph-id (car g))))))
        (fprintf port "  label = ~a\n" (dot-string (graph-name (car g))))

        (for-each (lambda ([v : (VisNode T S)])
                    (when (symbol=? (graph-id (car g)) (graph-id (cadr v)))
                      (define get-id (inst visnode-id T S))
                      (cond
                        [(eq? 'node (car v))
                         (fprintf port "  ~a ~a\n"
                                  (dot-string (symbol->string (get-id v)))
                                  (format-node-attributes
                                   (string-join `(,(mark-node-title (node-name (caddr v)))
                                                  ,@(cond [(node-desc (caddr v)) => list]
                                                          [else '()])
                                                  ,@(cond [(node-prompt-sexp (caddr v))
                                                           => (lambda (x)
                                                                (list (format "prompt: ~a" (show-sexp x))))]
                                                          [else '()])
                                                  ,@(cond [(node-trans-sexp (caddr v))
                                                           => (lambda (x)
                                                                (list (format "trans: ~a" (show-sexp x))))]
                                                          [else '()]))
                                                "\n")
                                   ((graph-config-node config) (caddr v)
                                                               (make-dot-node-config))))]
                        [(eq? 'edge (car v))
                         (fprintf port "  ~a ~a\n"
                                  (dot-string (symbol->string (get-id v)))
                                  (format-node-attributes
                                   (string-join `(,(mark-edge-title (edge-name (caddr v)))
                                                  ,@(cond [(edge-desc (caddr v)) => list]
                                                          [else '()])
                                                  ,@(cond [(edge-when-sexp (caddr v))
                                                           => (lambda (x)
                                                                (list (format "when: ~a" (show-sexp x))))]
                                                          [else '()])
                                                  ,@(cond [(edge-trans-sexp (caddr v))
                                                           => (lambda (x)
                                                                (list (format "trans: ~a" (show-sexp x))))]
                                                          [else '()]))
                                                "\n")
                                   ((graph-config-edge-node config) (caddr v)
                                                                    (make-dot-node-config))))])))
                  visnodes)
        (for-each display-visnodes (cdr g))
        (displayln "}" port))
      (for-each display-visnodes (graphs->nested (visnodes->graphs visnodes)))
      (newline port)
      (for-each (lambda ([v : (VisNode-Edge T S)])
                  (fprintf port "  ~a -> ~a ~a\n"
                           (dot-string (symbol->string (node-id (edge-dom (caddr v)))))
                           (dot-string (symbol->string (edge-id (caddr v))))
                           (format-edge-attributes
                            (show-priority (edge-priority (caddr v)))
                            (if (edge-half? (caddr v))
                                ((graph-config-edge config) (caddr v) (make-dot-edge-config))
                                (struct-copy edge-config ((graph-config-edge config) (caddr v)
                                                                                     (make-dot-edge-config))
                                             [arrowhead 'none]))))
                  (unless (edge-half? (caddr v))
                    (fprintf port "  ~a -> ~a ~a\n"
                             (dot-string (symbol->string (edge-id (caddr v))))
                             (dot-string (symbol->string (node-id (edge-cod (caddr v)))))
                             (format-edge-attributes
                              ""
                              (struct-copy edge-config ((graph-config-edge config) (caddr v)
                                                                                   (make-dot-edge-config))
                                           [arrowtail 'none])))))
                (visnodes-edges visnodes))
      (displayln "}" port))))

(: mark-node-title (-> String String))
(define (mark-node-title str)
  (format "【~a】" str))

(: mark-edge-title (-> String String))
(define (mark-edge-title str)
  (format "[~a]" str))

(: byte->hex-string (-> Byte String))
(define (byte->hex-string b)
  (if (<= b 15)
      (format "0~x" b)
      (format "~x" b)))

(: node-styles->string (-> (Listof DotNodeStyle) String))
(define (node-styles->string styles)
  (string-join (map symbol->string styles) ","))

(: edge-styles->string (-> (Listof DotEdgeStyle) String))
(define (edge-styles->string styles)
  (string-join (map symbol->string styles) ","))

(: node-shape->string (-> DotNodeShape String))
(define node-shape->string symbol->string)

(: format-node-attributes (-> String DotNodeConfig String))
(define (format-node-attributes label nc)
  (format "[label=~a,shape=~a,style=~a,color=~a,fillcolor=~a]"
          (dot-string label)
          (dot-string (node-shape->string (node-config-shape nc)))
          (dot-string (node-styles->string (node-config-style nc)))
          (dot-string (node-config-color nc))
          (dot-string (node-config-fillcolor nc))))

(: arrow-shape->string (-> (U DotArrowShape String) String))
(define (arrow-shape->string s)
  (if (symbol? s)
      (symbol->string s)
      s))

(: format-edge-attributes (-> String DotEdgeConfig String))
(define (format-edge-attributes label ec)
  (format "[label=~a,arrowhead=~a,arrowtail=~a,style=~a,color=~a,minlen=~a]"
          (dot-string label)
          (dot-string (arrow-shape->string (edge-config-arrowhead ec)))
          (dot-string (arrow-shape->string (edge-config-arrowtail ec)))
          (dot-string (edge-styles->string (edge-config-style ec)))
          (dot-string (edge-config-color ec))
          (edge-config-minlen ec)))

(: dot-string (-> String String))
(define (dot-string str)
  (with-output-to-string
    (lambda ()
      (write-char #\")
      (for ([ch (in-string str)])
        (case ch
          [(#\\) (display "\\\\")]
          [(#\") (display "\\\"")]
          [(#\newline) (display "\\n")]
          [else (write-char ch)]))
      (write-char #\"))))

(: show-priority (-> Integer String))
(define (show-priority k)
  (cond [(positive-integer? k) (format "+~a" k)]
        [(zero? k) ""]
        [else (format "~a" k)]))

(: history->visited-ids (All (T S) (-> (History T S)  (Setof Symbol))))
(define (history->visited-ids h)
  (list->set (map (lambda ([r : (History-Record T S)])
                    (case (car r)
                      [(node) (node-id (history-record-node r))]
                      [(auto choose) (edge-id (history-record-edge r))]))
                  h)))

(: history->current-node-id (All (T S) (-> (History T S) (Option Symbol))))
(define (history->current-node-id h)
  (and (pair? h)
       (eq? (caar h) 'node)
       (node-id (history-record-node (car h)))))

(: current-visited-ids (Parameterof (Setof Symbol)))
(define current-visited-ids (make-parameter ((inst set Symbol))))

(: current-node-id (Parameterof (Option Symbol)))
(define current-node-id (make-parameter #f))

(: dot-visited-node? (All (T S) (-> (Node T S) Boolean)))
(define (dot-visited-node? n)
  (set-member? (current-visited-ids) (node-id n)))

(: dot-visited-edge? (All (T S) (-> (Edge T S) Boolean)))
(define (dot-visited-edge? e)
  (set-member? (current-visited-ids) (edge-id e)))

(: dot-current-node? (All (T S) (-> (Node T S) Boolean)))
(define (dot-current-node? n)
  (cond [(current-node-id) => (lambda ([id : Symbol]) (eq? id (node-id n)))]
        [else #f]))

(: render-dot (-> DotWriter (Instance Bitmap%) Void))
(define (render-dot writer bmp)
  (define p (process "dot -Tpng"))
  (write-dot writer (second p))
  (close-output-port (second p))
  (send bmp load-file (first p))
  (if (eq? ((fifth p) 'status) 'done-ok)
      (void)
      (error 'render-dot "fail load")))
