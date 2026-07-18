#lang typed/racket

(require "../graph.rkt")
(require "../graph/dot.rkt")
(require "../private/visualizer.rkt")

(provide write-dot
         DotConfig make-dot-config
         DotNodeConfig make-dot-node-config
         DotEdgeConfig make-dot-edge-config
         DotColor DotRGBColor dot-rgb-color
         DotNodeShape DotNodeStyle
         DotArrowShape DotEdgeStyle
         current-dot-node-config
         current-dot-auto-edge-config
         current-dot-choose-edge-config
         current-dot-annotation-edge-config)

(struct (T S) graph-config ([node : (-> (Node T S) DotNodeConfig DotNodeConfig)]
                            [edge-node : (-> (Edge T S) DotNodeConfig DotNodeConfig)]
                            [edge : (-> (Edge T S) DotEdgeConfig DotEdgeConfig)])
  #:type-name DotConfig)

(: make-dot-config (All (T S)
                        (-> [#:node (Option (-> (Node T S) DotNodeConfig DotNodeConfig))]
                            [#:edge-node (Option (-> (Edge T S) DotNodeConfig DotNodeConfig))]
                            [#:edge (Option (-> (Edge T S) DotEdgeConfig DotEdgeConfig))]
                            (DotConfig T S))))
(define (make-dot-config #:node [node #f]
                         #:edge-node [edge-node #f]
                         #:edge [edge #f])
  (: edge-default (-> (Edge T S) DotEdgeConfig))
  (define (edge-default e)
    (let ([mode (edge-mode e)])
      (cond [(eq? mode 'auto) (current-dot-auto-edge-config)]
            [(eq? mode 'choose) (current-dot-choose-edge-config)]
            [(eq? mode 'annotation) (current-dot-annotation-edge-config)])))
  ((inst graph-config T S) (lambda ([n : (Node T S)] [_ : DotNodeConfig])
                             (if node
                                 (node n (current-dot-node-config))
                                 (current-dot-node-config)))
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

(struct node-config ([shape : DotNodeShape]
                     [style : (Listof DotNodeStyle)]
                     [color : DotColor]
                     [fillcolor : DotColor])
  #:type-name DotNodeConfig)

(: make-dot-node-config (-> [#:shape (Option DotNodeShape)]
                            [#:style (Option (Listof DotNodeStyle))]
                            [#:color (Option DotColor)]
                            [#:fillcolor (Option DotColor)]
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
                     [color : DotColor]
                     [minlen : Natural])
  #:type-name DotEdgeConfig)

(: make-dot-edge-config (-> [#:arrowhead (Option DotArrowShape)]
                            [#:arrowtail (Option DotArrowShape)]
                            [#:style (Option (Listof DotEdgeStyle))]
                            [#:color (Option DotColor)]
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

(define-type DotColor (U DotRGBColor String))

(struct dot-rgb-color ([red : Byte]
                       [green : Byte]
                       [blue : Byte])
  #:type-name DotRGBColor)

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

(: current-dot-edge-node-config (Parameterof DotNodeConfig))
(define current-dot-edge-node-config
  (make-parameter (make-dot-node-config #:shape 'plaintext)))

(: current-dot-auto-edge-config (Parameterof DotEdgeConfig))
(define current-dot-auto-edge-config
  (make-parameter (make-dot-edge-config #:color "red")))

(: current-dot-choose-edge-config (Parameterof DotEdgeConfig))
(define current-dot-choose-edge-config
  (make-parameter (make-dot-edge-config #:color "blue")))

(: current-dot-annotation-edge-config (Parameterof DotEdgeConfig))
(define current-dot-annotation-edge-config
  (make-parameter (make-dot-edge-config #:style '(dashed) #:color "black")))

(: show-sexp (-> Sexp String))
(define (show-sexp x)
  (let ([out (open-output-string)])
    (print x out 1)
    (get-output-string out)))

(: write-dot (All (T S) (-> (Listof (Graph T S)) (Node T S)
                            [#:config (DotConfig T S)]
                            [#:port Output-Port]
                            Void)))
(define (write-dot gs node
                   #:config [config ((inst make-dot-config T S))]
                   #:port [port (current-output-port)])
  (let ([visnodes (reachable-visnodes gs node)])
    (displayln (format "digraph G {") port)
    (displayln  "  graph [rankdir=TB]" port)
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
    (displayln "}" port)))

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

(: color->string (-> DotColor String))
(define (color->string c)
  (if (dot-rgb-color? c)
      (format "#~a~a~a"
              (byte->hex-string (dot-rgb-color-red c))
              (byte->hex-string (dot-rgb-color-green c))
              (byte->hex-string (dot-rgb-color-blue c)))
      c))

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
          (dot-string (color->string (node-config-color nc)))
          (dot-string (color->string (node-config-fillcolor nc)))))

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
          (dot-string (color->string (edge-config-color ec)))
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
  (cond [(positive-integer? k) (format "priority: +~a" k)]
        [(zero? k) ""]
        [else (format "priority: ~a" k)]))
