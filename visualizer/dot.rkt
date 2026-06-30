#lang typed/racket

(require "../graph.rkt")
(require "../graph/dot.rkt")
(require "../private/visualizer.rkt")

(provide write-dot
         GraphConfig make-graph-config
         NodeConfig make-node-config
         EdgeConfig make-edge-config
         Color RGBColor rgb-color rgb-color?
         NodeShape NodeStyle
         ArrowShape EdgeStyle)

(struct (T S) graph-config ([node : (-> (Node T S) NodeConfig)]
                            [edge-node : (-> (Edge T S) NodeConfig)]
                            [edge : (-> (Edge T S) EdgeConfig)])
  #:type-name GraphConfig)

(: make-graph-config (All (T S)
                          (-> [#:node (Option (-> (Node T S) NodeConfig))]
                              [#:edge-node (Option (-> (Edge T S) NodeConfig))]
                              [#:edge (Option (-> (Edge T S) EdgeConfig))]
                              (GraphConfig T S))))
(define (make-graph-config #:node [node #f]
                           #:edge-node [edge-node #f]
                           #:edge [edge #f])
  (: edge-default (-> (Edge T S) EdgeConfig))
  (define (edge-default e)
    (let ([mode (edge-mode e)])
      (cond [(eq? mode 'auto)
             (make-edge-config #:color "red")]
            [(eq? mode 'choose) (make-edge-config #:color "blue")]
            [(eq? mode 'annotation)
             (make-edge-config #:style '(dashed)
                               #:color "black")])))
  ((inst graph-config T S) (lambda ([n : (Node T S)])
                             (if node
                                 (node n)
                                 (make-node-config #:shape 'box
                                                   #:style '(filled rounded))))
                           (lambda ([e : (Edge T S)])
                             (if edge-node
                                 (edge-node e)
                                 (make-node-config #:shape 'plaintext)))
                           (lambda ([e : (Edge T S)])
                             (let ([c (if edge
                                          (edge e)
                                          (edge-default e))])
                               (if (edge-dot-minlen e)
                                   (struct-copy edge-config c
                                                [minlen (edge-dot-minlen e)])
                                   c)))))

(struct node-config ([shape : NodeShape]
                     [style : (Listof NodeStyle)]
                     [color : Color]
                     [fillcolor : Color])
  #:type-name NodeConfig)

(: make-node-config (-> [#:shape (Option NodeShape)]
                        [#:style (Option (Listof NodeStyle))]
                        [#:color (Option Color)]
                        [#:fillcolor (Option Color)]
                        NodeConfig))
(define (make-node-config #:shape [shape #f]
                          #:style [style #f]
                          #:color [color #f]
                          #:fillcolor [fillcolor #f])
  (node-config (or shape 'ellipse)
               (or style '())
               (or color "black")
               (or fillcolor "white")))

(struct edge-config ([arrowhead : (U ArrowShape String)]
                     [arrowtail : (U ArrowShape String)]
                     [style : (Listof EdgeStyle)]
                     [color : Color]
                     [minlen : Natural])
  #:type-name EdgeConfig)

(: make-edge-config (-> [#:arrowhead (Option ArrowShape)]
                        [#:arrowtail (Option ArrowShape)]
                        [#:style (Option (Listof EdgeStyle))]
                        [#:color (Option Color)]
                        [#:minlen (Option Natural)]
                        EdgeConfig))
(define (make-edge-config #:arrowhead [arrowhead #f]
                          #:arrowtail [arrowtail #f]
                          #:style [style #f]
                          #:color [color #f]
                          #:minlen [minlen #f])
  (edge-config (or arrowhead 'normal)
               (or arrowtail 'normal)
               (or style '())
               (or color "black")
               (or minlen 1)))

(define-type NodeShape
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

(define-type NodeStyle
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

(define-type EdgeStyle
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

(define-type Color (U RGBColor String))

(struct rgb-color ([red : Byte]
                   [green : Byte]
                   [blue : Byte])
  #:type-name RGBColor)

(define-type ArrowShape
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

(: write-dot (All (T S) (-> (Listof (Graph T S)) (Node T S)
                            [#:config (GraphConfig T S)]
                            [#:port Output-Port]
                            Void)))
(define (write-dot gs node
                   #:config [config ((inst make-graph-config T S))]
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
                                                        [else '()]))
                                              "\n")
                                 ((graph-config-node config) (caddr v))))]
                      [(eq? 'edge (car v))
                       (fprintf port "  ~a ~a\n"
                                (dot-string (symbol->string (get-id v)))
                                (format-node-attributes
                                 (string-join `(,(mark-edge-title (edge-name (caddr v)))
                                                ,@(cond [(edge-desc (caddr v)) => list]
                                                        [else '()]))
                                              "\n")
                                 ((graph-config-edge-node config) (caddr v))))])))
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
                          ""
                          (struct-copy edge-config ((graph-config-edge config) (caddr v))
                                       [arrowhead 'none])))
                (fprintf port "  ~a -> ~a ~a\n"
                         (dot-string (symbol->string (edge-id (caddr v))))
                         (dot-string (symbol->string (node-id (edge-cod (caddr v)))))
                         (format-edge-attributes
                          ""
                          (struct-copy edge-config ((graph-config-edge config) (caddr v))
                                       [arrowtail 'none]))))
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

(: color->string (-> Color String))
(define (color->string c)
  (if (rgb-color? c)
      (format "#~a~a~a"
              (byte->hex-string (rgb-color-red c))
              (byte->hex-string (rgb-color-green c))
              (byte->hex-string (rgb-color-blue c)))
      c))

(: node-styles->string (-> (Listof NodeStyle) String))
(define (node-styles->string styles)
  (string-join (map symbol->string styles) ","))

(: edge-styles->string (-> (Listof EdgeStyle) String))
(define (edge-styles->string styles)
  (string-join (map symbol->string styles) ","))

(: node-shape->string (-> NodeShape String))
(define node-shape->string symbol->string)

(: format-node-attributes (-> String NodeConfig String))
(define (format-node-attributes label nc)
  (format "[label=~a,shape=~a,style=~a,color=~a,fillcolor=~a]"
          (dot-string label)
          (dot-string (node-shape->string (node-config-shape nc)))
          (dot-string (node-styles->string (node-config-style nc)))
          (dot-string (color->string (node-config-color nc)))
          (dot-string (color->string (node-config-fillcolor nc)))))

(: arrow-shape->string (-> (U ArrowShape String) String))
(define (arrow-shape->string s)
  (if (symbol? s)
      (symbol->string s)
      s))

(: format-edge-attributes (-> String EdgeConfig String))
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
