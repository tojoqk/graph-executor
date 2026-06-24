#lang typed/racket

(require "../graph.rkt")
(require "../private/visualizer.rkt")

(provide write-dot
         GraphConfig make-graph-config
         NodeConfig make-node-config
         EdgeConfig make-edge-config
         Color RGBColor rgb-color rgb-color?
         NodeShape NodeStyle
         ArrowShape EdgeStyle)

(define-type EdgeNodeConfig (-> (U 'auto 'choose)
                                (Option Condition)
                                (Option Trans)
                                ))

(struct (T) graph-config ([node : (-> T NodeConfig)]
                          [edge-node : (-> (U 'auto 'choose) NodeConfig)]
                          [bridge-node : (-> (U 'auto 'choose) NodeConfig)]
                          [edge : (-> (U 'auto 'choose) EdgeConfig)]
                          [bridge : (-> (U 'auto 'choose) EdgeConfig)])
  #:type-name GraphConfig)

(: make-graph-config (All (T)
                          (-> [#:node (Option (-> T NodeConfig))]
                              [#:edge-node (Option (-> (U 'auto 'choose) NodeConfig))]
                              [#:bridge-node (Option (-> (U 'auto 'choose) NodeConfig))]
                              [#:edge (Option (-> (U 'auto 'choose) EdgeConfig))]
                              [#:bridge (Option (-> (U 'auto 'choose) EdgeConfig))]
                              (GraphConfig T))))
(define (make-graph-config #:node [node #f]
                           #:edge-node [edge-node #f]
                           #:bridge-node [bridge-node #f]
                           #:edge [edge #f]
                           #:bridge [bridge #f])
  ((inst graph-config T) (or node (const (make-node-config #:shape 'box
                                                           #:style '(filled rounded))))
                         (or edge-node (const (make-node-config #:shape 'plaintext)))
                         (or bridge-node (const (make-node-config #:shape 'plaintext)))
                         (or edge (lambda ([mode : (U 'auto 'choose)])
                                         (if (eq? mode 'auto)
                                             (make-edge-config #:color "red")
                                             (make-edge-config #:color "blue"))))
                         (or bridge (const (make-edge-config)))))

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
                     [color : Color])
  #:type-name EdgeConfig)

(: make-edge-config (-> [#:arrowhead (Option ArrowShape)]
                        [#:arrowtail (Option ArrowShape)]
                        [#:style (Option (Listof EdgeStyle))]
                        [#:color (Option Color)]
                        EdgeConfig))
(define (make-edge-config #:arrowhead [arrowhead #f]
                          #:arrowtail [arrowtail #f]
                          #:style [style #f]
                          #:color [color #f])
  (edge-config (or arrowhead 'normal)
               (or arrowtail 'normal)
               (or style '())
               (or color "black")))

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
     'component
     'promoter
     'cds
     'terminator
     'utr
     'primersite
     'restrictionsite
     'fivepoverhang
     'threepoverhang
     'noverhang
     'assembly
     'signature
     'insulator
     'ribosite
     'rnastab
     'proteasesite
     'proteinstab
     'rpromoter
     'rarrow
     'larrow
     'lpromoter))

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
                            [#:config (GraphConfig T)]
                            [#:port Output-Port]
                            Void)))
(define (write-dot gs node
                   #:config [config (make-graph-config)]
                   #:port [port (current-output-port)])
  (let ([visnodes (reachable-visnodes gs node)])
    (displayln (format "digraph G {") port)
    (displayln  "  graph [rankdir=TB]" port)
    (for-each (lambda ([v : (VisNode T S)])
                (define get-id (inst visnode-id T S))
                (cond
                  [(eq? 'node (car v))
                   (fprintf port "  ~a ~a"
                            (dot-string (symbol->string (get-id v)))
                            (format-node-attributes
                             (node-name (cdr v))
                             ((graph-config-node config) (node-type (cdr v)))))]
                  [(eq? 'edge (car v))
                   (fprintf port "  ~a ~a"
                            (dot-string (symbol->string (get-id v)))
                            (format-node-attributes
                             (edge-name (cdr v))
                             ((graph-config-edge-node config) (edge-mode (cdr v)))))]
                  [(eq? 'bridge (car v))
                   (fprintf port "  ~a ~a"
                            (dot-string (symbol->string (get-id v)))
                            (format-node-attributes
                             (edge-name (cdr v))
                             ((graph-config-bridge-node config) (edge-mode (cdr v)))))]))
              visnodes)
    (newline port)
    (for-each (lambda ([v : (U (Pairof 'edge (Edge T S))
                               (Pairof 'bridge (Edge T S)))])
                (fprintf port "  ~a -> ~a ~a"
                         (dot-string (symbol->string (node-id (edge-dom (cdr v)))))
                         (dot-string (symbol->string (edge-id (cdr v))))
                         (format-edge-attributes
                          ""
                          (struct-copy edge-config
                                       (cond
                                         [(eq? 'edge (car v))
                                          ((graph-config-edge config) (edge-mode (cdr v)))]
                                         [(eq? 'bridge (car v))
                                          ((graph-config-edge config) (edge-mode (cdr v)))])
                                       [arrowhead 'none])))
                (fprintf port "  ~a -> ~a ~a\n"
                         (dot-string (symbol->string (edge-id (cdr v))))
                         (dot-string (symbol->string (node-id (edge-cod (cdr v)))))
                         (format-edge-attributes
                          ""
                          (struct-copy edge-config
                                       (cond
                                         [(eq? 'edge (car v))
                                          ((graph-config-edge config) (edge-mode (cdr v)))]
                                         [(eq? 'bridge (car v))
                                          ((graph-config-edge config) (edge-mode (cdr v)))])
                                       [arrowtail 'none]))))
              (visnodes-edges visnodes))
    (displayln "}" port)))

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
  (format "[label=~a,arrowhead=~a,arrowtail=~a,style=~a,color=~a]"
          (dot-string label)
          (dot-string (arrow-shape->string (edge-config-arrowhead ec)))
          (dot-string (arrow-shape->string (edge-config-arrowtail ec)))
          (dot-string (edge-styles->string (edge-config-style ec)))
          (dot-string (color->string (edge-config-color ec)))))

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
