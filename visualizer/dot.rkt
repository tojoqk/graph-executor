#lang typed/racket

(require "../graph.rkt")
(require "../graph/dot.rkt")
(require "../private/visualizer.rkt")
(require "../history.rkt")

(provide DotWriter dot-writer write-dot
         DotConfig dot-config
         DotNodeStatus DotEdgeStatus
         DotGlobalConfig dot-global-config
         DotNodeConfig dot-node-config
         DotEdgeConfig dot-edge-config
         current-dot-fontname current-dot-fontsize current-dot-dpi current-dot-rankdir
         current-dot-node-config current-dot-edge-node-config current-dot-edge-config)

(define-type DotNodeStatus (U 'default 'visited 'current))
(: dot-node-status (All (S) (-> (Node S) DotNodeStatus)) )
(define (dot-node-status n)
  (cond [(dot-current-node? n) 'current]
        [(dot-visited-node? n) 'visited]
        [else 'default]))

(define-type DotEdgeStatus (U 'default 'visited))
(: dot-edge-status (All (S) (-> (Edge S) DotEdgeStatus)) )
(define (dot-edge-status n)
  (cond [(dot-visited-edge? n) 'visited]
        [else 'default]))

(struct %dot-config ([global : DotGlobalConfig]
                     [node : (-> Symbol DotNodeStatus DotNodeConfig)]
                     [edge-node : (-> EdgeMode Symbol Symbol DotEdgeStatus DotNodeConfig)]
                     [edge : (-> EdgeMode Symbol Symbol DotEdgeStatus DotEdgeConfig)])
  #:type-name DotConfig)

(: dot-config (-> [#:global (Option DotGlobalConfig)]
                  [#:node (Option (-> Symbol DotNodeStatus DotNodeConfig))]
                  [#:edge-node (Option (-> EdgeMode Symbol Symbol DotEdgeStatus DotNodeConfig))]
                  [#:edge (Option (-> EdgeMode Symbol Symbol DotEdgeStatus DotEdgeConfig))]
                  DotConfig))
(define (dot-config #:global [global #f]
                    #:node [node #f]
                    #:edge-node [edge-node #f]
                    #:edge [edge #f])
  (%dot-config (or global (dot-global-config))
               (or node (current-dot-node-config))
               (or edge-node (current-dot-edge-node-config))
               (or edge (current-dot-edge-config))))

(define-type Rankdir (U 'TB 'LR 'BT 'RL))

(struct global-config ([fontname : String]
                       [fontsize : Positive-Integer]
                       [rankdir : Rankdir]
                       [dpi : Positive-Integer])
  #:type-name DotGlobalConfig)

(: dot-global-config (-> [#:fontname (Option String)]
                         [#:fontsize (Option Positive-Integer)]
                         [#:rankdir (Option Rankdir)]
                         [#:dpi (Option Positive-Integer)]
                         DotGlobalConfig))
(define (dot-global-config #:fontname [fontname #f]
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

(struct node-config ([shape : String]
                     [style : (Listof String)]
                     [color : String]
                     [fillcolor : String])
  #:type-name DotNodeConfig)

(: dot-node-config (-> [#:shape (Option String)]
                       [#:style (Option (Listof String))]
                       [#:color (Option String)]
                       [#:fillcolor (Option String)]
                       [#:base (Option DotNodeConfig)]
                       DotNodeConfig))
(define (dot-node-config #:shape [shape #f]
                         #:style [style #f]
                         #:color [color #f]
                         #:fillcolor [fillcolor #f]
                         #:base [base #f])
  (if base
      (node-config (or shape (node-config-shape base))
                   (or style (node-config-style base))
                   (or color (node-config-color base))
                   (or fillcolor (node-config-fillcolor base)))
      (node-config (or shape "ellipse")
                   (or style '())
                   (or color "black")
                   (or fillcolor "white"))))

(struct edge-config ([arrowhead : (U String)]
                     [arrowtail : (U String)]
                     [style : (Listof String)]
                     [color : String]
                     [minlen : Natural])
  #:type-name DotEdgeConfig)

(: dot-edge-config (-> [#:arrowhead (Option String)]
                       [#:arrowtail (Option String)]
                       [#:style (Option (Listof String))]
                       [#:color (Option String)]
                       [#:minlen (Option Natural)]
                       [#:base (Option DotEdgeConfig)]
                       DotEdgeConfig))
(define (dot-edge-config #:arrowhead [arrowhead #f]
                         #:arrowtail [arrowtail #f]
                         #:style [style #f]
                         #:color [color #f]
                         #:minlen [minlen #f]
                         #:base [base #f])
  (if base
      (edge-config (or arrowhead (edge-config-arrowhead base))
                   (or arrowtail (edge-config-arrowtail base))
                   (or style (edge-config-style base))
                   (or color (edge-config-color base))
                   (or minlen (edge-config-minlen base)))
      (edge-config (or arrowhead "normal")
                   (or arrowtail "normal")
                   (or style '())
                   (or color "black")
                   (or minlen 1))))

(: current-dot-node-config (Parameterof (-> Symbol DotNodeStatus DotNodeConfig)))
(define current-dot-node-config
  (make-parameter
   (lambda (_t [s : DotNodeStatus])
     (case s
       [(default) (dot-node-config #:shape "box" #:style '("filled" "rounded"))]
       [(visited) (dot-node-config #:shape "box" #:style '("filled" "rounded")
                                   #:fillcolor "gray")]
       [(current) (dot-node-config #:shape "box" #:style '("filled" "rounded")
                                   #:fillcolor "yellow")]))))

(: current-dot-edge-node-config (Parameterof (-> EdgeMode Symbol Symbol DotEdgeStatus DotNodeConfig)))
(define current-dot-edge-node-config
  (make-parameter (lambda (_mode _from-type _to-type _s)
                    (dot-node-config #:shape "plaintext"))))

(: current-dot-edge-config (Parameterof (-> EdgeMode Symbol Symbol DotEdgeStatus DotEdgeConfig)))
(define current-dot-edge-config
  (make-parameter (lambda ([mode : EdgeMode] _ft _tt [s : DotEdgeStatus])
                    (case mode
                      [(auto) (case s
                                [(default) (dot-edge-config #:color "red")]
                                [(visited) (dot-edge-config #:color "orange")])]
                      [(choose) (case s
                                  [(default) (dot-edge-config #:color "blue")]
                                  [(visited) (dot-edge-config #:color "cyan")])]
                      [(annotation) (dot-edge-config #:style '("dashed") #:color "black")]))))

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

(: dot-writer (All (S) (-> (Listof (Graph S)) (Node S)
                             [#:config DotConfig]
                             [#:history (History S)]
                             DotWriter)))
(define (dot-writer gs node
                    #:config [config (dot-config)]
                    #:history [h '()])
  (%dot-writer
   (lambda ([port : Output-Port])
     (%write-dot gs node #:config config #:history h #:port port))))

(: %write-dot (All (S) (-> (Listof (Graph S)) (Node S)
                             #:config DotConfig
                             #:port Output-Port
                             #:history (History S)
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
                             (%dot-config-global config))))
               (dot-string (number->string
                            (global-config-dpi (%dot-config-global config)))))
      (fprintf port "  fontname=~a\n"
               (dot-string (global-config-fontname (%dot-config-global config))))
      (fprintf port "  fontsize=~a\n"
               (dot-string (number->string
                            (global-config-fontsize (%dot-config-global config)))))

      (: display-visnodes (-> (Nested-Graphs S) Void))
      (define (display-visnodes g)
        (fprintf port "subgraph ~a {\n" (dot-string (string-append
                                                     "cluster_"
                                                     (symbol->string (graph-id (car g))))))
        (fprintf port "  label = ~a\n" (dot-string (graph-name (car g))))

        (for-each (lambda ([v : (VisNode S)])
                    (when (symbol=? (graph-id (car g)) (graph-id (cadr v)))
                      (define get-id (inst visnode-id S))
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
                                   ((%dot-config-node config) (node-type (caddr v))
                                                              (dot-node-status (caddr v)))))]
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
                                   ((%dot-config-edge-node config) (edge-mode (caddr v))
                                                                   (node-type (edge-from (caddr v)))
                                                                   (node-type (edge-to (caddr v)))
                                                                   (dot-edge-status (caddr v))
)))])))
                  visnodes)
        (for-each display-visnodes (cdr g))
        (displayln "}" port))
      (for-each display-visnodes (graphs->nested (visnodes->graphs visnodes)))
      (newline port)
      (for-each (lambda ([v : (VisNode-Edge S)])
                  (: apply-half (-> DotEdgeConfig DotEdgeConfig))
                  (define (apply-half c)
                    (if (edge-half? (caddr v))
                        c
                        (struct-copy edge-config c
                                     [arrowhead "none"])))
                  (: apply-minlen (-> DotEdgeConfig DotEdgeConfig))
                  (define (apply-minlen c)
                    (if (edge-dot-minlen (caddr v))
                        (struct-copy edge-config c [minlen (edge-dot-minlen (caddr v))])
                        c))
                  (fprintf port "  ~a -> ~a ~a\n"
                           (dot-string (symbol->string (node-id (edge-from (caddr v)))))
                           (dot-string (symbol->string (edge-id (caddr v))))
                           (format-edge-attributes
                            (show-priority (edge-priority (caddr v)))
                            (apply-half
                             (apply-minlen
                              ((%dot-config-edge config) (edge-mode (caddr v))
                                                         (node-type (edge-from (caddr v)))
                                                         (node-type (edge-to (caddr v)))
                                                         (dot-edge-status (caddr v)))))))
                  (unless (edge-half? (caddr v))
                    (fprintf port "  ~a -> ~a ~a\n"
                             (dot-string (symbol->string (edge-id (caddr v))))
                             (dot-string (symbol->string (node-id (edge-to (caddr v)))))
                             (format-edge-attributes
                              ""
                              (struct-copy edge-config
                                           (apply-minlen
                                            ((%dot-config-edge config) (edge-mode (caddr v))
                                                                       (node-type (edge-from (caddr v)))
                                                                       (node-type (edge-to (caddr v)))
                                                                       (dot-edge-status (caddr v))))
                                           [arrowtail "none"])))))
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

(: format-node-attributes (-> String DotNodeConfig String))
(define (format-node-attributes label nc)
  (format "[label=~a,shape=~a,style=~a,color=~a,fillcolor=~a]"
          (dot-string label)
          (dot-string (node-config-shape nc))
          (dot-string (string-join (node-config-style nc) ","))
          (dot-string (node-config-color nc))
          (dot-string (node-config-fillcolor nc))))

(: arrow-shape->string (-> String String))
(define (arrow-shape->string s)
  (if (symbol? s)
      (symbol->string s)
      s))

(: format-edge-attributes (-> String DotEdgeConfig String))
(define (format-edge-attributes label ec)
  (format "[label=~a,arrowhead=~a,arrowtail=~a,style=~a,color=~a,minlen=~a]"
          (dot-string label)
          (dot-string (edge-config-arrowhead ec))
          (dot-string (edge-config-arrowtail ec))
          (dot-string (string-join (edge-config-style ec) ","))
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

(: history->visited-ids (All (S) (-> (History S)  (Setof Symbol))))
(define (history->visited-ids h)
  (list->set (map (lambda ([r : (History-Record S)])
                    (case (car r)
                      [(node) (node-id (history-record-node r))]
                      [(auto choose) (edge-id (history-record-edge r))]))
                  h)))

(: history->current-node-id (All (S) (-> (History S) (Option Symbol))))
(define (history->current-node-id h)
  (and (pair? h)
       (eq? (caar h) 'node)
       (node-id (history-record-node (car h)))))

(: current-visited-ids (Parameterof (Setof Symbol)))
(define current-visited-ids (make-parameter ((inst set Symbol))))

(: current-node-id (Parameterof (Option Symbol)))
(define current-node-id (make-parameter #f))

(: dot-visited-node? (All (S) (-> (Node S) Boolean)))
(define (dot-visited-node? n)
  (set-member? (current-visited-ids) (node-id n)))

(: dot-visited-edge? (All (S) (-> (Edge S) Boolean)))
(define (dot-visited-edge? e)
  (set-member? (current-visited-ids) (edge-id e)))

(: dot-current-node? (All (S) (-> (Node S) Boolean)))
(define (dot-current-node? n)
  (cond [(current-node-id) => (lambda ([id : Symbol]) (eq? id (node-id n)))]
        [else #f]))
