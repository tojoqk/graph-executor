#lang typed/racket

(require "../graph.rkt")
(require "../model.rkt")
(require "../executor.rkt")
(require (except-in "../visualizer.rkt" find-graph))
(require "../journal.rkt")
(require "../history.rkt")
(require typed/xml)

(provide DotNode dot-node-name dot-node-desc dot-node-type dot-node-tags dot-node-prompt dot-node-trans
         DotEdge dot-edge-name dot-edge-desc dot-edge-from dot-edge-to dot-edge-when dot-edge-trans
         render-dot
         DotConfig dot-config
         DotNodeStatus DotEdgeStatus
         DotGlobalConfig dot-global-config
         DotNodeConfig dot-node-config
         DotEdgeConfig dot-edge-config
         default-dot-node-config
         default-dot-node-label-config
         default-dot-edge-node-config
         default-dot-edge-node-label-config
         default-dot-edge-config
         Dot-Edge-Option dot-edge-option)

(struct %dot-edge-option edge-option ([minlen : Natural])
  #:type-name Dot-Edge-Option)

(: edge-dot-minlen (All (S) (-> (Edge S) Natural)))
(define (edge-dot-minlen e)
  (let ([opts (filter %dot-edge-option? (edge-edge-options e))])
    (if (null? opts)
        1
        (%dot-edge-option-minlen (car opts)))))

(: dot-edge-option (-> [#:minlen Natural] Dot-Edge-Option))
(define (dot-edge-option #:minlen [minlen 1])
  (%dot-edge-option minlen))

(: default-dot-fontname String)
(define default-dot-fontname "Times-Roman")

(: default-dot-fontsize Positive-Integer)
(define default-dot-fontsize 14)

(: default-dot-rankdir Rankdir)
(define default-dot-rankdir 'TB)

(: default-dot-dpi Positive-Integer)
(define default-dot-dpi 96)

(struct dot-node ([name : String]
                  [desc : (Option String)]
                  [type : Symbol]
                  [tags : (Listof Symbol)]
                  [prompt : (Option Code-Expr)]
                  [trans : (Option Code-Expr)]
                  [before : (Option Code-Expr)]
                  [after : (Option Code-Expr)])
  #:transparent
  #:type-name DotNode)

(: node->dot-node (All (S) (-> (Node S) DotNode)))
(define (node->dot-node n)
  (dot-node (node-name n) (node-desc n) (node-type n) (node-tags n) (node-prompt-code-expr n) (node-trans-code-expr n) (node-before-code-expr n) (node-after-code-expr n)))

(struct dot-edge ([name : String] [desc : (Option String)] [mode : EdgeMode] [from : Symbol] [to : Symbol] [when : (Option Code-Expr)] [trans : (Option Code-Expr)] [before : (Option Code-Expr)] [after : (Option Code-Expr)])
  #:transparent
  #:type-name DotEdge)

(: edge->dot-edge (All (S) (-> (Edge S) DotEdge)))
(define (edge->dot-edge e)
  (dot-edge (edge-name e) (edge-desc e) (edge-mode e) (node-type (edge-from e)) (node-type (edge-to e)) (edge-when-code-expr e) (edge-trans-code-expr e) (edge-before-code-expr e) (edge-after-code-expr e)))

(define-type DotNodeStatus (U 'default 'visited 'current))
(: dot-node-status (All (S) (-> (Node S) DotNodeStatus)))
(define (dot-node-status n)
  (cond [(dot-current-node? n) 'current]
        [(dot-visited-node? n) 'visited]
        [else 'default]))

(define-type DotEdgeStatus (U 'default 'visited))
(: dot-edge-status (All (S) (-> (Edge S) DotEdgeStatus)))
(define (dot-edge-status n)
  (cond [(dot-visited-edge? n) 'visited]
        [else 'default]))

(: default-dot-node-config (-> DotNode DotNodeStatus DotNodeConfig))
(define default-dot-node-config
  (lambda (_ [s : DotNodeStatus])
    (case s
      [(default) (dot-node-config #:shape "box" #:style '("filled" "rounded"))]
      [(visited) (dot-node-config #:shape "box" #:style '("filled" "rounded")
                                  #:fillcolor "gray")]
      [(current) (dot-node-config #:shape "box" #:style '("filled" "rounded")
                                  #:fillcolor "yellow")])))

(: default-dot-edge-node-config (-> DotEdge DotEdgeStatus DotNodeConfig))
(define default-dot-edge-node-config
  (lambda (_e _s)
    (dot-node-config #:shape "plaintext")))

(: default-dot-edge-config (-> DotEdge DotEdgeStatus DotEdgeConfig))
(define default-dot-edge-config
  (lambda ([e : DotEdge] [s : DotEdgeStatus])
    (let ([mode (dot-edge-mode e)])
      (case mode
        [(auto) (case s
                  [(default) (dot-edge-config #:color "red")]
                  [(visited) (dot-edge-config #:color "orange")])]
        [(choose) (case s
                    [(default) (dot-edge-config #:color "blue")]
                    [(visited) (dot-edge-config #:color "cyan")])]
        [(annotation) (dot-edge-config #:style '("dashed") #:color "black")]))))

(: default-dot-node-label-config (-> DotNode DotNodeStatus (U (List 'text String)
                                                              (Pairof 'html (Listof XExpr)))))
(define default-dot-node-label-config
  (lambda ([dn : DotNode] _)
    (: center-row (-> (Listof XExpr) XExpr))
    (define (center-row contents)
      `(tr (td ((align "center")) ,@contents)))
    (: left-row (-> (Listof XExpr) XExpr))
    (define (left-row contents)
      `(tr (td ((align "left")) ,@contents)))
    (list 'html
          `(table ((border "0") (cellborder "0") (cellspacing "0") (cellpadding "4"))
                  ,(center-row (list `(b ,(dot-node-name dn))))
                  ,@(cond [(dot-node-desc dn)
                           => (lambda (d) (list (left-row (text->xexprs (show-desc d)))))]
                          [else '()])
                  ,@(cond [(dot-node-trans dn)
                           => (lambda (x)
                                (list (center-row
                                       `((b "trans")
                                         (br) (font ((point-size "4")) (br))))
                                      (left-row (text->xexprs (show-code-expr x)))))]
                          [else '()])))))

(: default-dot-edge-node-label-config (-> DotEdge DotEdgeStatus (U (List 'text String)
                                                                   (Pairof 'html (Listof XExpr)))))
(define default-dot-edge-node-label-config
  (lambda ([de : DotEdge] _)
    (: center-row (-> (Listof XExpr) XExpr))
    (define (center-row contents)
      `(tr (td ((align "center")) ,@contents)))
    (: left-row (-> (Listof XExpr) XExpr))
    (define (left-row contents)
      `(tr (td ((align "left")) ,@contents)))
    (list 'html
          `(table ((border "0") (cellborder "0") (cellspacing "0") (cellpadding "1"))
                  ,(center-row (list `(b ,(dot-edge-name de))))
                  ,@(cond [(dot-edge-desc de)
                           => (lambda (d) (list (left-row (text->xexprs (show-desc d)))))]
                          [else '()])
                  ,@(cond [(dot-edge-when de)
                           => (lambda (x)
                                (list (center-row
                                       `((b "when")
                                         (br) (font ((point-size "4")) (br))))
                                      (left-row (text->xexprs (show-code-expr x)))))]
                          [else '()])
                  ,@(cond [(dot-edge-trans de)
                           => (lambda (x)
                                (list (center-row
                                       `((b "trans")
                                         (br) (font ((point-size "4")) (br))))
                                      (left-row (text->xexprs (show-code-expr x)))))]
                          [else '()])))))

(struct %dot-config ([global : DotGlobalConfig]
                     [node : (-> DotNode DotNodeStatus DotNodeConfig)]
                     [node-label : (-> DotNode DotNodeStatus (U (List 'text String)
                                                                (Pairof 'html (Listof XExpr))))]
                     [edge-node : (-> DotEdge DotEdgeStatus DotNodeConfig)]
                     [edge-node-label : (-> DotEdge DotEdgeStatus (U (List 'text String)
                                                                     (Pairof 'html (Listof XExpr))))]
                     [edge : (-> DotEdge DotEdgeStatus DotEdgeConfig)])
  #:type-name DotConfig)

(: dot-config (-> [#:global (Option DotGlobalConfig)]
                  [#:node (Option (-> DotNode DotNodeStatus DotNodeConfig))]
                  [#:node-label (Option (-> DotNode DotNodeStatus (U (List 'text String)
                                                                     (Pairof 'html (Listof XExpr)))))]
                  [#:edge-node (Option (-> DotEdge DotEdgeStatus DotNodeConfig))]
                  [#:edge-node-label (Option (-> DotEdge DotEdgeStatus (U (List 'text String)
                                                                          (Pairof 'html (Listof XExpr)))))]
                  [#:edge (Option (-> DotEdge DotEdgeStatus DotEdgeConfig))]
                  DotConfig))
(define (dot-config #:global [global #f]
                    #:node [node #f]
                    #:node-label [node-label #f]
                    #:edge-node [edge-node #f]
                    #:edge-node-label [edge-node-label #f]
                    #:edge [edge #f])
  (%dot-config (or global (dot-global-config))
               (or node default-dot-node-config)
               (or node-label default-dot-node-label-config)
               (or edge-node default-dot-edge-node-config)
               (or edge-node-label default-dot-edge-node-label-config)
               (or edge default-dot-edge-config)))

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
  (global-config (or fontname default-dot-fontname)
                 (or fontsize default-dot-fontsize)
                 (or rankdir default-dot-rankdir)
                 (or dpi default-dot-dpi)))

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

(: text->xexprs (-> String (Listof XExpr)))
(define (text->xexprs str)
  (let rec ([lines (regexp-split #rx"\n" str)])
    (cond [(null? lines) '()]
          [(null? (rest lines)) lines]
          [else (list* (first lines)
                       '(br ((align "left")))
                       '(font ((point-size "4")) (br ((align "left"))))
                       (rec (rest lines)))])))

(: show-desc (-> String String))
(define (show-desc desc)
  (string-append desc "\n"))

(: show-code-expr (-> Code-Expr String))
(define (show-code-expr ce)
  (cond [(code-sexp? ce)
         (let* ([x (code-sexp-sexp ce)]
                [out (open-output-string)])
           (pretty-print x out 1)
           (get-output-string out))]
        [(code-text? ce)
         (let ([out (open-output-string)])
           (displayln (code-text-text ce) out)
           (get-output-string out))]))

(: render-dot (All (S) (-> (Model S)  [#:journal (Listof Journal-Entry)] [#:port Output-Port] [#:config DotConfig] Void)))
(define (render-dot m #:journal [j '()] #:port [port (current-output-port)] #:config [config (dot-config)])
  (define-values (_n _s h) (replay m j))
  (%render-dot (model-graphs m) (model-node m) #:config config #:history h #:port port))

(: %render-dot (All (S) (-> (Listof (Graph S)) (Node S)
                            #:config DotConfig
                            #:port Output-Port
                            #:history (History S)
                            Void)))
(define (%render-dot gs node
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
                                  (format-node-extra
                                   ((%dot-config-node-label config) (node->dot-node (caddr v))
                                                                    (dot-node-status (caddr v)))
                                   ((%dot-config-node config) (node->dot-node (caddr v))
                                                              (dot-node-status (caddr v)))))]
                        [(eq? 'edge (car v))
                         (fprintf port "  ~a ~a\n"
                                  (dot-string (symbol->string (get-id v)))
                                  (format-node-extra
                                   ((%dot-config-edge-node-label config) (edge->dot-edge (caddr v))
                                                                         (dot-edge-status (caddr v)))
                                   ((%dot-config-edge-node config) (edge->dot-edge (caddr v))
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
                           (format-edge-extra
                            (show-priority (edge-priority (caddr v)))
                            (apply-half
                             (apply-minlen
                              ((%dot-config-edge config) (edge->dot-edge (caddr v))
                                                         (dot-edge-status (caddr v)))))))
                  (unless (edge-half? (caddr v))
                    (fprintf port "  ~a -> ~a ~a\n"
                             (dot-string (symbol->string (edge-id (caddr v))))
                             (dot-string (symbol->string (node-id (edge-to (caddr v)))))
                             (format-edge-extra
                              ""
                              (struct-copy edge-config
                                           (apply-minlen
                                            ((%dot-config-edge config) (edge->dot-edge (caddr v))
                                                                       (dot-edge-status (caddr v))))
                                           [arrowtail "none"])))))
                (visnodes-edges visnodes))
      (displayln "}" port))))

(: byte->hex-string (-> Byte String))
(define (byte->hex-string b)
  (if (<= b 15)
      (format "0~x" b)
      (format "~x" b)))

(: format-node-extra (-> (U (List 'text String) (Pairof 'html (Listof XExpr))) DotNodeConfig String))
(define (format-node-extra label nc)
  (format "[label=~a,shape=~a,style=~a,color=~a,fillcolor=~a]"
          (ann (case (first label)
                 [(text) (dot-string (second label))]
                 [(html) (format "<~a>"
                                 (string-join (map xexpr->string (rest label)) ""))])
               String)
          (dot-string (node-config-shape nc))
          (dot-string (string-join (node-config-style nc) ","))
          (dot-string (node-config-color nc))
          (dot-string (node-config-fillcolor nc))))

(: arrow-shape->string (-> String String))
(define (arrow-shape->string s)
  (if (symbol? s)
      (symbol->string s)
      s))

(: format-edge-extra (-> String DotEdgeConfig String))
(define (format-edge-extra label ec)
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
  (list->set (map (lambda ([r : (Record S)])
                    (case (car r)
                      [(node) (node-id (record-node r))]
                      [(auto choose) (edge-id (record-edge r))]))
                  h)))

(: history->current-node-id (All (S) (-> (History S) (Option Symbol))))
(define (history->current-node-id h)
  (and (pair? h)
       (eq? (caar h) 'node)
       (node-id (record-node (car h)))))

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
