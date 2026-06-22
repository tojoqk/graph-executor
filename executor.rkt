#lang typed/racket

(require "graph.rkt")

(provide find-graph next-edges auto-choose step repl-choose repl-run)

(: find-graph (-> (Listof AnyGraph) Symbol AnyGraph))
(define (find-graph gs g-id)
  (cond [(memf (lambda ([g : AnyGraph]) (equal? (graph-id g) g-id)) gs) => car]
        [else (error "Graph not found" gs g-id)]))

(: next-edges (-> (Listof AnyGraph)
                  Any
                  AnyNode
                  (U (List 'auto (Pairof AnyEdge (Listof AnyEdge)))
                     (List 'choose (Pairof AnyEdge (Listof AnyEdge)))
                     (List 'terminated))))
(define (next-edges gs st n)
  (cond [(find-graph gs (node-graph-id n))
         => (lambda ([g : AnyGraph])
              (let* ([es (edge-sort (filter-state st (filter-node n (graph-all-edges g))))]
                     [aes (auto-edges es)])
                (if (null? aes)
                    (if (null? es)
                        (list 'terminated)
                        (list 'choose es))
                    (list 'auto aes))))]
        [else (error "Graph not found" (node-graph-id n))]))

(: auto-choose (-> (List 'auto (Pairof AnyEdge (Listof AnyEdge))) AnyEdge))
(define (auto-choose ne)
  (let* ([edges (cadr ne)]
         [s (sum-weight edges)]
         [r (random s)])
    (let loop ([edges edges]
               [r r])
      (let ([fst (car edges)]
            [rst (cdr edges)])
        (cond [(< r (edge-weight fst)) fst]
              [(null? rst) (error "auto-choose: unreachble")]
              [else (loop rst (- r (edge-weight fst)))])))))

(: repl-choose (-> (List 'choose (Pairof AnyEdge (Listof AnyEdge))) AnyEdge))
(define (repl-choose ne)
  (let ([es (cadr ne)])
    (displayln "choose:")
    (for ([e : AnyEdge es]
          [i (in-naturals 0)])
      (display "  ")
      (display i)
      (display ": ")
      (displayln (edge-name e)))
    (display "? ")
    (let ([n (read)])
      (if (and (natural? n)
               (< n (length es)))
          (list-ref es n)
          (repl-choose ne)))))

(: step (-> Any AnyEdge (values Any AnyNode)))
(define (step st e)
  (let* ([n (edge-cod e)]
         [p1 (trans-proc (edge-trans e))]
         [p2 (trans-proc (node-trans n))])
    (values (p2 (p1 st)) n)))

(: repl-run (-> (Listof AnyGraph) Any AnyNode (values Any AnyNode)))
(define (repl-run gs st n)
  (let ([g (find-graph gs (node-graph-id n))])
    (displayln (format "--- Current Node: ~a (Graph: ~a) ---"
                       (node-name n)
                       (graph-name g)))
    (let ([ne (next-edges gs st n)])
      (case (car ne)
        [(terminated)
         (displayln ">> Terminated")
         (values st n)]
        [(auto)
         (let ([chosen-edge (auto-choose ne)])
           (displayln (format ">> [Auto] ~a" (edge-name chosen-edge)))
           (let-values ([(next-st next-node) (step st chosen-edge)])
             (repl-run gs next-st next-node)))]
        [(choose)
         (let ([chosen-edge (repl-choose ne)])
           (let-values ([(next-st next-node) (step st chosen-edge)])
             (repl-run gs next-st next-node)))]))))

;; --- private ---

(define-type AnyGraph (Graph Any Any))
(define-type AnyEdge (Edge Any Any Any Any))
(define-type AnyNode (Node Any Any))

(: edge-sort (-> (Listof AnyEdge) (Listof AnyEdge)))
(define (edge-sort es)
  ((inst sort AnyEdge Integer) es > #:key edge-priority))

(: group-by-priority (-> (Listof AnyEdge) (Listof (Listof AnyEdge))))
(define (group-by-priority es)
  ((inst group-by AnyEdge Integer) edge-priority es))

(: filter-state (-> Any (Listof AnyEdge) (Listof AnyEdge)))
(define (filter-state st es)
  (filter (lambda ([e : AnyEdge])
            (let* ([c (edge-when e)]
                   [proc (condition-proc c)])
              (proc st)))
          es))

(: filter-auto (-> (Listof AnyEdge) (Listof AnyEdge)))
(define (filter-auto es)
  (filter (lambda ([e : AnyEdge]) (eq? (edge-mode e) 'auto))
          es))

(: auto-edges (-> (Listof AnyEdge) (Listof AnyEdge)))
(define (auto-edges es)
  (let loop ([ess : (Listof (Listof AnyEdge)) (group-by-priority es)])
    (if (null? ess)
        '()
        (let ([auto-es (filter-auto (car ess))])
          (if (null? auto-es)
              (loop (cdr ess))
              auto-es)))))

(: sum-weight (-> (Pairof AnyEdge (Listof AnyEdge)) Positive-Integer))
(define (sum-weight es)
  (define any-edge-foldl (inst foldl AnyEdge Exact-Positive-Integer))
  (any-edge-foldl (lambda ([e1 : AnyEdge] [acc : Exact-Positive-Integer])
                    (+ (edge-weight e1)
                       acc))
                  (edge-weight (car es))
                  (cdr es)))

(: graph-all-edges (-> AnyGraph (Listof AnyEdge)))
(define (graph-all-edges g)
  (append (graph-edges g) (graph-bridges g)))

(: filter-node (-> AnyNode (Listof AnyEdge) (Listof AnyEdge)))
(define (filter-node n es)
  (filter (lambda ([e : AnyEdge])
            (eq? (node-id n) (node-id (edge-dom e))))
          es))
