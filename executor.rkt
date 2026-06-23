#lang typed/racket

(require "graph.rkt")

(provide find-graph next-edges auto-choose step repl-choose repl-run)

(: find-graph (All (T S) (-> (Listof (Graph T S)) Symbol (Graph T S))))
(define (find-graph gs g-id)
  (cond [(memf (lambda ([g : (Graph T S)]) (equal? (graph-id g) g-id)) gs) => car]
        [else (error "Graph not found" gs g-id)]))

(: next-edges (All (T S)
                   (-> (Listof (Graph T S))
                       S
                       (Node T S)
                       (U (List 'auto (Pairof (Edge T S) (Listof (Edge T S))))
                          (List 'choose (Pairof (Edge T S) (Listof (Edge T S))))
                          (List 'terminated)))))
(define (next-edges gs st n)
  (cond [(find-graph gs (node-graph-id n))
         => (lambda ([g : (Graph T S)])
              (let* ([es (edge-sort (filter-state st (filter-node n (graph-all-edges g))))]
                     [aes (auto-edges es)])
                (if (null? aes)
                    (if (null? es)
                        (list 'terminated)
                        (list 'choose es))
                    (list 'auto aes))))]
        [else (error "Graph not found" (node-graph-id n))]))

(: auto-choose (All (T S)
                    (-> (List 'auto (Pairof (Edge T S) (Listof (Edge T S)))) (Edge T S))))
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

(: repl-choose (All (T S)
                    (-> (List 'choose (Pairof (Edge T S) (Listof (Edge T S)))) (Edge T S))))
(define (repl-choose ne)
  (let ([es (cadr ne)])
    (displayln "choose:")
    (for ([e : (Edge T S) es]
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

(: step (All (T S) (-> S (Edge T S) (values S (Node T S)))))
(define (step st e)
  (let* ([n (edge-cod e)]
         [p1 (trans-proc (edge-trans e))]
         [p2 (trans-proc (node-trans n))])
    (values (p2 (p1 st)) n)))

(: repl-run (All (T S) (-> (Listof (Graph T S)) S (Node T S) (values S (Node T S)))))
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
(: edge-sort (All (T S) (-> (Listof (Edge T S)) (Listof (Edge T S)))))
(define (edge-sort es)
  ((inst sort (Edge T S) Integer) es > #:key edge-priority))

(: group-by-priority (All (T S) (-> (Listof (Edge T S)) (Listof (Listof (Edge T S))))))
(define (group-by-priority es)
  ((inst group-by (Edge T S) Integer) edge-priority es))

(: filter-state (All (T S) (-> S (Listof (Edge T S)) (Listof (Edge T S)))))
(define (filter-state st es)
  (filter (lambda ([e : (Edge T S)])
            (let* ([c (edge-when e)]
                   [proc (condition-proc c)])
              (proc st)))
          es))

(: filter-auto (All (T S) (-> (Listof (Edge T S)) (Listof (Edge T S)))))
(define (filter-auto es)
  (filter (lambda ([e : (Edge T S)]) (eq? (edge-mode e) 'auto))
          es))

(: auto-edges (All (T S) (-> (Listof (Edge T S)) (Listof (Edge T S)))))
(define (auto-edges es)
  (let loop ([ess : (Listof (Listof (Edge T S))) (group-by-priority es)])
    (if (null? ess)
        '()
        (let ([auto-es (filter-auto (car ess))])
          (if (null? auto-es)
              (loop (cdr ess))
              auto-es)))))

(: sum-weight (All (T S) (-> (Pairof (Edge T S) (Listof (Edge T S))) Positive-Integer)))
(define (sum-weight es)
  (define any-edge-foldl (inst foldl (Edge T S) Exact-Positive-Integer))
  (any-edge-foldl (lambda ([e1 : (Edge T S)] [acc : Exact-Positive-Integer])
                    (+ (edge-weight e1)
                       acc))
                  (edge-weight (car es))
                  (cdr es)))

(: graph-all-edges (All (T S) (-> (Graph T S) (Listof (Edge T S)))))
(define (graph-all-edges g)
  ((inst append (Edge T S)) (graph-edges g) (graph-bridges g)))

(: filter-node (All (T S) (-> (Node T S) (Listof (Edge T S)) (Listof (Edge T S)))))
(define (filter-node n es)
  (filter (lambda ([e : (Edge T S)])
            (eq? (node-id n) (node-id (edge-dom e))))
          es))
