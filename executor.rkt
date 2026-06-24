#lang typed/racket

(require "graph.rkt")

(provide find-graph next-edges auto-choose step)

(: find-graph (All (T S) (-> (Listof (Graph T S)) Symbol (Option (Graph T S)))))
(define (find-graph gs g-id)
  (cond [(memf (lambda ([g : (Graph T S)]) (equal? (graph-id g) g-id)) gs) => car]
        [else #f]))

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
              (let* ([es (edge-sort (filter-state st (remove-annotation (filter-node n (graph-all-edges g)))))]
                     [aes (auto-edges es)])
                (if (null? aes)
                    (if (null? es)
                        (list 'terminated)
                        (list 'choose es))
                    (list 'auto aes))))]
        [else (list 'terminated)]))

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

(: step (All (T S) (-> S (Edge T S) (values S (Node T S)))))
(define (step st e)
  (let ([n (edge-cod e)])
    (values ((node-trans n) ((edge-trans e) st)) n)))

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
            ((edge-when e) st))
          es))

(: remove-annotation (All (T S) (-> (Listof (Edge T S)) (Listof (Edge T S)))))
(define (remove-annotation es)
  (filter (lambda ([e : (Edge T S)]) (not (eq? (edge-mode e) 'annotation)))
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
