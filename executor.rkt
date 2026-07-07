#lang typed/racket

(require "graph.rkt")
(require "history.rkt")
(require "prompt.rkt")

(provide replay
         find-graph next-edges auto-choose
         current-auto-conflict-policy current-single-choose-policy
         current-node-id current-node?
         current-edge-id current-edge?)

(: current-node-id (Parameterof (Option Symbol)))
(define current-node-id (make-parameter #f))

(: current-node? (All (T S) (-> (Node T S) Boolean)))
(define (current-node? n)
  (cond [(current-node-id) => (curry symbol=? (node-id n))]
        [else #f]))

(: current-edge-id (Parameterof (Option Symbol)))
(define current-edge-id (make-parameter #f))

(: current-edge? (All (T S) (-> (Edge T S) Boolean)))
(define (current-edge? e)
  (cond [(current-edge-id) => (curry symbol=? (edge-id e))]
        [else #f]))

(: replay (All (T S) (-> (Listof (Graph T S)) (Node T S) S Journal
                         (Values (Node T S) S))))
(define (replay gs n st j)
  (let ([ne (next-edges gs st n)])
    (if (null? j)
        (values n st)
        (case (car ne)
          [(choose auto)
           (let* ([edges (cadr ne)]
                  [j-rec (car j)]
                  [name (car j-rec)]
                  [ps-init (cdr j-rec)])
             (cond [(findf (lambda ([e : (Edge T S)]) (string=? name (edge-name e))) edges)
                    => (lambda ([e : (Edge T S)])
                         (let ([cod (edge-cod e)]
                               [bps : (Boxof (Listof Prompt-Value)) (box ps-init)])
                           (: pop-bps (Prompt Any))
                           (define (pop-bps _title op [_ (hash)])
                             (let ([ps (unbox bps)])
                               (set-box! bps (cdr ps))
                               (if (null? ps)
                                   (error 'replay "unexpected end of prompt values")
                                   (let ([p (car ps)])
                                     (case (car op)
                                       [(const) p]
                                       [(choose string) (assert p string?)]
                                       [(integer) (assert (assert p exact?) integer?)]
                                       [(natural) (assert (assert p exact?) natural?)]
                                       [(positive) (assert (assert p exact?) positive-integer?)]
                                       [(range) (if (natural? (second op))
                                                    (assert (assert p exact?) natural?)
                                                    (assert (assert p exact?) integer?))]
                                       [(random) (assert p natural?)])))))
                           (replay gs cod (parameterize ([current-prompt pop-bps])
                                            ((node-trans cod) ((edge-trans e) st)))
                                   (cdr j))))]
                   [else (error 'replay "edge not found")]))]
          [(terminated) (error 'replay "unexpected termination")]))))

(: current-auto-conflict-policy (Parameterof (U 'random 'choose)))
(define current-auto-conflict-policy (make-parameter 'random))

(: current-single-choose-policy (Parameterof (U 'skip 'choose)))
(define current-single-choose-policy (make-parameter 'skip))

(: find-graph (All (T S) (-> (Listof (Graph T S)) Symbol (Option (Graph T S)))))
(define (find-graph gs g-id)
  (cond [(memf (lambda ([g : (Graph T S)]) (equal? (graph-id g) g-id)) gs) => car]
        [else (error 'find-graph "not found" g-id)]))

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
                        (if (null? (cdr es))
                            (let ([policy (current-single-choose-policy)])
                              (cond [(eq? policy 'skip) (list 'auto es)]
                                    [(eq? policy 'choose) (list 'choose es)]))
                            (list 'choose es)))
                    (if (null? (cdr aes))
                        (list 'auto aes)
                        (let ([policy (current-auto-conflict-policy)])
                          (cond [(eq? policy 'random) (list 'auto aes)]
                                [(eq? policy 'choose) (list 'choose aes)]))))))]
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
