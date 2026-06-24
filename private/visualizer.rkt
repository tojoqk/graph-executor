#lang typed/racket

(require "../graph.rkt")
(provide VisNode reachable-visnodes visnode-id visnodes-edges)

(: find-graph (All (T S) (-> (Listof (Graph T S)) Symbol (Option (Graph T S)))))
(define (find-graph gs g-id)
  (cond [(memf (lambda ([g : (Graph T S)]) (equal? (graph-id g) g-id)) gs) => car]
        [else #f]))

(: filter-dom (All (T S) (-> (Node T S) (Listof (Edge T S)) (Listof (Edge T S)))))
(define (filter-dom n es)
  (filter (lambda ([e : (Edge T S)])
            (eq? (node-id n) (node-id (edge-dom e))))
          es))

(define-type (VisNode T S) (U (Pairof 'node (Node T S))
                              (Pairof 'edge (Edge T S))
                              (Pairof 'bridge (Edge T S))))

(: node->visnode (All (T S) (-> (Node T S) (Pairof 'node (Node T S)))))
(define (node->visnode n)
  (cons 'node n))

(: edge->visnode (All (T S) (-> (Edge T S) (Pairof 'edge (Edge T S)))))
(define (edge->visnode e)
  (cons 'edge e))

(: bridge->visnode (All (T S) (-> (Edge T S) (Pairof 'bridge (Edge T S)))))
(define (bridge->visnode b)
  (cons 'bridge b))

(: visnode-id (All (T S) (-> (VisNode T S) Symbol)))
(define (visnode-id v)
  (cond
    [(eq? (car v) 'node)
     (node-id (cdr v))]
    [(eq? (car v) 'edge)
     (edge-id (cdr v))]
    [(eq? (car v) 'bridge)
     (edge-id (cdr v))]))


(: visnodes-edges (All (T S)
                       (-> (Listof (VisNode T S)) (Listof (U (Pairof 'edge (Edge T S))
                                                             (Pairof 'bridge (Edge T S)))))))
(define (visnodes-edges visnodes)
  (if (null? visnodes)
      '()
      (let ([visnode (car visnodes)])
        (cond
          [(eq? (car visnode) 'node)
           (visnodes-edges (cdr visnodes))]
          [(eq? (car visnode) 'edge)
           (cons visnode
                 (visnodes-edges (cdr visnodes)))]
          [(eq? (car visnode) 'bridge)
           (cons visnode
                 (visnodes-edges (cdr visnodes)))]))))

(: reachable-visnodes (All (T S) (-> (Listof (Graph T S)) (Node T S) (Listof (VisNode T S)))))
(define (reachable-visnodes gs n)
  (: loop (-> (Node T S) (Setof Symbol) (Values (Listof (VisNode T S)) (Setof Symbol))))
  (define (loop n seen)
    (cond [(set-member? seen (node-id n)) (values '() seen)]
          [(find-graph gs (node-graph-id n))
           => (lambda ([g : (Graph T S)])
                (let ([edges (filter-dom n (graph-edges g))]
                      [bridges (filter-dom n (graph-bridges g))])
                  (let* ([visnodes (append (list (node->visnode n))
                                           ((inst map (VisNode T S) (Edge T S)) edge->visnode edges)
                                           ((inst map (VisNode T S) (Edge T S)) bridge->visnode bridges))]
                         [seen (set-union seen (list->set ((inst map Symbol (VisNode T S)) visnode-id visnodes)))])
                    (for/fold : (Values (Listof (VisNode T S)) (Setof Symbol))
                              ([visnodes visnodes]
                               [seen seen])
                              ([edge (append edges bridges)])
                      (: new-visnodes (Listof (VisNode T S)))
                      (: new-seen (Setof Symbol))
                      (define-values (new-visnodes new-seen)
                        (loop (edge-cod edge) seen))
                      (values (append visnodes new-visnodes) new-seen)))))]
          [else (values '() (set-add seen (node-id n)))]))
  (define-values (visnodes _)
    (loop n (set)))
  visnodes)
