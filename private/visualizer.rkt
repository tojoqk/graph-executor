#lang typed/racket

(require "../graph.rkt")
(provide VisNode VisNode-Node VisNode-Edge
         find-graph reachable-visnodes visnode-id visnodes-edges visnodes->graphs
         Nested-Graphs graphs->nested)

(: find-graph (All (T S) (-> (Listof (Graph T S)) Symbol (Option (Graph T S)))))
(define (find-graph gs g-id)
  (cond [(memf (lambda ([g : (Graph T S)]) (equal? (graph-id g) g-id)) gs) => car]
        [else #f]))

(: filter-dom (All (T S) (-> (Node T S) (Listof (Edge T S)) (Listof (Edge T S)))))
(define (filter-dom n es)
  (filter (lambda ([e : (Edge T S)])
            (eq? (node-id n) (node-id (edge-dom e))))
          es))

(define-type (VisNode-Node T S) (List 'node (Graph T S) (Node T S)))
(define-type (VisNode-Edge T S) (List 'edge (Graph T S) (Edge T S)))
(define-type (VisNode T S) (U (VisNode-Node T S) (VisNode-Edge T S)))

(: node->visnode (All (T S) (-> (Graph T S) (-> (Node T S) (VisNode-Node T S)))))
(define ((node->visnode g) n)
  (list 'node g n))

(: edge->visnode (All (T S) (-> (Graph T S) (-> (Edge T S) (VisNode-Edge T S)))))
(define ((edge->visnode g) e)
  (list 'edge g e))

(: visnode-id (All (T S) (-> (VisNode T S) Symbol)))
(define (visnode-id v)
  (cond
    [(eq? (car v) 'node)
     (node-id (caddr v))]
    [(eq? (car v) 'edge)
     (edge-id (caddr v))]
    [(eq? (car v) 'bridge)
     (edge-id (caddr v))]))

(: visnode-graph (All (T S) (-> (VisNode T S) (Option (Graph T S)))))
(define (visnode-graph v)
  (cadr v))

(: visnodes-edges (All (T S)
                       (-> (Listof (VisNode T S)) (Listof (VisNode-Edge T S)))))
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
                  (let* ([visnodes (append (list ((node->visnode g) n))
                                           ((inst map (VisNode T S) (Edge T S)) (edge->visnode g) edges)
                                           ((inst map (VisNode T S) (Edge T S)) (edge->visnode g) bridges))]
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

(: visnodes->graphs (All (T S) (-> (Listof (VisNode T S)) (Listof (Graph T S)))))
(define (visnodes->graphs vs)
  (let loop ([vs vs] [gs : (Listof (Graph T S)) '()])
    (if (null? vs)
        gs
        (cond [(visnode-graph (car vs))
               => (lambda ([g : (Graph T S)])
                    (cond [(memf (lambda ([h : (Graph T S)])
                                   (symbol=? (graph-id g) (graph-id h)))
                                 gs)
                           (loop (cdr vs) gs)]
                          [else (loop (cdr vs) (cons g gs))]))]
              [else (loop (cdr vs) gs)]))))

(define-type (Nested-Graphs T S) (Pairof (Graph T S) (Listof (Nested-Graphs T S))))

(: graphs->nested (All (T S) (-> (Listof (Graph T S)) (Listof (Nested-Graphs T S)))))
(define (graphs->nested gs)
  (let ([ht : (Mutable-HashTable Symbol (Listof (Graph T S))) (make-hash)])
    (define get-parent-id (inst graph-parent-id T S T S))
    (: roots-box (Boxof (Listof (Graph T S))))
    (define roots-box (box '()))
    (for-each (lambda ([g : (Graph T S)])
                (cond [(get-parent-id g)
                       => (lambda ([parent-id : Symbol])
                            ((inst hash-set! Symbol (Listof (Graph T S)))
                             ht
                             parent-id
                             (cons g (hash-ref ht parent-id (lambda () '())))))]
                      [else (set-box! roots-box (cons g (unbox roots-box)))]))
              (reverse gs))
    (: ->nested (-> (Graph T S) (Nested-Graphs T S)))
    (define (->nested g)
      (cons g (map ->nested (or (hash-ref ht (graph-id g) #f) '()))))
    (map ->nested (unbox roots-box))))
