#lang typed/racket

(require "graph.rkt")
(provide VisNode VisNode-Node VisNode-Edge
         find-graph reachable-visnodes visnode-id visnodes-edges visnodes->graphs
         Nested-Graphs graphs->nested)

(: find-graph (All (S) (-> (Listof (Graph S)) Symbol (Option (Graph S)))))
(define (find-graph gs g-id)
  (cond [(memf (lambda ([g : (Graph S)]) (equal? (graph-id g) g-id)) gs) => car]
        [else #f]))

(: filter-from (All (S) (-> (Node S) (Listof (Edge S)) (Listof (Edge S)))))
(define (filter-from n es)
  (filter (lambda ([e : (Edge S)])
            (eq? (node-id n) (node-id (edge-from e))))
          es))

(define-type (VisNode-Node S) (List 'node (Graph S) (Node S)))
(define-type (VisNode-Edge S) (List 'edge (Graph S) (Edge S)))
(define-type (VisNode S) (U (VisNode-Node S) (VisNode-Edge S)))

(: node->visnode (All (S) (-> (Graph S) (-> (Node S) (VisNode-Node S)))))
(define ((node->visnode g) n)
  (list 'node g n))

(: edge->visnode (All (S) (-> (Graph S) (-> (Edge S) (VisNode-Edge S)))))
(define ((edge->visnode g) e)
  (list 'edge g e))

(: visnode-id (All (S) (-> (VisNode S) Symbol)))
(define (visnode-id v)
  (cond
    [(eq? (car v) 'node)
     (node-id (caddr v))]
    [(eq? (car v) 'edge)
     (edge-id (caddr v))]))

(: visnode-graph (All (S) (-> (VisNode S) (Option (Graph S)))))
(define (visnode-graph v)
  (cadr v))

(: visnodes-edges (All (S)
                       (-> (Listof (VisNode S)) (Listof (VisNode-Edge S)))))
(define (visnodes-edges visnodes)
  (if (null? visnodes)
      '()
      (let ([visnode (car visnodes)])
        (cond
          [(eq? (car visnode) 'node)
           (visnodes-edges (cdr visnodes))]
          [(eq? (car visnode) 'edge)
           (cons visnode
                 (visnodes-edges (cdr visnodes)))]))))

(: reachable-visnodes (All (S) (-> (Listof (Graph S)) (Node S) (Listof (VisNode S)))))
(define (reachable-visnodes gs n)
  (: loop (-> (Node S) (Setof Symbol) (Values (Listof (VisNode S)) (Setof Symbol))))
  (define (loop n seen)
    (cond [(set-member? seen (node-id n)) (values '() seen)]
          [(find-graph gs (node-graph-id n))
           => (lambda ([g : (Graph S)])
                (let ([edges (filter-from n (graph-edges g))])
                  (let* ([visnodes (append (list ((node->visnode g) n))
                                           ((inst map (VisNode S) (Edge S)) (edge->visnode g) edges))]
                         [seen (set-union seen (list->set ((inst map Symbol (VisNode S)) visnode-id visnodes)))])
                    (for/fold : (Values (Listof (VisNode S)) (Setof Symbol))
                              ([visnodes visnodes]
                               [seen seen])
                              ([edge edges])
                      (: new-visnodes (Listof (VisNode S)))
                      (: new-seen (Setof Symbol))
                      (define-values (new-visnodes new-seen)
                        (loop (edge-to edge) seen))
                      (values (append visnodes new-visnodes) new-seen)))))]
          [else (values '() (set-add seen (node-id n)))]))
  (define-values (visnodes _)
    (loop n (set)))
  visnodes)

(: visnodes->graphs (All (S) (-> (Listof (VisNode S)) (Listof (Graph S)))))
(define (visnodes->graphs vs)
  (let loop ([vs vs] [gs : (Listof (Graph S)) '()])
    (if (null? vs)
        gs
        (cond [(visnode-graph (car vs))
               => (lambda ([g : (Graph S)])
                    (cond [(memf (lambda ([h : (Graph S)])
                                   (symbol=? (graph-id g) (graph-id h)))
                                 gs)
                           (loop (cdr vs) gs)]
                          [else (loop (cdr vs) (cons g gs))]))]
              [else (loop (cdr vs) gs)]))))

(define-type (Nested-Graphs S) (Pairof (Graph S) (Listof (Nested-Graphs S))))

(: graphs->nested (All (S) (-> (Listof (Graph S)) (Listof (Nested-Graphs S)))))
(define (graphs->nested gs)
  (let ([ht : (Mutable-HashTable Symbol (Listof (Graph S))) (make-hash)])
    (define get-parent-id (inst graph-parent-id S))
    (: roots-box (Boxof (Listof (Graph S))))
    (define roots-box (box '()))
    (for-each (lambda ([g : (Graph S)])
                (cond [(get-parent-id g)
                       => (lambda ([parent-id : Symbol])
                            ((inst hash-set! Symbol (Listof (Graph S)))
                             ht
                             parent-id
                             (cons g (hash-ref ht parent-id (lambda () '())))))]
                      [else (set-box! roots-box (cons g (unbox roots-box)))]))
              (reverse gs))
    (: ->nested (-> (Graph S) (Nested-Graphs S)))
    (define (->nested g)
      (cons g (map ->nested (or (hash-ref ht (graph-id g) #f) '()))))
    (map ->nested (unbox roots-box))))
