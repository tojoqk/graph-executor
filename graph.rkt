#lang typed/racket

(provide Graph graph?
         graph-node graph-edge graph-dom-edges graph-cod-edges
         Node node?
         node-key node-name node-attribute
         Edge edge?
         edge-key edge-name edge-dom edge-cod edge-attribute
         Attribute attribute?
         attribute-key attribute-value
         Value value?)

(module+ test
  (require typed/rackunit)

  (define example
    `((nodes ((a (node-name "node-a"))
              (b (node-name "node-b")
                 (desc "node of b"))
              (c (node-name "node-c")
                 (desc "node of c"))))
      (edges ((a->a (edge-name "a->a")
                    (dom a)
                    (cod a))
              (a->b (edge-name "a->b")
                    (dom a)
                    (cod b))
              (c->b (edge-name "c->b")
                    (dom c)
                    (cod b)
                    (desc "edge of c->b")))))))
(define-type Graph (List (List 'nodes (Listof Node))
                         (List 'edges (Listof Edge))))

(define-type Node (Pairof Symbol Node-Body))
(define-type Edge (Pairof Symbol Edge-Body))

(define-type Node-Body (List* (List 'node-name String)
                              (Listof (List Symbol Value))))
(define-type Edge-Body (List* (List 'edge-name String)
                              (List 'dom Symbol)
                              (List 'cod Symbol)
                              (Listof (List Symbol Value))))
(define-type Attribute (List Symbol Value))
(define-type Value (U Symbol Natural String Boolean))

(define-predicate graph? Graph)

(module+ test
  (check-true (graph? example)))

(define-predicate attribute? Attribute)
(define-predicate value? Value)
(define-predicate node? Node)
(define-predicate edge? Edge)

(: attribute-key (Attribute -> Symbol))
(define (attribute-key attr)
  (car attr))

(module+ test
  (check-eq? (attribute-key '(desc "hello")) 'desc))

(: attribute-value (Attribute -> Value))
(define (attribute-value attr)
  (cadr attr))

(module+ test
  (check-eq? (attribute-value '(desc "hello")) "hello"))

(: graph-nodes (Graph -> (Listof Node)))
(define (graph-nodes g)
  (cadar g))

(module+ test
  (check-equal? (graph-nodes example)
                '((a (node-name "node-a"))
                  (b (node-name "node-b") (desc "node of b"))
                  (c (node-name "node-c") (desc "node of c")))))

(: graph-node (Graph Symbol -> (Option Node)))
(define (graph-node g key)
  (cond [(assoc key (graph-nodes g)) => identity]
        [else #f]))

(module+ test
  (check-equal? (graph-node example 'b)
                '(b (node-name "node-b") (desc "node of b")))
  (check-equal? (graph-node example 'z)
                #f))

(: graph-edges (Graph -> (Listof Edge)))
(define (graph-edges g)
  (cadadr g))

(module+ test
  (check-equal? (graph-edges example)
                '((a->a (edge-name "a->a") (dom a) (cod a))
                  (a->b (edge-name "a->b") (dom a) (cod b))
                  (c->b (edge-name "c->b") (dom c) (cod b) (desc "edge of c->b")))))

(: graph-edge (Graph Symbol -> (Option Edge)))
(define (graph-edge g key)
  (cond [(assoc key (graph-edges g)) => identity]
        [else #f]))

(module+ test
  (check-equal? (graph-edge example 'a->b)
                '(a->b (edge-name "a->b") (dom a) (cod b))))

(: graph-dom-edges (Graph Symbol -> (Listof Edge)))
(define (graph-dom-edges g node-key)
  (filter (lambda ([edge : Edge])
            (symbol=? (edge-dom edge) node-key))
          (graph-edges g)))

(module+ test
  (check-equal? (graph-dom-edges example 'a)
                '((a->a (edge-name "a->a") (dom a) (cod a))
                  (a->b (edge-name "a->b") (dom a) (cod b)))))

(: graph-cod-edges (Graph Symbol -> (Listof Edge)))
(define (graph-cod-edges g node-key)
  (filter (lambda ([edge : Edge])
            (symbol=? (edge-cod edge) node-key))
          (graph-edges g)))

(module+ test
  (check-equal? (graph-cod-edges example 'b)
                '((a->b (edge-name "a->b") (dom a) (cod b))
                  (c->b (edge-name "c->b") (dom c) (cod b) (desc "edge of c->b")))))

(: node-key (Node -> Symbol))
(define (node-key node)
  (car node))

(module+ test
  (check-equal? (node-key (cast (graph-node example 'a) Node)) 'a))

(: node-name (Node -> String))
(define (node-name node)
    (let ((x : (List 'node-name String) (car (cdr node))))
      (car (cdr x))))

(module+ test
  (check-equal? (node-name (cast (graph-node example 'a) Node)) "node-a"))

(: node-attribute (Node Symbol -> (Option Attribute)))
(define (node-attribute node key)
  (cond [(assoc key (cdr node)) => identity]
        [else #f]))

(module+ test
  (check-equal? (node-attribute (cast (graph-node example 'b) Node) 'desc) '(desc "node of b"))
  (check-equal? (node-attribute (cast (graph-node example 'a) Node) 'desc) #f))

(: edge-key (Edge -> Symbol))
(define (edge-key e)
  (car e))

(module+ test
  (check-equal? (edge-key (cast (graph-edge example 'a->b) Edge)) 'a->b))

(: edge-name (Edge -> String))
(define (edge-name e)
  (let ((x : (List 'edge-name String) (car (cdr e))))
    (car (cdr x))))

(module+ test
  (check-equal? (edge-name (cast (graph-edge example 'a->b) Edge)) "a->b"))

(: edge-dom (Edge -> Symbol))
(define (edge-dom e)
  (let ((x : (List 'dom Symbol) (car (cdr (cdr e))) ))
    (car (cdr x))))

(module+ test
  (check-equal? (edge-dom (cast (graph-edge example 'a->b) Edge)) 'a))

(: edge-cod (Edge -> Symbol))
(define (edge-cod e)
  (let ((x : (List 'cod Symbol) (car (cdr (cdr (cdr e))))))
    (car (cdr x))))

(module+ test
  (check-equal? (edge-cod (cast (graph-edge example 'a->b) Edge)) 'b))

(: edge-attribute (Edge Symbol -> (Option Attribute)))
(define (edge-attribute edge key)
  (cond [(assoc key (cdr edge)) => identity]
        [else #f]))

(module+ test
  (check-equal? (edge-attribute (cast (graph-edge example 'a->a) Edge) 'desc) #f)
  (check-equal? (edge-attribute (cast (graph-edge example 'c->b) Edge) 'desc) '(desc "edge of c->b")))
