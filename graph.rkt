#lang typed/racket

(require srfi/2)

(provide Graph graph?
         graph-key graph-name
         graph-node-name graph-node-attribute
         graph-edge-name graph-edge-dom graph-edge-cod graph-edge-attribute
         Attribute attribute?
         attribute-key attribute-value
         Value value?
         find-graph dom-edges cod-edges valid-edges?)

(module+ test
  (require typed/rackunit)

  (define example
    `(example (graph-name "graph-example")
              (nodes (a (node-name "node-a"))
                     (b (node-name "node-b")
                        (desc "node of b"))
                     (c (node-name "node-c")
                        (desc "node of c")))
              (edges (a->a (edge-name "a->a")
                           (dom a)
                           (cod a))
                     (a->b (edge-name "a->b")
                           (dom a)
                           (cod b))
                     (c->b (edge-name "c->b")
                           (dom c)
                           (cod b)
                           (desc "edge of c->b"))))))

(define-type Graph (List Symbol
                         (List 'graph-name String)
                         (List* 'nodes (Listof Node))
                         (List* 'edges (Listof Edge))))

(define-type Node (List* Symbol
                         (List 'node-name String)
                         (Listof (List Symbol Value))))

(define-type Node-Ref (U Symbol (List Symbol Symbol)))

(define-type Edge (List* Symbol
                         (List 'edge-name String)
                         (List 'dom Node-Ref)
                         (List 'cod Node-Ref)
                         (Listof (List Symbol Value))))

(define-type Attribute (List Symbol Value))
(define-type Value (U Symbol Natural String Boolean Any))

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

(: graph-key (Graph -> Symbol))
(define (graph-key g)
  (car g))

(: graph-name (Graph -> String))
(define (graph-name g)
  (let ((x : (List 'graph-name String) (car (cdr g))))
    (car (cdr x))))

(: graph-nodes (Graph -> (Listof Node)))
(define (graph-nodes g)
  (cdaddr g))

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

(: graph-node-name (Graph Symbol -> (Option String)))
(define (graph-node-name g key)
  (cond [(graph-node g key) => (lambda ([node : Node])
                                 (node-name node))]
        [else #f]))

(module+ test
  (check-equal? (graph-node-name example 'b)
                "node-b")
  (check-equal? (graph-node-name example 'z)
                #f))

(: graph-node-attribute (Graph Symbol Symbol -> (Option Attribute)))
(define (graph-node-attribute g node-key key)
  (cond [(graph-node g node-key) => (lambda ([node : Node])
                                      (node-attribute node key))]
        [else #f]))

(module+ test
  (check-equal? (graph-node-attribute example 'b 'desc)
                '(desc "node of b"))
  (check-equal? (graph-node-attribute example 'a 'desc)
                #f))

(: graph-normalize-node-ref (Graph (U Symbol (List Symbol Symbol)) -> (List Symbol Symbol)))
(define (graph-normalize-node-ref graph ref)
  (if (pair? ref)
      ref
      (list (graph-key graph) ref)))

(: graph-edges (Graph -> (Listof Edge)))
(define (graph-edges g)
  (cdr (cadddr g)))

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

(: graph-edge-name (Graph Symbol -> (Option String)))
(define (graph-edge-name g edge-key)
  (cond [(graph-edge g edge-key) => edge-name]
        [else #f]))

(: graph-edge-dom (Graph Symbol -> (Option (List Symbol Symbol))))
(define (graph-edge-dom g edge-key)
  (cond [(graph-edge g edge-key)
         => (lambda ([edge : Edge])
              (graph-normalize-node-ref g (edge-dom edge)))]
        [else #f]))

(module+ test
  (check-equal? (graph-edge-dom example 'a->b) '(example a)))

(: graph-edge-cod (Graph Symbol -> (Option (List Symbol Symbol))))
(define (graph-edge-cod g edge-key)
  (cond [(graph-edge g edge-key)
         => (lambda ([edge : Edge])
              (graph-normalize-node-ref g (edge-cod edge)))]
        [else #f]))

(module+ test
  (check-equal? (graph-edge-cod example 'a->b) '(example b)))

(: graph-edge-attribute (Graph Symbol Symbol -> (Option Attribute)))
(define (graph-edge-attribute g edge-key key)
  (cond [(graph-edge g edge-key) => (lambda ([edge : Edge])
                                      (edge-attribute edge key))]
        [else #f]))

(module+ test
  (check-equal? (graph-edge-attribute example 'a->b 'desc) '#f)
  (check-equal? (graph-edge-attribute example 'c->b 'desc) '(desc "edge of c->b")))

(: find-graph ((Listof Graph) Symbol -> (Option Graph)))
(define (find-graph graphs key)
  (cond [(assoc key graphs) => identity]
        [else #f]))

(module+ test
  (check-equal? (find-graph (list example) 'example)
                example)
  (check-false (find-graph (list example) 'example-z)))


(: find-node ((Listof Graph) (List Symbol Symbol) -> (Option Node)))
(define (find-node gs ref)
  (let ([graph-key (car ref)]
        [node-key (cadr ref)])
    (cond [(find-graph gs graph-key)
           => (lambda ([g : Graph])
                (graph-node g node-key))]
          [else #f])))

(module+ test
  (check-equal? (find-node (list example) '(example b))
                '(b (node-name "node-b") (desc "node of b")))
  (check-equal? (find-node (list example) '(example z))
                #f)
  (check-equal? (find-node (list example) '(example-z b))
                #f))

(: dom-edges ((Listof Graph) (List Symbol Symbol) -> (Listof Symbol)))
(define (dom-edges gs ref)
  (append-map (lambda ([g : Graph])
                (filter-map (lambda ([edge : Edge])
                              (and (equal? (graph-normalize-node-ref g (edge-dom edge))
                                           ref)
                                   (edge-key edge)))
                            (graph-edges g)))
              gs))

(module+ test
  (check-equal? (dom-edges (list example) '(example a))
                '(a->a a->b)))

(: cod-edges ((Listof Graph) (List Symbol Symbol) -> (Listof Symbol)))
(define (cod-edges gs ref)
  (append-map (lambda ([g : Graph])
                (filter-map (lambda ([edge : Edge])
                              (and (equal? (graph-normalize-node-ref g (edge-cod edge))
                                           ref)
                                   (edge-key edge)))
                            (graph-edges g)))
              gs))

(module+ test
  (check-equal? (cod-edges (list example) '(example b))
                '(a->b c->b)))

(: valid-edges? ((Listof Graph) -> Boolean))
(define (valid-edges? gs)
  (andmap (lambda ([g : Graph])
            (andmap (lambda ([edge : Edge])
                      (let ([key (edge-key edge)])
                        (and-let* ([dom (graph-edge-dom g key)]
                                   [cod (graph-edge-cod g key)]
                                   (find-node gs dom)
                                   (find-node gs cod))
                          #t)))
                    (graph-edges g)))
          gs))

(module+ test
  (check-true (valid-edges? (list example)))
  (check-true (valid-edges? (list example '(test (graph-name "test")
                                                 (nodes)
                                                 (edges (test (edge-name "test")
                                                              (dom (example a))
                                                              (cod (example b))))))))
  (check-false (valid-edges? (list example '(test (graph-name "test")
                                                  (nodes)
                                                  (edges (test (edge-name "test")
                                                               (dom (example z))
                                                               (cod (example b))))))))
  (check-false (valid-edges? (list example '(test (graph-name "test")
                                                  (nodes)
                                                  (edges (test (edge-name "test")
                                                               (dom (example a))
                                                               (cod (example z)))))))))

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

(: edge-dom (Edge -> Node-Ref))
(define (edge-dom e)
  (let ((x : (List 'dom Node-Ref) (car (cdr (cdr e))) ))
    (car (cdr x))))

(module+ test
  (check-equal? (edge-dom (cast (graph-edge example 'a->b) Edge)) 'a))

(: edge-cod (Edge -> Node-Ref))
(define (edge-cod e)
  (let ((x : (List 'cod Node-Ref) (car (cdr (cdr (cdr e))))))
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
