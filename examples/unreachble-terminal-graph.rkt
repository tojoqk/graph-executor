#lang typed/racket

(require graph-executor)

(: id-graph (-> String (Values (Graph Null) (Node Null))))
(define (id-graph graph-name)
  (define g-node (inst (node graph-name) Null 'node))
  (define g-edge (inst dot-edge Null))
  (define g-graph (inst graph Null))

  (define n (g-node "Node" #:type 'node))

  (values
   (g-graph graph-name
            #:edges (list (g-edge "Id" #:from n #:to n)))
   n))

(module+ model
  (provide make-id-model)

  (: make-id-model (-> (Model Null)))
  (define (make-id-model)
    (define-values (graph node-init) (id-graph "ID"))
    (define graphs (list graph))
    (model graphs node-init '())))

(module+ test
  (require typed/rackunit)
  (require (submod ".." model))

  (define m (make-id-model))
  (check-equal? (find-livelock m) '((choose ("Id")))))
