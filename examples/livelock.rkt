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

(: triangle-graph (-> String (Values (Graph Null) (Node Null))))
(define (triangle-graph graph-name)
  (define g-node (inst (node graph-name) Null 'node))
  (define g-edge (inst dot-edge Null))
  (define g-graph (inst graph Null))

  (define a (g-node "A" #:type 'node))
  (define b (g-node "B" #:type 'node))
  (define c (g-node "C" #:type 'node))

  (values
   (g-graph graph-name
            #:edges (list (g-edge "A->B" #:from a #:to b)
                          (g-edge "B->C" #:from b #:to c)
                          (g-edge "C->A" #:from c #:to a)))
   a))

(: ρ-graph (-> String (Values (Graph Null) (Node Null))))
(define (ρ-graph graph-name)
  (define g-node (inst (node graph-name) Null (U 'node 'terminal)))
  (define g-edge (inst dot-edge Null))
  (define g-graph (inst graph Null))

  (define a (g-node "A" #:type 'node))
  (define b (g-node "B" #:type 'node))
  (define c (g-node "C" #:type 'node))
  (define d (g-node "D" #:type 'node))
  (define e (g-node "E" #:type 'terminal))

  (values
   (g-graph graph-name
            #:edges (list (g-edge "A->B" #:from a #:to b)
                          (g-edge "A->E" #:from a #:to e)
                          (g-edge "B->C" #:from b #:to c)
                          (g-edge "C->D" #:from c #:to d)
                          (g-edge "D->B" #:from d #:to b)))
   a))


(module+ model
  (provide make-id-model
           make-triangle-model
           make-ρ-model)

  (: make-id-model (-> (Model Null)))
  (define (make-id-model)
    (model
     (thunk
      (define-values (graph node-init) (id-graph "ID"))
      (define graphs (list graph))
      (values graphs node-init '()))))

  (: make-triangle-model (-> (Model Null)))
  (define (make-triangle-model)
    (model
     (thunk
      (define-values (graph node-init) (triangle-graph "TRIANGLE"))
      (define graphs (list graph))
      (values graphs node-init '()))))

  (: make-ρ-model (-> (Model Null)))
  (define (make-ρ-model)
    (model
     (thunk
      (define-values (graph node-init) (ρ-graph "ρ"))
      (define graphs (list graph))
      (values graphs node-init '())))))

(module+ test
  (require typed/rackunit)
  (require (submod ".." model))

  (define id-m (make-id-model))
  (check-equal? (find-livelock id-m) '((choose ("Id"))))

  (define tri-m (make-triangle-model))
  (check-equal? (find-livelock tri-m) '((choose ("C->A")) (choose ("B->C")) (choose ("A->B"))))

  (define ρ-m (make-ρ-model))
  (: ρ-terminal-node? (-> Node-Meta Boolean))
  (define (ρ-terminal-node? n)
    (symbol=? (node-meta-type n) 'terminal))
  (check-equal? (find-livelock ρ-m) '((choose ("D->B")) (choose ("C->D")) (choose ("B->C")) (choose ("A->B"))))
  (check-false (find-deadlock ρ-m ρ-terminal-node?)))
