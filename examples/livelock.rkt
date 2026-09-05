#lang typed/racket

(require graph-executor)

(: id-graph (-> String (Values (Graph Null) (Node Null))))
(define (id-graph graph-name)
  (define node (inst (node-maker graph-name) Null 'node))
  (define edge (inst make-edge Null))
  (define graph (inst make-graph Null))

  (define n (node "Node" #:type 'node))

  (values
   (graph graph-name
          #:edges (list (edge "Id" #:from n #:to n)))
   n))

(: triangle-graph (-> String (Values (Graph Null) (Node Null))))
(define (triangle-graph graph-name)
  (define node (inst (node-maker graph-name) Null 'node))
  (define edge (inst make-edge Null))
  (define graph (inst make-graph Null))

  (define a (node "A" #:type 'node))
  (define b (node "B" #:type 'node))
  (define c (node "C" #:type 'node))

  (values
   (graph graph-name
          #:edges (list (edge "A->B" #:from a #:to b)
                        (edge "B->C" #:from b #:to c)
                        (edge "C->A" #:from c #:to a)))
   a))

(: ρ-graph (-> String (Values (Graph Null) (Node Null))))
(define (ρ-graph graph-name)
  (define node (inst (node-maker graph-name) Null (U 'node 'terminal)))
  (define edge (inst make-edge Null))
  (define graph (inst make-graph Null))

  (define a (node "A" #:type 'node))
  (define b (node "B" #:type 'node))
  (define c (node "C" #:type 'node))
  (define d (node "D" #:type 'node))
  (define e (node "E" #:type 'terminal))

  (values
   (graph graph-name
          #:edges (list (edge "A->B" #:from a #:to b)
                        (edge "A->E" #:from a #:to e)
                        (edge "B->C" #:from b #:to c)
                        (edge "C->D" #:from c #:to d)
                        (edge "D->B" #:from d #:to b)))
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
  (check-equal? (find-livelock id-m) (journal (journal-entry 'choose "Id")))

  (define tri-m (make-triangle-model))
  (check-equal? (find-livelock tri-m) (journal (journal-entry 'choose "A->B")
                                               (journal-entry 'choose "B->C")
                                               (journal-entry 'choose "C->A")))

  (define ρ-m (make-ρ-model))
  (: ρ-terminal-node? (-> Node-Info Boolean))
  (define (ρ-terminal-node? n)
    (symbol=? (node-info-type n) 'terminal))
  (check-equal? (find-livelock ρ-m) (journal (journal-entry 'choose "A->B")
                                              (journal-entry 'choose "B->C")
                                              (journal-entry 'choose "C->D")
                                              (journal-entry 'choose "D->B")))
  (check-false (find-deadlock ρ-m ρ-terminal-node?)))
