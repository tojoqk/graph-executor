#lang typed/racket

(require "graph.rkt")

(provide Model (rename-out [model* model]) model-graphs model-node model-state)

(struct (S) model ([graphs : (Listof (Graph S))]
                   [node : (Node S)]
                   [state : S])
  #:type-name Model)

(: model* (All (S) (-> (-> (Values (Listof (Graph S)) (Node S) S)) (Model S))))
(define (model* proc)
  (parameterize ([current-graph-used-ids (set)])
    (call-with-values proc model)))
