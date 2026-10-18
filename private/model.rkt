#lang typed/racket

(require "graph.rkt")

(provide Model model? (rename-out [model* model]) model-graphs model-node model-state model-terminal)

(struct (S) model ([graphs : (Listof (Graph S))]
                   [node : (Node S)]
                   [state : S]
                   [terminal : (-> Node-Info Any)])
  #:type-name Model)

(: model* (All (S) (-> (-> (Values (Listof (Graph S)) (Node S) S))
                       (-> Node-Info Any)
                       [#:default-node-prompt Any] (Model S))))
(define (model* proc terminal? #:default-node-prompt [default-node-prompt "Choose:"])
  (parameterize ([current-graph-used-ids (set)]
                 [current-node-prompt default-node-prompt])
    (define-values (gs n st) (proc))
    (model gs n st terminal?)))
