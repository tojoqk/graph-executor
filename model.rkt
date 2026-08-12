#lang typed/racket

(require "graph.rkt")

(provide Model model model-graphs model-node model-state)

(struct (S) model ([graphs : (Listof (Graph S))]
                   [node : (Node S)]
                   [state : S])
  #:type-name Model)
