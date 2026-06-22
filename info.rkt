#lang info
(define collection "graph-executor")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/graph-executor.scrbl" ())))
(define pkg-desc "A library for modeling and executing directed graph structures")
(define version "0.0")
(define pkg-authors '(tojoqk))
(define license '(Apache-2.0))
