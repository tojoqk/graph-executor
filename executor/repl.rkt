#lang typed/racket

(require "../graph.rkt")
(require "../executor.rkt")

(provide repl-choose repl-run)

(: repl-choose (All (T S)
                    (-> (List 'choose (Pairof (Edge T S) (Listof (Edge T S)))) (Edge T S))))
(define (repl-choose ne)
  (let ([es (cadr ne)])
    (displayln "choose:")
    (for ([e : (Edge T S) es]
          [i (in-naturals 0)])
      (display "  ")
      (display i)
      (display ": ")
      (displayln (edge-name e)))
    (display "? ")
    (let ([n (read)])
      (if (and (natural? n)
               (< n (length es)))
          (list-ref es n)
          (repl-choose ne)))))

(: repl-run (All (T S) (-> (Listof (Graph T S)) S (Node T S) (values S (Node T S)))))
(define (repl-run gs st n)
  (let ([g (find-graph gs (node-graph-id n))])
    (displayln (format "--- Current Node: ~a (Graph: ~a) ---"
                       (node-name n)
                       (graph-name g)))
    (let ([ne (next-edges gs st n)])
      (case (car ne)
        [(terminated)
         (displayln ">> Terminated")
         (values st n)]
        [(auto)
         (let ([chosen-edge (auto-choose ne)])
           (displayln (format ">> [Auto] ~a" (edge-name chosen-edge)))
           (let-values ([(next-st next-node) (step st chosen-edge)])
             (repl-run gs next-st next-node)))]
        [(choose)
         (let ([chosen-edge (repl-choose ne)])
           (let-values ([(next-st next-node) (step st chosen-edge)])
             (repl-run gs next-st next-node)))]))))
