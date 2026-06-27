#lang typed/racket

(require "../graph.rkt")
(require "../prompt.rkt")
(require "../prompt/repl.rkt")
(require "../executor.rkt")
(require "../history.rkt")

(provide repl-run)

(: repl-run (All (T S) (-> (Listof (Graph T S)) (Node T S) S
                           (Values (Node T S) S History))))
(define (repl-run gs entry initial-state)
  (let loop ([n entry]
             [st initial-state]
             [h : History '()])
    (define (terminate)
      (displayln ">> Terminated")
      (values n st h))
    (cond [(find-graph gs (node-graph-id n))
           => (lambda ([g : (Graph T S)])
                (displayln (format "--- Current Node: ~a (Graph: ~a) ---"
                                   (node-name n)
                                   (graph-name g)))
                (let ([ne (next-edges gs st n)])
                  (case (car ne)
                    [(terminated) (terminate)]
                    [(auto)
                     (let ([chosen-edge (auto-choose ne)])
                       (displayln (format ">> [Auto] ~a" (edge-name chosen-edge)))
                       (define-values (next-st next-node next-h)
                         (repl-step st chosen-edge
                                    (cons (history-choose 'auto
                                                          (edge-name chosen-edge) (edge-desc chosen-edge)
                                                          (node-graph-name n)
                                                          (node-name n) (node-desc n))
                                          h)))
                       (loop next-node next-st next-h))]
                    [(choose)
                     (let ([chosen-edge (repl-choose ne)])
                       (define-values (next-st next-node next-h)
                         (repl-step st chosen-edge
                                    (cons (history-choose 'choose
                                                          (edge-name chosen-edge) (edge-desc chosen-edge)
                                                          (node-graph-name n)
                                                          (node-name n) (node-desc n))
                                          h)))
                       (loop next-node next-st next-h))])))]
          [else (terminate)])))

(: repl-step (All (T S) (-> S (Edge T S) History (values S (Node T S) History))))
(define (repl-step st e h)
  (let ([n (edge-cod e)]
        [bh : (Boxof History) (box h)])
    (: log-edge-prompt (-> String Prompt-Value Void))
    (define (log-edge-prompt title val)
      (set-box! bh (cons (history-prompt val title) (unbox bh))))
    (: log-node-prompt (-> String Prompt-Value Void))
    (define (log-node-prompt title val)
      (set-box! bh (cons (history-prompt val title) (unbox bh))))
    (define st-1
      (parameterize ([current-prompt ((inst repl-prompt/log Any) log-edge-prompt)])
        ((edge-trans e) st)))
    (define st-2
      (parameterize ([current-prompt ((inst repl-prompt/log Any) log-node-prompt)])
        ((node-trans n) st-1)))
    (values st-2 n (unbox bh))))

(: repl-choose (All (T S)
                    (-> (List 'choose (Pairof (Edge T S) (Listof (Edge T S)))) (Edge T S))))
(define (repl-choose ne)
  (let* ([edges : (Pairof (Edge T S) (Listof (Edge T S))) (second ne)]
         [edge-names ((inst map String (Edge T S)) edge-name edges)]
         [dom : (Node T S) (edge-dom (car edges))]
         [title : String (cond [(node-desc dom)
                                => (lambda ([desc : String])
                                     (format "~a\n~a\n" (node-name dom) desc))]
                               [else (node-name dom)])])
    (let ([name : String (repl-prompt title `(choose ,string? ,edge-names))])
      (cond [(memf (lambda ([edge : (Edge T S)]) (string=? name (edge-name edge))) edges) => car]
            [else (error 'repl-choose "unexpected error")]))))

(: repl-prompt/log (All (A) (-> (-> String Prompt-Value Void) (Prompt A))))
(define ((repl-prompt/log k) title op)
  (let ([value ((inst repl-prompt A) title op)])
    (k title value)
    value))
