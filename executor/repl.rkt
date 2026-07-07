#lang typed/racket

(require "../graph.rkt")
(require "../prompt.rkt")
(require "../message.rkt")
(require "../private/prompt/repl.rkt")
(require "../executor.rkt")
(require "../history.rkt")

(provide repl-run repl-choose repl-prompt/log
         current-repl-random-prompt-mode)

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
                (let ([ne (next-edges gs st n)])
                  (case (car ne)
                    [(terminated) (terminate)]
                    [(auto)
                     (let ([chosen-edge (auto-choose ne)])
                       (displayln (format ">> [Auto] ~a" (edge-name chosen-edge)))
                       (define-values (next-st next-node next-h)
                         (repl-step st chosen-edge
                                    (cons (make-history-edge 'auto
                                                             (edge-name chosen-edge)
                                                             (string-join `(,(node-name n)
                                                                            ,@(cond [(node-desc n) => list]
                                                                                    [else '()])
                                                                            ,@(cond [(edge-desc chosen-edge) => list]
                                                                                    [else '()]))
                                                                     "\n"))
                                          h)))
                       (loop next-node next-st next-h))]
                    [(choose)
                     (define-values (chosen-edge next-h-1)
                       (repl-choose ne h))
                     (define-values (next-st next-node next-h-2)
                       (repl-step st chosen-edge next-h-1))
                     (loop next-node next-st next-h-2)])))]
          [else (terminate)])))

(: repl-step (All (T S) (-> S (Edge T S) History (values S (Node T S) History))))
(define (repl-step st e h)
  (let ([n (edge-cod e)]
        [bh : (Boxof History) (box h)])
    (: log-edge-prompt (-> String Prompt-Value Void))
    (define (log-edge-prompt title val)
      (set-box! bh (cons (make-history-prompt val title) (unbox bh))))
    (: log-node-prompt (-> String Prompt-Value Void))
    (define (log-node-prompt title val)
      (set-box! bh (cons (make-history-prompt val title) (unbox bh))))
    (: message-with-log (-> Any Void))
    (define (message-with-log val)
      (let ([str (~a val)])
        (set-box! bh (cons (make-history-message str) (unbox bh)))
        (displayln val)))
    (define st-1
      (parameterize ([current-prompt ((inst repl-prompt/log Any) log-edge-prompt)]
                     [current-message message-with-log])
        ((edge-trans e) st)))
    (printf "--- Current Node: ~a (Graph: ~a) ---\n" (node-name n) (node-graph-name n))
    (cond [(node-desc n) => displayln])
    (set-box! bh (cons (make-history-node (node-name n) (node-desc n)) (unbox bh)))
    (define st-2
      (parameterize ([current-prompt ((inst repl-prompt/log Any) log-node-prompt)]
                     [current-message message-with-log])
        ((node-trans n) st-1)))
    (values st-2 n (unbox bh))))

(: repl-choose (All (T S)
                    (-> (List 'choose (Pairof (Edge T S) (Listof (Edge T S))))
                        History
                        (Values (Edge T S) History))))
(define (repl-choose ne h)
  (let* ([edges : (Pairof (Edge T S) (Listof (Edge T S))) (second ne)]
         [edge-names ((inst map String (Edge T S)) edge-name edges)]
         [dom : (Node T S) (edge-dom (car edges))]
         [title : String (node-prompt dom)]
         [prompt-text-box : (Boxof String) (box "")])
    (: log-prompt-text (-> String Void))
    (define (log-prompt-text prompt-text)
      (set-box! prompt-text-box prompt-text))
    (let ([name : String ((repl-prompt log-prompt-text) title `(choose ,string? ,edge-names))])
      (cond [(findf (lambda ([edge : (Edge T S)]) (string=? name (edge-name edge))) edges)
             => (lambda ([e : (Edge T S)])
                  (values e (cons (make-history-edge 'choose (edge-name e)
                                                     (unbox prompt-text-box))
                                  h)))]
            [else (error 'repl-choose "unexpected error")]))))

(: repl-prompt/log (All (A) (-> (-> String Prompt-Value Void) (Prompt A))))
(define ((repl-prompt/log k) title op [_ (hash)])
  (let ([prompt-text-box : (Boxof String) (box "")])
    (: log-prompt (-> String Void))
    (define (log-prompt text)
      (set-box! prompt-text-box text))
    (let ([value (((inst repl-prompt A) log-prompt) title op)])
      (k (unbox prompt-text-box) value)
      value)))
