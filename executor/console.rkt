#lang typed/racket

(require "../graph.rkt")
(require "../prompt.rkt")
(require "../message.rkt")
(require "../private/prompt/console.rkt")
(require "../executor.rkt")
(require "../history.rkt")

(provide console-run console-choose console-prompt/log
         current-console-random-prompt-display
         current-console-trace-display current-console-trace-display?)

(: current-console-trace-display (Parameterof (U 'show 'hide)))
(define current-console-trace-display (make-parameter 'show))

(: current-console-trace-display? (-> Boolean))
(define (current-console-trace-display?)
  (case (current-console-trace-display)
    [(show) #t]
    [(hide) #f]))

(: console-run (All (T S) (-> (Listof (Graph T S)) (Node T S) S
                           (Values (Node T S) S History))))
(define (console-run gs entry initial-state)
  (let loop ([n entry]
             [st initial-state]
             [h : History '()])
    (define (terminate)
      (when (current-console-trace-display?)
        (displayln ">> Terminated"))
      (values n st h))
    (cond [(find-graph gs (node-graph-id n))
           => (lambda ([g : (Graph T S)])
                (let ([ne (next-edges gs st n)])
                  (case (car ne)
                    [(terminated) (terminate)]
                    [(auto)
                     (let ([chosen-edge (auto-choose ne)])
                       (when (current-console-trace-display?)
                         (displayln (format ">> [Auto] ~a" (edge-name chosen-edge))))
                       (define-values (next-st next-node next-h)
                         (console-step st chosen-edge
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
                       (console-choose ne h))
                     (define-values (next-st next-node next-h-2)
                       (console-step st chosen-edge next-h-1))
                     (loop next-node next-st next-h-2)])))]
          [else (terminate)])))

(: console-step (All (T S) (-> S (Edge T S) History (values S (Node T S) History))))
(define (console-step st e h)
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
        (newline)
        (displayln val)))
    (define st-1
      (parameterize ([current-prompt ((inst console-prompt/log Any) log-edge-prompt)]
                     [current-message message-with-log])
        ((edge-trans e) st)))
    (when (current-console-trace-display?)
      (printf "--- Current Node: ~a (Graph: ~a) ---\n" (node-name n) (node-graph-name n)))
    (cond [(node-desc n) => displayln])
    (set-box! bh (cons (make-history-node (node-name n) (node-desc n)) (unbox bh)))
    (define st-2
      (parameterize ([current-prompt ((inst console-prompt/log Any) log-node-prompt)]
                     [current-message message-with-log])
        ((node-trans n) st-1)))
    (values st-2 n (unbox bh))))

(: console-choose (All (T S)
                    (-> (List 'choose (Pairof (Edge T S) (Listof (Edge T S))))
                        History
                        (Values (Edge T S) History))))
(define (console-choose ne h)
  (let* ([edges : (Pairof (Edge T S) (Listof (Edge T S))) (second ne)]
         [edge-names ((inst map String (Edge T S)) edge-name edges)]
         [dom : (Node T S) (edge-dom (car edges))]
         [title : String (node-prompt dom)]
         [prompt-text-box : (Boxof String) (box "")])
    (: log-prompt-text (-> String Void))
    (define (log-prompt-text prompt-text)
      (set-box! prompt-text-box prompt-text))
    (let ([name : String ((console-prompt log-prompt-text) title `(choose ,string? ,edge-names))])
      (cond [(findf (lambda ([edge : (Edge T S)]) (string=? name (edge-name edge))) edges)
             => (lambda ([e : (Edge T S)])
                  (values e (cons (make-history-edge 'choose (edge-name e)
                                                     (unbox prompt-text-box))
                                  h)))]
            [else (error 'console-choose "unexpected error")]))))

(: console-prompt/log (All (A) (-> (-> String Prompt-Value Void) (Prompt A))))
(define ((console-prompt/log k) title op [_ (hash)])
  (let ([prompt-text-box : (Boxof String) (box "")])
    (: log-prompt (-> String Void))
    (define (log-prompt text)
      (set-box! prompt-text-box text))
    (let ([value (((inst console-prompt A) log-prompt) title op)])
      (k (unbox prompt-text-box) value)
      value)))
