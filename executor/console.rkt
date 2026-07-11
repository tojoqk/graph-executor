#lang typed/racket

(require "../graph.rkt")
(require "../prompt.rkt")
(require "../message.rkt")
(require "../private/prompt/console.rkt")
(require "../executor.rkt")
(require "../journal.rkt")

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

(: console-run (All (T S) (->* ((Listof (Graph T S)) (Node T S) S)
                               (Journal)
                               (Values (Node T S) S Journal))))
(define (console-run gs entry initial-state [j '()])
  (define-values (n st _) (replay gs entry initial-state j))
  (let loop ([n n]
             [st st]
             [j : Journal j])
    (define (terminate)
      (when (current-console-trace-display?)
        (displayln ">> Terminated"))
      (values n st j))
    (let ([ne (next-edges gs st n)])
      (case (car ne)
        [(terminated) (terminate)]
        [(auto)
         (let* ([chosen-edge (auto-choose ne)]
                [logger (make-prompt-logger (edge-name chosen-edge) '())])
           (when (current-console-trace-display?)
             (displayln (format ">> [Auto] ~a" (edge-name chosen-edge))))
           (let ([next-st (console-step st chosen-edge logger)])
             (loop (edge-cod chosen-edge)
                   next-st
                   (cons (prompt-logger->entry logger) j))))]
        [(choose)
         (let-values ([(chosen-edge attrs) (console-choose ne)])
           (let* ([logger (make-prompt-logger (edge-name chosen-edge) attrs)]
                  [next-st (console-step st chosen-edge logger)])
             (loop (edge-cod chosen-edge)
                   next-st
                   (cons (prompt-logger->entry logger) j))))]))))

(: console-step (All (T S) (-> S (Edge T S) Prompt-Logger S)))
(define (console-step st e logger)
  (: message-with-log (-> Any Void))
  (define (message-with-log val)
    (newline)
    (displayln val))
  (parameterize ([current-prompt ((inst console-prompt/log Any) logger)]
                 [current-message message-with-log])
    ((node-trans (edge-cod e))
     (begin0 ((edge-trans e) st)
       (when (current-console-trace-display?)
         (let ([n (edge-cod e)])
           (printf "--- Current Node: ~a (Graph: ~a) ---\n" (node-name n) (node-graph-name n))
           (cond [(node-desc n) => displayln])))))))

(: console-choose (All (T S)
                       (-> (List 'choose (Pairof (Edge T S) (Listof (Edge T S))))
                           (Values (Edge T S) Prompt-Attributes))))
(define (console-choose ne)
  (let* ([edges : (Pairof (Edge T S) (Listof (Edge T S))) (second ne)]
         [edge-names ((inst map String (Edge T S)) edge-name edges)]
         [dom : (Node T S) (edge-dom (car edges))]
         [title : String (node-prompt dom)])
    (let-values ([(name attrs) (console-prompt title `(choose ,string? ,edge-names))])
      (cond [(findf (lambda ([edge : (Edge T S)]) (string=? name (edge-name edge))) edges)
             => (lambda ([e : (Edge T S)])
                  (values e attrs))]
            [else (error 'console-choose "unexpected error")]))))

(: console-prompt/log (All (A) (-> Prompt-Logger (Prompt A))))
(define ((console-prompt/log logger) title op)
  (let-values ([(value attrs) ((inst console-prompt A) title op)])
    (prompt-logger-log! logger value attrs)
    value))
