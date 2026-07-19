#lang typed/racket

(require "../graph.rkt")
(require "../prompt.rkt")
(require "../message.rkt")
(require "../prompt/console.rkt")
(require "../executor.rkt")
(require "../journal.rkt")
(require "../event-logger.rkt")

(provide console-run console-choose
         current-console-random-prompt-display
         current-console-trace-display current-console-trace-display?
         current-console-quit-command current-console-undo-command)

(: current-console-undo-command (Parameterof (Option (List Symbol String))))
(define current-console-undo-command (make-parameter '(u "Undo")))

(: current-console-quit-command (Parameterof (Option (List Symbol String))))
(define current-console-quit-command (make-parameter '(q "Quit")))

(: current-console-trace-display (Parameterof (U 'show 'hide)))
(define current-console-trace-display (make-parameter 'show))

(: current-console-trace-display? (-> Boolean))
(define (current-console-trace-display?)
  (case (current-console-trace-display)
    [(show) #t]
    [(hide) #f]))

(: console-run (All (T S) (-> (Listof (Graph T S)) (Node T S) S
                              [#:journal Journal]
                              (Values (Node T S) S Journal))))
(define (console-run gs entry initial-state #:journal [j '()])
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
                [logger (make-event-logger chosen-edge (edge-cod chosen-edge))])
           (when (current-console-trace-display?)
             (displayln (format ">> [Auto] ~a" (edge-name chosen-edge))))
           (let ([next-st (console-step st chosen-edge logger)])
             (loop (edge-cod chosen-edge)
                   next-st
                   (cons (event-logger->journal-entry logger) j))))]
        [(choose)
         (define choose-pmt ((node-prompt n) st))
         (let ([chosen-edge (console-choose choose-pmt ne)])
           (cond [(eq? chosen-edge 'quit) (terminate)]
                 [(eq? chosen-edge 'undo)
                  (define undo-j (journal-undo j))
                  (define-values (undo-n undo-st _)
                    (replay gs entry initial-state (journal-undo j)))
                  (loop undo-n undo-st undo-j)]
                 [else
                  (let* ([logger (make-event-logger chosen-edge
                                                    choose-pmt
                                                    (second ne)
                                                    '()
                                                    (edge-cod chosen-edge))]
                         [next-st (console-step st chosen-edge logger)])
                    (loop (edge-cod chosen-edge)
                          next-st
                          (cons (event-logger->journal-entry logger) j)))]))]))))

(: console-step (All (T S) (-> S (Edge T S) (Event-Logger T S) S)))
(define (console-step st e logger)
  (: message-with-log (-> (U 'node 'edge) (-> Any Void)))
  (define ((message-with-log type) val)
    (event-logger-message-log! logger type (message-info val))
    (newline)
    (displayln val))
  (parameterize ([current-prompt (console-prompt/log logger 'node)]
                 [current-message (message-with-log 'node)])
    ((node-trans (edge-cod e))
     (parameterize ([current-prompt (console-prompt/log logger 'edge)]
                    [current-message (message-with-log 'edge)])
       (begin0 ((edge-trans e) st)
         (when (current-console-trace-display?)
           (let ([n (edge-cod e)])
             (printf "--- Current Node: ~a (Graph: ~a) ---\n" (node-name n) (node-graph-name n))
             (cond [(node-desc n) => displayln]))))))))

(: console-choose (All (T S)
                       (-> String
                           (List 'choose (Pairof (Edge T S) (Listof (Edge T S))))
                           (U (Edge T S) 'quit 'undo))))
(define (console-choose title ne)
  (let* ([edges : (Pairof (Edge T S) (Listof (Edge T S))) (second ne)]
         [edge-names ((inst map String (Edge T S)) edge-name edges)]
         [dom : (Node T S) (edge-dom (car edges))])
    (let* ([name (choose-edge title edge-names)])
      (cond [(eq? name 'quit) 'quit]
            [(eq? name 'undo) 'undo]
            [(findf (lambda ([edge : (Edge T S)]) (string=? name (edge-name edge))) edges) => identity]
            [else (error 'console-choose "unexpected error")]))))

(: console-prompt/log (All (T S) (-> (Event-Logger T S) (U 'edge 'node) Prompt-Implementation)))
(define ((console-prompt/log logger type) title op)
  (let ([info (console-prompt title op)])
    (event-logger-prompt-log! logger type info)
    info))

(: choose-edge (-> String (Listof String)
                   (Values (U String 'quit 'undo))))
(define (choose-edge title choices)
  (let ([out (open-output-string)])
    (newline)
    (fprintf out "* ~a\n" title)
    (for ([choice choices]
          [i : Positive-Integer (in-naturals 1)])
      (if (pair? choice)
          (cond [(car choice)
                 => (lambda ([target : String])
                      (fprintf out "- [~a] ~a: ~a\n" i (car choice) (cadr choice)))])
          (fprintf out "  - [~a] ~a\n" i choice)))
    (for ([cmd (list (current-console-undo-command)
                     (current-console-quit-command))])
      (when cmd (fprintf out "  - [~a] ~a\n" (first cmd) (second cmd))))
    (let ([text (get-output-string out)])
      (display text)
      (let retry ()
        (display "? ")
        (let ([line (read-line)]
              [quit-cmd (current-console-quit-command)]
              [undo-cmd (current-console-undo-command)])
          (cond [(eof-object? line) (retry)]
                [(string->number line)
                 => (lambda ([n : Number])
                      (if (and (exact? n)
                               (positive-integer? n)
                               (<= n (length choices)))
                          (list-ref choices (sub1 n))
                          (retry)))]
                [(and quit-cmd (string=? (symbol->string (first quit-cmd)) (string-trim line))) 'quit]
                [(and undo-cmd (string=? (symbol->string (first undo-cmd)) (string-trim line))) 'undo]
                [else (retry)]))))))
