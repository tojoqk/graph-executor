#lang typed/racket

(require "../graph.rkt")
(require "../prompt.rkt")
(require "../message.rkt")
(require "../prompt/console.rkt")
(require "../executor.rkt")
(require "../journal.rkt")
(require "../history.rkt")
(require "../event-logger.rkt")

(provide console-run console-choose console-command-dispatch
         current-console-random-prompt-display
         current-console-trace-display current-console-trace-display?
         Console-Command)

(define-type Console-Command (U (List 'transform Symbol String (-> Journal Journal))
                                (List 'action Symbol String (-> Journal Void))
                                (List 'restore Symbol String (-> (Option Journal)))
                                (List 'quit Symbol String)))

(: current-console-commands (Parameterof (Listof Console-Command)))
(define current-console-commands (make-parameter
                                  (list (list 'transform 'u "Undo" journal-undo)
                                        (list 'quit 'q "Quit"))))

(: current-console-has-quit-command? (-> Boolean))
(define (current-console-has-quit-command?)
  (and (memf (lambda ([c : Console-Command]) (eq? 'quit (first c))) (current-console-commands))
       #t))

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
  (let loop ([n n] [st st] [j : Journal j])
    (define command-dispatch (console-command-dispatch gs entry initial-state loop))
    (let ([ne (next-edges gs st n)])
      (case (car ne)
        [(terminated)
         (when (current-console-trace-display?)
           (newline)
           (displayln ">> Terminated"))
         (define choose-pmt ((node-prompt n) st))
         (if (current-console-has-quit-command?)
             (command-dispatch n st j (console-choose choose-pmt '()))
             (values n st j))]
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
         (let ([cmd (console-choose choose-pmt (map (inst edge-name T S) (second ne)))])
           (cond [(string? cmd)
                  (define chosen-edge (find-edge (second ne) cmd))
                  (let* ([logger (make-event-logger chosen-edge
                                                    choose-pmt
                                                    (second ne)
                                                    '()
                                                    (edge-cod chosen-edge))]
                         [next-st (console-step st chosen-edge logger)])
                    (loop (edge-cod chosen-edge)
                          next-st
                          (cons (event-logger->journal-entry logger) j)))]
                 [else (command-dispatch n st j cmd)]))]))))

(: console-command-dispatch (All (T S)
                                 (-> (Listof (Graph T S)) (Node T S) S
                                     (-> (Node T S) S Journal
                                         (Values (Node T S) S Journal))
                                     (-> (Node T S) S Journal
                                         Console-Command
                                         (Values (Node T S) S Journal)))))
(define ((console-command-dispatch gs n-init st-init loop) n st j cmd)
  (case (car cmd)
    [(quit) (values n st j)]
    [(action) ((fourth cmd) j)
              (loop n st j)]
    [(transform) (define-values (tr-n tr-st tr-h)
                   (replay gs n-init st-init ((fourth cmd) j)))
                 (loop tr-n tr-st (history->journal tr-h))]
    [(restore) (define-values (rs-n rs-st rs-h)
                 (replay gs n-init st-init
                         (cond [((fourth cmd)) => identity]
                               [else j])))
               (loop rs-n rs-st (history->journal rs-h))]))

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

(: console-prompt/log (All (T S) (-> (Event-Logger T S) (U 'edge 'node) Prompt-Implementation)))
(define ((console-prompt/log logger type) title op)
  (let ([info (console-prompt title op)])
    (event-logger-prompt-log! logger type info)
    info))

(: console-choose (case-> (-> String (Pairof String (Listof String))
                              (Values (U String Console-Command)))
                          (-> String Null
                              (Values Console-Command))))
(define (console-choose title choices)
  (let ([out (open-output-string)])
    (newline)
    (fprintf out "* ~a\n" title)
    (unless (null? choices)
      (for ([choice choices]
            [i : Positive-Integer (in-naturals 1)])
        (if (pair? choice)
            (cond [(car choice)
                   => (lambda ([target : String])
                        (fprintf out "- [~a] ~a: ~a\n" i (car choice) (cadr choice)))])
            (fprintf out "  - [~a] ~a\n" i choice))))
    (for ([cmd (current-console-commands)])
      (when cmd (fprintf out "  - [~a] ~a\n" (second cmd) (third cmd))))
    (let ([text (get-output-string out)])
      (display text)
      (let retry ()
        (display "? ")
        (let ([line (read-line)])
          (cond [(eof-object? line) (retry)]
                [(string->number line)
                 => (lambda ([n : Number])
                      (if (and (exact? n)
                               (positive-integer? n)
                               (<= n (length choices)))
                          (list-ref choices (sub1 n))
                          (retry)))]
                [(findf (lambda ([cmd : Console-Command])
                          (string=? (symbol->string (second cmd)) (string-trim line)))
                        (current-console-commands))
                 => identity]
                [else (retry)]))))))
