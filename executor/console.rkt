#lang typed/racket

(require "../graph.rkt")
(require "../model.rkt")
(require "../prompt.rkt")
(require "../message.rkt")
(require "../prompt/console.rkt")
(require "../executor.rkt")
(require "../journal.rkt")
(require "../history.rkt")
(require "../effect/emitter.rkt")

(provide console-run console-choose console-command-dispatch
         current-console-random-prompt-display
         current-console-trace-display current-console-trace-display?
         Console-Command current-console-commands)

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

(: console-run (All (S) (-> (Model S) [#:journal Journal] Journal)))
(define (console-run m #:journal [j '()])
  (define gs (model-graphs m))
  (define-values (n st _) (replay m j))
  (define-values (call-with-emitter emit)
    ((inst make-emitter (Pairof Prompt-Value Prompt-Attributes) S)))
  (define-values (_n _st result-j)
    (let loop : (Values (Node S) S Journal) ([n n] [st st] [j : Journal j])
      (define command-dispatch (console-command-dispatch m loop))
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
           (let* ([chosen-edge (auto-choose ne)])
             (when (current-console-trace-display?)
               (displayln (format ">> [Auto] ~a" (edge-name chosen-edge))))
             (match-define (cons ps next-st)
               (call-with-emitter
                (thunk (console-step st chosen-edge emit))))
             (loop (edge-to chosen-edge)
                   next-st
                   (cons (auto-journal-entry (edge-name chosen-edge) ps) j)))]
          [(choose)
           (define choose-pmt ((node-prompt n) st))
           (let ([cmd (console-choose choose-pmt (map (inst edge-name S) (second ne)))])
             (cond [(string? cmd)
                    (define chosen-edge (find-edge (second ne) cmd))
                    (match-define (cons ps next-st)
                      (call-with-emitter
                       (thunk (console-step st chosen-edge emit))))
                    (loop (edge-to chosen-edge)
                          next-st
                          (cons (choose-journal-entry (edge-name chosen-edge) '() ps) j))]
                   [else (command-dispatch n st j cmd)]))]))))
  result-j)

(: console-command-dispatch (All (S)
                                 (-> (Model S)
                                     (-> (Node S) S Journal
                                         (Values (Node S) S Journal))
                                     (-> (Node S) S Journal
                                         Command
                                         (Values (Node S) S Journal)))))
(define ((console-command-dispatch m loop) n st j cmd)
  (define gs (model-graphs m))
  (case (car cmd)
    [(quit) (values n st j)]
    [(action) ((second cmd) j)
              (loop n st j)]
    [(transform) (define-values (tr-n tr-st tr-h)
                   (replay m ((second cmd) j)))
                 (loop tr-n tr-st (history->journal tr-h))]
    [(restore) (define-values (rs-n rs-st rs-h)
                 (replay m (cond [((second cmd)) => identity]
                                 [else j])))
               (loop rs-n rs-st (history->journal rs-h))]))

(: console-step (All (S) (-> S (Edge S) (-> (Pairof Prompt-Value Prompt-Attributes) Void) S)))
(define (console-step st e emit)
  (define (console-message val)
    (newline)
    (displayln val))
  (parameterize ([current-prompt (console-prompt/log emit)]
                 [current-message console-message])
    ((node-trans (edge-to e))
     (parameterize ([current-prompt (console-prompt/log emit)]
                    [current-message console-message])
       (begin0 ((edge-trans e) st)
         (when (current-console-trace-display?)
           (let ([n (edge-to e)])
             (printf "--- Current Node: ~a (Graph: ~a) ---\n" (node-name n) (node-graph-name n))
             (cond [(node-desc n) => displayln]))))))))

(: console-prompt/log (All (S) (-> (-> (Pairof Prompt-Value Prompt-Attributes) Void)
                                   Prompt-Implementation)))
(define ((console-prompt/log emit) title op)
  (define-values (val attrs) (console-prompt title op))
  (emit (cons val attrs))
  (values val attrs))

(: console-command->command (-> Console-Command Command))
(define (console-command->command c)
  (case (first c)
    [(quit) (list (first c))]
    [(action) (list (first c) (fourth c))]
    [(transform) (list (first c) (fourth c))]
    [(restore) (list (first c) (fourth c))]))

(: console-choose (case-> (-> Prompt-Meta (Pairof String (Listof String))
                              (U String Command))
                          (-> Prompt-Meta Null
                              Command)))
(define (console-choose meta choices)
  (let ([out (open-output-string)])
    (newline)
    (fprintf out "* ~a\n" (prompt-meta-title meta))
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
                 => console-command->command]
                [else (retry)]))))))
