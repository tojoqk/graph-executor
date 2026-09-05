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
         Console-Command console-command?
         transform-console-command action-console-command restore-console-command quit-console-command
         Console-Config console-config
         random-edge-option
         default-console-commands default-console-chooser
         transform-console-command? transform-console-command
         action-console-command? action-console-command
         restore-console-command? restore-console-command
         quit-console-command? quit-console-command)

(struct random-edge-option edge-option ([weight : Positive-Integer]))

(struct transform-console-command ([key : Symbol] [name : String] [proc : (-> (Listof Journal-Entry) (Listof Journal-Entry))])
  #:type-name Transform-Console-Command)
(struct action-console-command ([key : Symbol] [name : String] [proc : (-> (Listof Journal-Entry) Void)])
  #:type-name Action-Console-Command)
(struct restore-console-command ([key : Symbol] [name : String] [proc : (-> (Option (Listof Journal-Entry)))])
    #:type-name Restore-Console-Command)
(struct quit-console-command ([key : Symbol] [name : String])
  #:type-name Quit-Console-Command)

(define-type Console-Command (U Transform-Console-Command
                                Action-Console-Command
                                Restore-Console-Command
                                Quit-Console-Command))
(define-predicate console-command? Console-Command)

(: console-command-key (-> Console-Command Symbol))
(define (console-command-key cmd)
  (cond [(transform-console-command? cmd) (transform-console-command-key cmd)]
        [(action-console-command? cmd) (action-console-command-key cmd)]
        [(restore-console-command? cmd) (restore-console-command-key cmd)]
        [(quit-console-command? cmd) (quit-console-command-key cmd)]))

(: console-command-name (-> Console-Command String))
(define (console-command-name cmd)
  (cond [(transform-console-command? cmd) (transform-console-command-name cmd)]
        [(action-console-command? cmd) (action-console-command-name cmd)]
        [(restore-console-command? cmd) (restore-console-command-name cmd)]
        [(quit-console-command? cmd) (quit-console-command-name cmd)]))

(: default-console-commands (Listof Console-Command))
(define default-console-commands (list (transform-console-command 'u "Undo" journal-undo)
                                       (quit-console-command 'q "Quit")))

(: default-console-chooser (-> Node-Info (U 'choose 'random)))
(define default-console-chooser (lambda (_) 'choose))

(struct %console-config ([commands : (Listof Console-Command)]
                         [trace-display : (U 'show 'hide)]
                         [chooser : (-> Node-Info (U 'choose 'random))])
  #:type-name Console-Config)

(: console-config (-> [#:commands (Listof Console-Command)]
                      [#:trace-display (U 'show 'hide)]
                      [#:chooser (-> Node-Info (U 'choose 'random))]
                      Console-Config))
(define (console-config #:commands [commands default-console-commands]
                        #:trace-display [trace-display 'show]
                        #:chooser [chooser default-console-chooser])
  (%console-config commands trace-display chooser))

(: console-config-has-quit-command? (-> Console-Config Boolean))
(define (console-config-has-quit-command? config)
  (and (memf (lambda ([c : Console-Command]) (quit-console-command? c))
             (%console-config-commands config))
       #t))

(: console-config-trace-display? (-> Console-Config Boolean))
(define (console-config-trace-display? config)
  (case (%console-config-trace-display config)
    [(show) #t]
    [(hide) #f]))

(: console-run (All (S) (-> (Model S) [#:journal (Listof Journal-Entry)] [#:config Console-Config] (Listof Journal-Entry))))
(define (console-run m #:journal [j '()] #:config [config (console-config)])
  (define gs (model-graphs m))
  (define-values (n st _) (replay m j))
  (define-values (call-with-emitter emit)
    ((inst make-emitter (Pairof Prompt-Value Any) S)))
  (define-values (_n _st result-j)
    (let loop : (Values (Node S) S (Listof Journal-Entry)) ([n n] [st st] [j : (Listof Journal-Entry) j])
      (define command-dispatch (console-command-dispatch m loop))
      (let ([ne (next-edges gs st n)])
        (case (car ne)
          [(terminated auto-conflicted)
           (case (car ne)
             [(auto-conflicted)
              (newline)
              (printf ">> Auto conflicted: ~s" (cdr ne))]
             [(terminated)
              (when (console-config-trace-display? config)
                (newline)
                (displayln ">> Terminated"))])
           (define choose-pmt ((node-prompt n) st))
           (if (and (eq? ((%console-config-chooser config) (node-node-info n)) 'choose)
                    (console-config-has-quit-command? config))
               (command-dispatch n st j
                                 (console-choose 'choose config choose-pmt '()))
               (values n st j))]
          [(auto)
           (let* ([chosen-edge (auto-choose ne)])
             (when (console-config-trace-display? config)
               (displayln (format ">> [Auto] ~a" (edge-name chosen-edge))))
             (match-define (cons ps next-st)
               (call-with-emitter
                (thunk (console-step config st chosen-edge emit))))
             (loop (edge-to chosen-edge)
                   next-st
                   (cons (journal-entry 'auto (edge-name chosen-edge) #:prompt-records ps) j)))]
          [(choose)
           (define choose-pmt ((node-prompt n) st))
           (let ([cmd (console-choose ((%console-config-chooser config) (node-node-info n)) config choose-pmt (second ne))])
             (cond [(edge? cmd)
                    (define chosen-edge cmd)
                    (match-define (cons ps next-st)
                      (call-with-emitter
                       (thunk (console-step config st chosen-edge emit))))
                    (loop (edge-to chosen-edge)
                          next-st
                          (cons (journal-entry 'choose (edge-name chosen-edge) #:prompt-records ps) j))]
                   [else (command-dispatch n st j cmd)]))]))))
  result-j)

(: console-command-dispatch (All (S)
                                 (-> (Model S)
                                     (-> (Node S) S (Listof Journal-Entry)
                                         (Values (Node S) S (Listof Journal-Entry)))
                                     (-> (Node S) S (Listof Journal-Entry)
                                         Command
                                         (Values (Node S) S (Listof Journal-Entry))))))
(define ((console-command-dispatch m loop) n st j cmd)
  (define gs (model-graphs m))
  (cond [(quit-command? cmd) (values n st j)]
        [(action-command? cmd) ((action-command-proc cmd) j) (loop n st j)]
        [(transform-command? cmd) (define-values (tr-n tr-st tr-h)
                                    (replay m ((transform-command-proc cmd) j)))
                                  (loop tr-n tr-st (history->journal tr-h))]
        [(restore-command? cmd) (define-values (rs-n rs-st rs-h)
                                  (replay m (cond [((restore-command-proc cmd)) => identity]
                                                  [else j])))
                                (loop rs-n rs-st (history->journal rs-h))]))

(: console-step (All (S) (-> Console-Config S (Edge S) (-> (Pairof Prompt-Value Any) Void) S)))
(define (console-step config st e emit)
  (define (console-message val)
    (newline)
    (displayln val))
  (parameterize ([current-prompt (console-prompt/log emit)]
                 [current-message console-message])
    ((node-trans (edge-to e))
     (parameterize ([current-prompt (console-prompt/log emit)]
                    [current-message console-message])
       (begin0 ((edge-trans e) st)
         (when (console-config-trace-display? config)
           (let ([n (edge-to e)])
             (printf "--- Current Node: ~a (Graph: ~a) ---\n" (node-name n) (node-graph-name n))
             (cond [(node-desc n) => displayln]))))))))

(: console-prompt/log (All (S) (-> (-> (Pairof Prompt-Value Any) Void)
                                   Prompt-Implementation)))
(define ((console-prompt/log emit) title op)
  (define-values (val extra) (console-prompt title op))
  (emit (cons val extra))
  (values val extra))

(: console-command->command (-> Console-Command Command))
(define (console-command->command c)
  (cond 
    [(quit-console-command? c) (quit-command)]
    [(action-console-command? c) (action-command (action-console-command-proc c))]
    [(transform-console-command? c) (transform-command (transform-console-command-proc c))]
    [(restore-console-command? c) (restore-command (restore-console-command-proc c))]))

(: console-choose/choose (All (S) (case-> (-> Console-Config
                                              String (Pairof (Edge S) (Listof (Edge S)))
                                              (U (Edge S) Command))
                                          (-> Console-Config
                                              String Null
                                              Command))))
(define (console-choose/choose config pmt choices)
  (let ([out (open-output-string)])
    (newline)
    (fprintf out "* ~a\n" pmt)
    (unless (null? choices)
      (for ([choice choices]
            [i : Positive-Integer (in-naturals 1)])
        (fprintf out "  - [~a] ~a\n" i (edge-name choice))))
    (for ([cmd (%console-config-commands config)])
      (when cmd (fprintf out "  - [~a] ~a\n" (console-command-key cmd) (console-command-name cmd))))
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
                          (string=? (symbol->string (console-command-key cmd)) (string-trim line)))
                        (%console-config-commands config))
                 => console-command->command]
                [else (retry)]))))))

(: edge-weight (All (S) (-> (Edge S) Positive-Integer)))
(define (edge-weight e)
  (cond [(findf random-edge-option? (edge-edge-options e))
         => (lambda (opt)
              (assert opt random-edge-option?)
              (random-edge-option-weight opt))]
        [else 1]))

(: console-choose/random (All (S) (-> (Pairof (Edge S) (Listof (Edge S))) (Edge S))))
(define (console-choose/random choices)
  (let* ([s (for/sum : Integer ([e choices]) (edge-weight e))]
         [r (random s)])
    (let loop ([choices choices]
               [r r])
      (let ([fst (car choices)]
            [rst (cdr choices)])
        (cond [(< r (edge-weight fst)) fst]
              [(null? rst) (error 'console-choose/random "unreachble")]
              [else (loop rst (- r (edge-weight fst)))])))))

(: console-choose (All (S) (case-> (-> (U 'choose 'random) Console-Config String (Pairof (Edge S) (Listof (Edge S))) (U (Edge S) Command))
                                   (-> 'choose Console-Config String Null Command))))
(define (console-choose chooser config pmt choices)
  (case chooser
    [(choose) (console-choose/choose config pmt choices)]
    [(random) (console-choose/random choices)]))
