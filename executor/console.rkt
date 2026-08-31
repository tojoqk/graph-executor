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
         Console-Command transform-console-command action-console-command restore-console-command quit-console-command
         Console-Config console-config
         weight-edge-option
         default-console-commands default-console-chooser)

(struct weight-edge-option edge-option ([weight : Positive-Integer]))

(define-type Console-Command (U (List 'transform Symbol String (-> Journal Journal))
                                (List 'action Symbol String (-> Journal Void))
                                (List 'restore Symbol String (-> (Option Journal)))
                                (List 'quit Symbol String)))

(: transform-console-command (-> Symbol String (-> Journal Journal) (List 'transform Symbol String (-> Journal Journal))))
(define (transform-console-command key name proc) `(transform ,key ,name ,proc))
(: action-console-command (-> Symbol String (-> Journal Journal) (List 'action Symbol String (-> Journal Journal))))
(define (action-console-command key name proc) `(action ,key ,name ,proc))
(: restore-console-command (-> Symbol String (-> Journal Journal) (List 'restore Symbol String (-> Journal Journal))))
(define (restore-console-command key name proc) `(restore ,key ,name ,proc))
(: quit-console-command (-> Symbol String (List 'quit Symbol String)))
(define (quit-console-command key name) `(quit ,key ,name))

(: default-console-commands (Listof Console-Command))
(define default-console-commands (list (list 'transform 'u "Undo" journal-undo)
                                       (list 'quit 'q "Quit")))

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
  (and (memf (lambda ([c : Console-Command]) (eq? 'quit (first c)))
             (%console-config-commands config))
       #t))

(: console-config-trace-display? (-> Console-Config Boolean))
(define (console-config-trace-display? config)
  (case (%console-config-trace-display config)
    [(show) #t]
    [(hide) #f]))

(: console-run (All (S) (-> (Model S) [#:journal Journal] [#:config Console-Config] Journal)))
(define (console-run m #:journal [j '()] #:config [config (console-config)])
  (define gs (model-graphs m))
  (define-values (n st _) (replay m j))
  (define-values (call-with-emitter emit)
    ((inst make-emitter (Pairof Prompt-Value Prompt-Attributes) S)))
  (define-values (_n _st result-j)
    (let loop : (Values (Node S) S Journal) ([n n] [st st] [j : Journal j])
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
                   (cons (auto-journal-entry (edge-name chosen-edge) #:prompt-records ps) j)))]
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
                          (cons (choose-journal-entry (edge-name chosen-edge)
                                                      #:edge-attributes '()
                                                      #:prompt-records ps)
                                j))]
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

(: console-step (All (S) (-> Console-Config S (Edge S) (-> (Pairof Prompt-Value Prompt-Attributes) Void) S)))
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
                        (%console-config-commands config))
                 => console-command->command]
                [else (retry)]))))))

(: edge-weight (All (S) (-> (Edge S) Positive-Integer)))
(define (edge-weight e)
  (cond [(findf weight-edge-option? (edge-edge-options e))
         => (lambda (opt)
              (assert opt weight-edge-option?)
              (weight-edge-option-weight opt))]
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
