#lang typed/racket

(require "prompt.rkt")
(provide History
         (struct-out history-choose) History-Choose
         (struct-out history-prompt) History-Prompt
         Journal history->journal)

(struct history-choose ([mode : (U 'choose 'auto)]
                        [edge-name : String]
                        [edge-desc : (Option String)]
                        [graph-name : String]
                        [node-name : String]
                        [node-desc : (Option String)])
  #:transparent
  #:type-name History-Choose)

(struct history-prompt ([value : Prompt-Value]
                        [title : String])
  #:transparent
  #:type-name History-Prompt)

(define-type History (Listof (U History-Choose History-Prompt)))

(define-type Journal (Listof (U (List (U 'choose 'auto) String)
                                (List 'prompt Prompt-Value))))

(: history->journal (-> History Journal))
(define (history->journal h)
  (map (lambda ([x : (U History-Choose History-Prompt)])
         (cond [(history-choose? x)
                (list (history-choose-mode x) (history-choose-edge-name x))]
               [(history-prompt? x)
                (list 'prompt (history-prompt-value x))]))
       h))
