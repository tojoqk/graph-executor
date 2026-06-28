#lang typed/racket

(require "prompt.rkt")
(provide History History-Record
         (struct-out history-edge) History-Edge
         (struct-out history-node) History-Node
         (struct-out history-prompt) History-Prompt
         Journal history->journal)

(struct history-node ([node : String]
                      [desc : (Option String)])
  #:transparent
  #:type-name History-Node)

(struct history-edge ([mode : (U 'choose 'auto)]
                      [name : String]
                      [prompt : String])
  #:transparent
  #:type-name History-Edge)

(struct history-prompt ([value : Prompt-Value]
                        [text : String])
  #:transparent
  #:type-name History-Prompt)

(define-type History-Record (U History-Edge History-Node History-Prompt))
(define-type History (Listof History-Record))

(define-type Journal (Listof (U (List (U 'choose 'auto) String)
                                (List 'prompt Prompt-Value))))

(: history->journal (-> History Journal))
(define (history->journal h)
  (filter-map (lambda ([x : (U History-Record)])
                (cond [(history-edge? x)
                       (list (history-edge-mode x) (history-edge-name x))]
                      [(history-prompt? x)
                       (list 'prompt (history-prompt-value x))]
                      [(history-node? x) #f]))
       h))
