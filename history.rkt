#lang typed/racket

(require "prompt.rkt")
(provide History History-Record
         (struct-out history-choose) History-Choose
         (struct-out history-prompt) History-Prompt
         Journal history->journal)

(struct history-choose ([mode : (U 'choose 'auto)]
                        [edge : String]
                        [prompt : String])
  #:transparent
  #:type-name History-Choose)

(struct history-prompt ([value : Prompt-Value]
                        [text : String])
  #:transparent
  #:type-name History-Prompt)

(define-type History-Record (U History-Choose History-Prompt))
(define-type History (Listof History-Record))

(define-type Journal (Listof (U (List (U 'choose 'auto) String)
                                (List 'prompt Prompt-Value))))

(: history->journal (-> History Journal))
(define (history->journal h)
  (map (lambda ([x : (U History-Choose History-Prompt)])
         (cond [(history-choose? x)
                (list (history-choose-mode x) (history-choose-edge x))]
               [(history-prompt? x)
                (list 'prompt (history-prompt-value x))]))
       h))
