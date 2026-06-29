#lang typed/racket

(require "prompt.rkt")

(provide History History-Record
         History-Node make-history-node history-node?
         history-node-name history-node-desc history-node-attributes
         History-Edge make-history-edge history-edge?
         history-edge-mode history-edge-name history-edge-prompt history-edge-attributes
         History-Prompt make-history-prompt history-prompt?
         history-prompt-value history-prompt-text history-prompt-attributes
         Journal
         history->journal)

(define-type Attribute-Value (U Symbol String Integer Boolean))

(struct history-node ([name : String]
                      [desc : (Option String)]
                      [attributes : (Immutable-HashTable Symbol Attribute-Value)])
  #:prefab
  #:type-name History-Node)

(: make-history-node (->* (String (Option String)) ((Immutable-HashTable Symbol Attribute-Value)) History-Node))
(define (make-history-node name desc [attributes ((inst hash Symbol Attribute-Value))])
  (history-node name desc attributes))

(struct history-edge ([mode : (U 'choose 'auto)]
                      [name : String]
                      [prompt : String]
                      [attributes : (Immutable-HashTable Symbol
                                                         Attribute-Value)])
  #:prefab
  #:type-name History-Edge)

(: make-history-edge (->* ((U 'choose 'auto) String String) ((Immutable-HashTable Symbol Attribute-Value)) History-Edge))
(define (make-history-edge mode name prompt [attributes ((inst hash Symbol Attribute-Value))])
  (history-edge mode name prompt attributes))

(struct history-prompt ([value : Prompt-Value]
                        [text : String]
                        [attributes : (Immutable-HashTable Symbol Attribute-Value)])
  #:prefab
  #:type-name History-Prompt)

(: make-history-prompt (->* (Prompt-Value String) ((Immutable-HashTable Symbol Attribute-Value)) History-Prompt))
(define (make-history-prompt value text [attributes ((inst hash Symbol Attribute-Value))])
  (history-prompt value text attributes))

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
