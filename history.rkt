#lang typed/racket

(require "prompt.rkt")

(provide History History-Record
         History-Node make-history-node history-node?
         history-node-name history-node-desc history-node-attributes
         History-Edge make-history-edge history-edge?
         history-edge-mode history-edge-name history-edge-prompt history-edge-attributes
         History-Prompt make-history-prompt history-prompt?
         history-prompt-value history-prompt-text history-prompt-attributes
         History-Message make-history-message history-message? history-message-content history-message-attributes
         Journal history->journal)

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

(struct history-message ([content : String]
                         [attributes : (Immutable-HashTable Symbol Attribute-Value)])
  #:prefab
  #:type-name History-Message)

(: make-history-message (->* (String) ((Immutable-HashTable Symbol Attribute-Value)) History-Message))
(define (make-history-message val [attributes ((inst hash Symbol Attribute-Value))])
  (history-message val attributes))

(define-type History-Record (U History-Edge History-Node History-Prompt History-Message))
(define-type History (Listof History-Record))

(define-type Journal-Record (Pairof String (Listof Prompt-Value)))
(define-type Journal (Listof Journal-Record))

(: take-to-choose (-> (Pairof History-Record (Listof History-Record))
                      (Values String (Listof Prompt-Value) History)))
(define (take-to-choose rs)
  (let loop ([rs rs] [ps : (Listof Prompt-Value) '()])
    (let ([fst (car rs)]
          [rst (cdr rs)])
      (cond [(history-edge? fst) (values (history-edge-name fst) ps rst)]
            [(history-prompt? fst)
             (if (null? rst)
                 (error 'history->journal "invalid history")
                 (loop (cdr rs) (cons (history-prompt-value fst) ps)))]
            [(or (history-node? fst)
                 (history-message? fst))
             (loop (cdr rs) ps)]))))

(: history->journal (-> History Journal))
(define (history->journal rs)
  (if (null? rs)
      '()
      (let loop ([rs : (Pairof History-Record (Listof History-Record)) rs]
                 [acc : Journal '()])
        (define-values (e ps rest-rs) (take-to-choose rs))
        (if (null? rest-rs)
            ((inst cons Journal-Record Journal) (cons e ps) acc)
            (loop rest-rs ((inst cons Journal-Record Journal) (cons e ps) acc))))))
