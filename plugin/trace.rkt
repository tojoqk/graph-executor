#lang typed/racket

(require "../private/trace.rkt")

(provide Node-Record node-record node-record? node-record-node-id node-record-node-info node-record-events
         Edge-Record edge-record edge-record-edge-id edge-record-edge-info edge-record-events
         Auto-Edge-Record auto-edge-record? auto-edge-record
         Choose-Edge-Record choose-edge-record? choose-edge-record choose-edge-record-prompt choose-edge-record-choices choose-edge-record-extra
         Trace Record
         trace->journal)
