#lang typed/racket

(require "../../private/executor/console.rkt")

(provide console-choose console-command-dispatch
         Console-Command console-command?
         transform-console-command action-console-command restore-console-command quit-console-command
         Console-Config console-config
         random-edge-option
         default-console-commands default-console-chooser
         transform-console-command? transform-console-command
         action-console-command? action-console-command
         restore-console-command? restore-console-command
         quit-console-command? quit-console-command)


