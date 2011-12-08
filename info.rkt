#lang setup/infotab

(define name "Automatic compilation of dependancies")
(define blurb '("First release"))
(define primary-file "main.rkt")
(define categories '(devtools))
(define raco-commands '(("auto" "auto.rkt"
                         "runs a file and automatically compiles dependancies"
                         100)))
