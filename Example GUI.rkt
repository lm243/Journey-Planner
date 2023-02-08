#lang racket/gui
 (define myframe (new frame%
                     [label "My window"]
                     [width 500] [height 400]))
                     
(define choice (new choice%
                    [label "Choose"] [parent myframe]
                    [choices (list "First" "Second" "Third")]
                    [callback (lambda (c e) (displayln (send c get-string-selection ))
                    )]
))

(send myframe show #t)


