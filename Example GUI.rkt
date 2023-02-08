#lang racket/gui
 (define myframe (new frame%
                     [label "My window"]
                     [width 500] [height 400]))
(send myframe show #t)

(define butt (new button% [parent myframe]
                  [label "Click me"]))
(define check-box (new check-box%
                       [label "Check me"] [parent myframe]
                       [value #t]))
(define choice (new choice%
                    [label "Choose"] [parent myframe]
                    [choices (list "First" "Second" "Third")]))
(define slider (new slider%
                    [label "pinguin"] [parent myframe]
                    [min-value 0] [max-value 100]))
(define text-field (new text-field%
                        [label "Text me"] [parent myframe]))