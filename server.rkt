#lang racket/gui

(require racket/gui)
(require graph)
(require csv-reading)

(define connections (undirected-graph '() '()))
(define filenameConnections "london.connections.csv")
(define filenameStations "london.stations.csv")
(define nameLookup (make-hash))
(define selection1 "")
(define selection2 "")

(define mkCsvReader
    (make-csv-reader-maker
    '((separator-chars #\,)
    (strip-leading-whitespace?  . #t)
    (strip-trailing-whitespace? . #t)    
    )))

(define nextRowStations
    (mkCsvReader (open-input-file filenameStations)))
(define nextRowConnections
    (mkCsvReader (open-input-file filenameConnections)))



(define makeGraph
  (lambda ()
    (let ([row (nextRowConnections)])
      (if (empty? row)
        (get-edges connections)
      (begin
        (add-vertex! connections (car row))
        (add-vertex! connections (last row))
        (add-edge! connections (car row) (last row))
        (makeGraph)
      )))))

(define makeLookupTable
   (lambda ()
    (let ([row (nextRowStations)])
        (if (empty? row)
            nameLookup
        (begin
            (hash-set! nameLookup (car row) (list (second row)(third row)))
            (makeLookupTable)
   )))))

(define returnConnections 
    (lambda (x)
    (map (lambda (y) (hash-ref nameLookup y) ) x)
    ))


(define (getKey stationName)
  (for/first ([(k v) nameLookup]
    #:when (equal? stationName (car v)))
  k))


(nextRowConnections)
(nextRowStations)
(makeGraph)
(makeLookupTable)

 (define myframe (new frame%
                     [label "Journey Go - Route Finder Prototype 1.0"]
                     [width 500] [height 200]))
                     
(define choiceTo (new choice%
                    [label "Choose"] [parent myframe]
                    [choices (map first (hash-values nameLookup))]
                    [callback (lambda (c e) 
                      (set! selection1 (send c get-string-selection ))
                  
                    )]
))

(define choiceFrom (new choice%
                    [label "Choose"] [parent myframe]
                    [choices (map first (hash-values nameLookup))]
                    [callback (lambda (c e) 
                      (set! selection2 (send c get-string-selection ))

                    )]
))

(define routeButton (new button% 
                      [label "get Route"]
                      [parent myframe]
                      [callback (lambda (b e)
                        (send textFieldSelections set-value (string-append "Showing Route From:   " selection1 " -> " selection2))
                        (send textFieldRoute set-value (let 
                          ((station-strings (map first (returnConnections (fewest-vertices-path connections (getKey selection1) (getKey selection2))))))
                          (string-append
                          (apply string-append (drop-right (map (lambda (x) (string-append x " -> ")) station-strings) 1))
                          (last station-strings))
)))]
                      ))

(define textFieldSelections (new text-field% 
                    [label "Selections:"]
                    [parent myframe]
                    ))


(define textFieldRoute (new text-field% 
                    [label "Route:"]
                    [parent myframe]
                    ))



(send myframe show #t)












