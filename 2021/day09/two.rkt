#lang racket

(define (char->digit c) (- (char->integer c) (char->integer #\0)))
(define (parse-line line) (map char->digit (string->list line)))

(define (parse-input lines)
  (let* {
    [ rows (map parse-line lines) ]
    [ height (length rows) ]
    [ width (length (first rows)) ]
    [ indices (map [λ (i) [list (remainder i width) (quotient i width)] ] [range (* width height)]) ]
  }
    (make-hash [map list indices (flatten rows)])
  )
)

(define height-map (parse-input (file->lines "input.in")))

(define offsets '[(-1 0) (1 0) (0 -1) (0 1)])
(define (apply-offset pos offset) (map + pos offset))
(define (height-at pos) (car (hash-ref height-map pos '(9))))

(define (neighbour-offsets pos) (map (λ (offset) (apply-offset pos offset)) offsets))
(define (neighbours pos) 
  (map 
    [λ (pos) {list pos (height-at pos)}] 
    (neighbour-offsets pos)
  )
)

(define (low-point? pos) 
  (let {
    [ height (car (hash-ref height-map pos)) ]
  }
    (andmap (λ (neighbour) (< height (second neighbour)))  (neighbours pos))
  )
)

(define low-points (filter low-point? (hash-keys height-map)))

(define (basin-size pos)
  (define seen (mutable-set pos))
  (let dfs ([pos pos])
    (set-add! seen pos)
    (add1
      (for/sum {
        [pos (in-list (neighbour-offsets pos))] 
        #:unless (set-member? seen pos)
        #:unless (= (height-at pos) 9)
      } 
        (dfs pos)
      )
    )
  ))

(println (apply * (take (sort (map basin-size low-points) >) 3)))