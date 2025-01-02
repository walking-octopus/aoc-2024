(define (read-puzzle-input path)
  (define (string->vector str)
    (list->vector (string->list str)))
  (call-with-input-file path
    (λ (in)
       (let loop [(lines empty)]
         (let [(line (read-line in 'any))]
           (if (eof-object? line)
               (list->vector (reverse lines))
               (loop (cons (string->vector line) lines))))))))

(define (matrix-ref board x y)
    (and (>= x 0)
         (>= y 0)
         (< y (vector-length board))
         (< x (vector-length (vector-ref board y)))
         (vector-ref (vector-ref board y) x)))

(define (matrix-find board symbol)
  (for*/list ([y (in-range (vector-length board))]
              [x (in-range (vector-length (vector-ref board 0)))]
              #:when (eq? (matrix-ref board x y) symbol))
             (list x y)))

(define *4-way*
  '((0 1) (1 0)
    (0 -1) (-1 0)))

(define *diagonals*
  '((1 1) (1 -1)
    (-1 -1) (-1 1)))

(define *8-way*
  (append *4-way* *diagonals*))

(define (flatten-once xs)
  (apply append xs))

(define (probe board pos0 dirs0 seek)
  (define (try pos dirs seek)
    (cond
      [(empty? seek)
       (list (list pos0 pos))]
      [(eq? (car seek)
            (apply matrix-ref board pos))
       (flatten-once
         (map (λ (dir)
           (try (if (empty? (cdr seek)) pos (map + dir pos))
                (list dir)
                (cdr seek)))
           dirs))]
      [else '()]))
  (try pos0 dirs0 seek))

(define (count-xmas board)
  (length
    (flatten-once
      (map (λ (pos) (probe board pos
                           *8-way*
                           '(#\X #\M #\A #\S)))
           (matrix-find board #\X)))))

(define (midpoint a b)
  (match (list a b)
    [(list (list ax ay) (list bx by))
     (list (/ (+ ax bx) 2)
           (/ (+ ay by) 2))]))

(define (cross-product v1 v2)
  (- (* (first v1)  (second v2))
     (* (second v1) (first v2))))

;; WIP
(define (count-x-mas board)
  (length (remove-duplicates (map (curry apply midpoint)
          (flatten-once (map
            (λ (pos) (probe board pos
                            *diagonals*
                            '(#\M #\A #\S)))
            (matrix-find board #\M)))))))

(let [(puzzle (read-puzzle-input "Downloads/input"))]
     (printf "Part 1 ~a~n" (count-xmas  puzzle))
     (printf "Part 2 ~a~n" (count-x-mas puzzle)))
