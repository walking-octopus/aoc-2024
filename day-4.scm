(define (read-puzzle-input path)
  (call-with-input-file path
    (λ (port)
      (for/vector ([line (in-lines port)])
        (for/vector ([c (in-string line)]) c)))))

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
           (try (if (empty? (cdr seek))
                    pos
                    (map + dir pos))
                (list dir)
                (cdr seek)))
           dirs))]
      [else '()]))
  (try pos0 dirs0 seek))

(define (search-word word dirs board)
  (flatten-once
    (map (λ (pos) (probe board pos dirs word))
         (matrix-find board (car word)))))

(define (count-xmas board)
  (length
    (search-word '(#\X #\M #\A #\S)
                 *8-way* board)))

(define (midpoint a b)
  (match (list a b)
    [(list (list ax ay) (list bx by))
     (list (/ (+ ax bx) 2)
           (/ (+ ay by) 2))]))

(define (build-hash xs f)
  (foldl (λ (x h) (let*
           [(key (f x))
            (vals (hash-ref h key '()))]
            (hash-set h key (cons x vals))))
         (make-immutable-hash) xs))

(define (count-x-mas board)
  (apply + (hash-map
    (build-hash
      (search-word '(#\M #\A #\S)
                   *diagonals* board)
      (curry apply midpoint))
    (λ (k v) (quotient (length v) 2)))))

(let [(puzzle (read-puzzle-input "Downloads/input"))]
     (printf "Part 1 ~a~n" (count-xmas  puzzle))
     (printf "Part 2 ~a~n" (count-x-mas puzzle)))
