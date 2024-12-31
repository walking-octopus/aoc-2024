(define (read-input path)
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

(define *8-ways*
  '((0 1) (1 1)
    (1 0) (1 -1)
    (0 -1) (-1 -1)
    (-1 0) (-1 1)))

(define (probe board pos0 seek)
  (define (loop pos dirs seek)
    (cond
      [(empty? seek)
       (list (list pos0 pos))]
      [(eq? (car seek) (apply matrix-ref board pos))
       (apply append
         (map (λ (dir)
                 (loop (map + dir pos)
                       (list dir)
                       (cdr seek)))
              dirs))]
      [else '()]))
  (loop pos0 *8-way* seek))

(define (count-xmas crossword)
  (length
    (apply append (map
      (λ (pos) (or (probe crossword pos '(#\X #\M #\A #\S))
                   (probe crossword pos '(#\S #\A #\M #\X))))
      (append (matrix-find crossword #\X)
              (matrix-find crossword #\S))))))

(printf "Part ~a ~a" 1 (count-xmas (read-input "Downloads/input")))
