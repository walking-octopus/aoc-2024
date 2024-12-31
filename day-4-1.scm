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

(define *4-way*
  '((0 1) (1 0)
    (0 -1) (-1 0)))

(define *diagonals*
  '((1 1)
    (1 -1)
    (-1 -1)
    (-1 1)))

(define *8-way*
  (append *4-way* *diagonals*))

(define (probe board pos0 dirs0 seek)
  (define (try pos dirs seek)
    (cond
      [(empty? cdr seek)
       (list (list pos0 pos))]
      [(eq? (car seek)
            (apply matrix-ref board pos))
       (apply append
         (map (λ (dir)
                 (try (map + dir pos)
                      (list dir)
                      (cdr seek)))
              dirs))]
      [else '()]))
  (try pos0 dirs0 seek))

(define (count-xmas crossword)
  (length
    (apply append (map
      (λ (pos) (probe crossword pos *8-way* '(#\X #\M #\A #\S)))
      (matrix-find crossword #\X)))))

(define (midpoint a b)
  (match (list a b)
    [(list (list ax ay) (list bx by))
     (list (/ (+ ax bx) 2) (/ (+ ay by) 2))]))

;; FIXME: WIP
(define (count-x-mas crossword)
  (length
    (remove-duplicates (map (curry apply midpoint)
      (apply append (map
        (λ (pos) (probe crossword pos *diagonals* '(#\M #\A #\S)))
        (matrix-find crossword #\M)))))))

(printf "Part ~a ~a~n" 1 (count-xmas  (read-input "Downloads/input")))
(printf "Part ~a ~a~n" 2 (count-x-mas (read-input "Downloads/input")))
