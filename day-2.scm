(require srfi/1)

(define (read-input path)
    (map (λ (line)
            (map string->number
                 (string-split line " ")))
         (file->lines path)))

(define (diff xs)
  (map - (cdr xs) xs))

(define (monotonic? xs)
  (or (andmap positive? xs)
      (andmap negative? xs)))

(define (safe? report)
    (let ([ds (diff report)])
      (and (monotonic? ds)
           (andmap (λ (n) (<= 1 (abs n) 3))
                   ds))))

(define (remove-at-index id xs)
    (append (take xs id)
            (drop xs (+ 1 id))))

(define (safe-with-removal? report)
  (define (try-remove i)
    (cond
      ((>= i (length report)) #f)
      ((safe? (remove-at-index i report)) #t)
      (else (try-remove (+ i 1)))))
  (or (safe? report)
      (try-remove 0)))

(let ([reports (read-input "Downloads/input"))]
     (println (count safe? reports))
     (println (count safe-with-removal? reports)))
