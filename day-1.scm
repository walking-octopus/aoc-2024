(define (read-input path)
    (apply map list
        (map (lambda (s)
               (map string->number
                    (string-split s "   ")))
             (file->lines path))))

(define (part-1 l-side r-side)
  (apply + 
    (map (compose abs -)
         (sort l-side <) 
         (sort r-side <))))

(define (frequencies lst)
    (foldl (lambda (val acc)
             (hash-update acc val add1 0))
           (hash) lst))

(define (part-2 l-side r-side)
      (let* ([freqs-r (frequencies r-side)]
             [r-freq (lambda (x) (hash-ref freqs-r x 0))])
        (apply +
          (map (lambda (l-val)
                 (* l-val (r-freq l-val)))
               l-side))))

(let* ([input (read-input "Downloads/input")]
       [l-side (car input)]
       [r-side (cadr input)])
  (println (part-1 l-side r-side))
  (println (part-2 l-side r-side)))
