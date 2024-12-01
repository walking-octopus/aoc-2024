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
    (let ([r-freq (frequencies r-side)])
      (apply +
        (map (lambda (l-val)
          (* l-val (hash-ref r-freq l-val 0)))
          l-side))))

(let*-values ([(input) (read-input "Downloads/input")]
              [(l-side r-side)
               (values (car *input*) (cadr *input*))])
  (println (part-1 l-side r-side))
  (println (part-2 l-side r-side)))
