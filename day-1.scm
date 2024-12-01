(define (read-input path)
    (map (lambda (s)
           (map string->number
                (string-split s "   ")))
         (file->lines path)))

(define (part-1 in)
  (apply +
    (map (lambda (x)
           (abs (- (car x)
                   (cdr x))))
         (map cons
              (sort (map car in) <)
              (sort (map cadr in) <)))))

(define (frequencies lst)
    (foldl (lambda (val acc)
             (hash-update acc val add1 0))
           (hash) lst))

(define (part-2 in)
    (let* ([l-side (car in)]
           [r-side (cadr in)]
           [r-freq (frequencies r-side)])
      (apply +
        (map (lambda (l-val)
          (* l-val (hash-ref r-freq l-val 0)))
          l-side))))
