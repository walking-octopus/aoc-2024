(define (parse-input raw-input)
  (let* [(raw-parts (string-split raw-input "\n\n"))
         (cmps (map (compose (curry map string->number)
                             (curryr string-split "|"))
                    (string-split (first raw-parts) "\n")))
         (xs (map (compose (curry map string->number)
                             (curryr string-split ","))
                    (string-split (second raw-parts) "\n")))]
    (values cmps xs)))

(define (list-middle xs)
  (list-ref xs (quotient (length xs) 2)))

(define (build-hash xs key-fn val-fn)
  (foldl (λ (x h)
          (let* ((key (key-fn x))
                 (val (val-fn x))
                 (existing (hash-ref h key '{})))
            (hash-set h key (cons val existing))))
        (make-immutable-hash) xs))

(define (cmps->succ cmps)
  (build-hash cmps first second))

(define (ordered? S xs)
  (for*/and [((x i) (in-indexed xs))
             (y (hash-ref S x '()))]
    (let [(j (or (index-of xs y) (+ 1 i)))]
         (< i j))))

(define (part-1 succ xs)
  (apply +
    (map list-middle
      (filter (curry ordered? succ)
              xs))))

(define (relative-sort S xs)
  (define (last-item xs)
    (findf (lambda (x)
             ((compose not ormap)
              (compose (curry member x)
                       (curry hash-ref S empty))
             xs)) xs))
  (define (sort-items xs sorted)
    (if (empty? xs) sorted
        (let [(x (last-item xs))]
          (sort-items (remove x xs)
                      (cons x sorted)))))
  (sort-items xs empty))

(define (part-2 succ xs)
  (apply +
    (map (compose list-middle (curry relative-sort succ))
      (filter-not (curry ordered? succ)
                  xs))))

(let*-values [((cmps xs)
               ((compose parse-input file->string)
                "Downloads/input"))
              ((succ) (cmps->succ cmps))]
  (printf "Part 1: ~a~n" (part-1 succ xs))
  (printf "Part 2: ~a~n" (part-2 succ xs)))
