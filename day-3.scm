(define (lexer input)
    (map (lambda (c)
           (if (not (cadr c))
               (list (string->symbol (car c)))
               (map (lambda (f x) (f x))
                    {list string->symbol
                          string->number
                          string->number}
                     c)))
         (regexp-match*
          #px"(mul|don't|do)\\(((\\d+),(\\d+))?\\)"
          input
          #:match-select
          (lambda (match)
            (map (curry list-ref match)
                 '{1 3 4})))))

(define (eval-puzzle acc do? cmds)
    (if (empty? cmds)
        acc
        (let* ([cmd (car cmds)]
               [op (car cmd)])
          (case op
            [(mul)
             (eval-puzzle
              (+ (if do? (apply * (cdr cmd)) 0) acc)
              do?
              (cdr cmds))]
            [else
             (eval-puzzle acc (eq? op 'do) (cdr cmds))]))))

(define (part-1 cmds)
    (eval-puzzle
     0 #t
     (filter (lambda (e) (eq? (car e) 'mul))
             cmds)))

(define (part-2 cmds)
  (eval-puzzle 0 #t cmds))

(let* ([puzzle-raw (file->string "Downloads/input")]
       [puzzle (lexer puzzle-raw)])
  (printf "Part 1: ~a~n" (part-1 puzzle))
  (printf "Part 2: ~a~n" (part-2 puzzle)))
