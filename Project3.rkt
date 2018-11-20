 #lang racket

(define (genTours n)
  (permutate (trevlist (makeList n) '()) 0))

(define (makeList n)
  (if (= n 0)
      null
      (cons n (makeList (- n 1)))))

(define (trevlist list acc)
  (if (null? list)
      acc
      (trevlist (cdr list) (cons (car list) acc))))

(define (permutate list n)
  (if (< n (factorial (length (cdr list))))
      (cons (cons (car list) (permutation (cdr list) n 1 (length (cdr list)))) (permutate list (+ n 1)))
      null))

(define (permutation list n a size)
  (cond
    [(< n 1) list]
    [(= a size) (permutation list n 1 size)]
    [#t (permutation (swapAt list a) (- n 1) (+ a 1) size)]))

(define (swapAt list a)
  (if (> a 1)
      (cons (car list) (swapAt (cdr list) (- a 1)))
      (cons (car (cdr list)) (cons (car list) (cdr (cdr list))))))

(define (factorial n)
  (if (> n 1)
      (* n (factorial (- n 1)))
      1))

(define (score points)
  (calcDistance points (car points) 0))

(define (calcDistance points start acc)
  (if (null? (cdr points))
      (+ acc (sqrt (+ (* (- (car (car points)) (car start)) (- (car (car points)) (car start))) (* (- (car (cdr (car points))) (car (cdr start))) (- (car (cdr (car points))) (car (cdr start)))))))
      (calcDistance (cdr points) start (+ acc (sqrt (+ (* (- (car (car points)) (car (car (cdr points)))) (- (car (car points)) (car (car (cdr points))))) (* (- (car (cdr (car points))) (car (cdr (car (cdr points))))) (- (car (cdr (car points))) (car (cdr (car (cdr points))))))))))))

(define (etsp points)
  (calcShortest (permutate points 0) (genTours (length points)) 10000 '()))

(define (calcShortest points lists value best)
  (if (null? (cdr lists))
      (if (< (score (car points)) value)
          (car lists)
          best)
      (if (< (score (car points)) value)
          (calcShortest (cdr points) (cdr lists) (score (car points)) (car lists))
          (calcShortest (cdr points) (cdr lists) value best))))
