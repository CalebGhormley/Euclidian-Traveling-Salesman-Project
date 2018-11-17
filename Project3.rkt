#lang racket
(define (trevlist list acc)
  (if (null? list)
      acc
      (trevlist (cdr list) (cons (car list) acc))
                )
      )

(define (makeList n)
  (if (= n 0)
      null
      (cons n (makeList (- n 1)))
  )
)

(define (permutate list)
  (if (null? (cdr list))
      null
      (cons (cons '1 list) (cons (car (cdr list)) (cons '1 (cons (car list) (cdr (cdr list))))))
      ) 
)

(define (remove list acc)
  (if (null? list)
      null
      (cons (cdr list) (cons (car list) acc))))

(define (genTours n)
  (permutate (cdr (trevlist (makeList n) '())))
  )

;;This function returns lists for every possible position
;;of the first element, leaving the other elements unmoved.
(define (walk list)
  (cond
    [ (null? (cdr list)) null ]
    [ #t (cons (car (cdr list)) (cons (car list) (cdr (cdr list))))]
    )
  )