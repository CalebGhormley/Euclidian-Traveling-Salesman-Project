#lang racket
(define (genTours n)
  (permutate (trevlist (makeList n) '()) 0)
  )

(define (makeList n)
  (if (= n 0)
      null
      (cons n (makeList (- n 1)))))

(define (trevlist list acc)
  (if (null? list)
      acc
      (trevlist (cdr list) (cons (car list) acc))))

(define (permutate list i)
  (if (< i (factorial (length (cdr list))))
      (cons (cons (car list) (permutation (cdr list) i 0)) (permutate list (+ i 1)))
      list)
)

(define (permutation list n a)
  (if (> a (length list))
      (= a 1)
      (= a a)
      )
  (if (> n 0)
      (permutation (swapAt list a) (- n 1) (+ a 1))
      list))

(define (swapAt list a)
  (if (> a 1)
      (cons (car list) (swapAt (cdr list) (- a 1)))
      (cons (car (cdr list)) (cons (car list) (cdr (cdr list))))))


(define (factorial n)
  (if (> n 1)
      (* n (factorial (- n 1)))
      1))