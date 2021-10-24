#lang racket

(display "Білоусова Наталія, ІПЗ-41, Лабораторна робота 3")
(newline)
(display "===============================================")
(newline)

(define a 0)
(define b 1)
(define n 10000)
(define sum 0.0)

(define (f_step a b n)
  (/ (- b a) n)
  )

(define (integ x)
  (/ 1 (+ 1 (* x x)))
  )

(define (find_sum_left step count h sum)
  (if (= count 1);метод лівих прямокутників
      sum
      (find_sum_left (+ step h) (- count 1) h (+ sum (integ step)))
      )
  )

(define (find_sum_right step count h sum)
  (if (= count 0)
      sum
      (find_sum_right (+ step h) (- count 1) h (+ sum (integ step)))
      )
  )

(define (find_sum_middle step count h sum n)
  (if (or (= count n) (= count 1))
      (find_sum_middle (/ (+ step h) 2) (- count 1) h (+ sum (integ step)) n)
      (if (= count 0);метод трапецій
      sum
      (find_sum_middle (+ step h) (- count 1) h (+ sum (integ step)) n))))       


(* (f_step a b n) (find_sum_left a n (f_step a b n) sum));метод лівих прямокутників
  
(* (f_step a b n) (find_sum_right (+ a (f_step a b n)) n (f_step a b n) sum));метод правих прямокутників

(* (f_step a b n) (find_sum_right (+ a (f_step a b (* n 2))) n (f_step a b n) sum)) ; метод середніх прямокутників

(* (f_step a b n) (find_sum_middle (+ a (f_step a b n)) n (f_step a b n) sum n));метод трапецій

