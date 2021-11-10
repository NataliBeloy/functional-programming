#lang racket

(display "Лаб6, завдання 3.1, Білоусова Наталія, ІПЗ-41/1\n")

(define vec1  (vector -3 2 -4 100 1 5 -10 -2 101 4 -7 9 6))
(define vec2  (vector 4 5 -4 101 1 5 -10 -7 11 5 -7 8 6))
(define k (- (vector-length vec1) 1))
(define ks 0)
(define (find-same vec1 vec2 k ks)
  (if (= k 0)
      ks
      (if (vector-member (vector-ref vec1 k)  vec2)
          (begin
            (set! ks (+ ks 1))
            (find-same vec1 vec2 (- k 1) ks)
          )
          (find-same vec1 vec2 (- k 1) ks)))
  )
(define same-count (find-same vec1 vec2 k ks))
(define vec-res (make-vector same-count 1))

(define (find-result-vector vec1 vec2 vec-res k ks)
  (if (= k 0)
      vec-res
      (if (vector-member (vector-ref vec1 k)  vec2)
          (begin
            (vector-set! vec-res ks (vector-ref vec1 k))
            (set! ks (+ ks 1))
            (find-result-vector vec1 vec2 vec-res (- k 1)  ks)
          )
          (find-result-vector vec1 vec2 vec-res (- k 1) ks)))
  )
      

(define result (find-result-vector vec1 vec2 vec-res k ks))
  
(vector-sort! result <)

;(display (vector-ref result 0))

(display "=============================================")(newline)
(display "Користувацький вектор 1: ")(display vec1)(newline)
(display "Користувацький вектор 2: ")(display vec2)(newline)
(display "Вектор, який сформований з однаковий елементів: ")(display result)(newline)
(display "Найменший елемент: ")(display (vector-ref result 0))
