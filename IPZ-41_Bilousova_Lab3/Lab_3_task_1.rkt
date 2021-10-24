#lang racket
(display "Білоусова Наталія, ІПЗ-41, Лабораторна робота 3")
(newline)
(display "===============================================")
(newline)
(display "         Знаходимо х методом перебору          ")
(newline)
(define a 0)
(define b 1)
(define h 0.0001)
(define temp1 1)
(define temp2 1)
; метод перебору
(define (find_x a h)
  (let(
      (find_F (func a))
      (find_F1 (func (+ a h)))
      )
    
  (if (>(* find_F find_F1) 0)
      (find_x (+ a h) h)      
      a)
  ))
(define (func x)
  (- (exp (* x -1)) (* x x))
  )
(display "Відповідь: ")
(find_x a h)
(newline)
(display "===============================================")
(newline)
(display "           Знаходимо х методом хорд            ")
(newline)
;метод хорд
(define (find_hord a b h)
  (if (< (func (find_c a b)) h)
  (find_c a b)
  (if (< (* (func a) (func (find_c a b))) 0)
      (find_hord a (find_c a b) h)
      (find_hord (find_c a b) b h))
  ))
(define (find_c a b)
  (+ a (* (abs (/ (func a) (- (func a) (func b)))) (- b a)))
  )

  
(display "Відповідь: ")
(find_hord a b h)
(newline)
(display "===============================================")
(newline)
(newline)
(display "Різниця результатів: ")

(abs (- (find_x a h) (find_hord a b h)))
(newline)
(display "===============================================")



  
  