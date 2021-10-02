
#lang racket
(display "Білоусова, ІПЗ-41, лабораторна робота No1")
(define (enter-num); вводимо два початкові числа
  (display "Please, enter a number - first number>second number:")
  (let ((num (read)))
    (if (integer? num)
        num
        (enter-num))))
(define recursion-depth 0); задаємо початкову глибину рекурсії
(define (ackermann m n d); функція Аккермана
  (set! recursion-depth (+ d 1))
  (cond [(zero? m) (add1 n)]
        [(zero? n) (ackermann (sub1 m) 1 (+ d 1))]
        [else (ackermann (sub1 m) (ackermann m (sub1 n) (+ d 1)) (+ d 1))])
  )
(define (main);головна функція
  (let ((m (enter-num)) (n (enter-num)))
    (display (ackermann m n 0))
    (newline)
    (display recursion-depth)
      )
  )
(newline)
(main)  ;запуск програми
(newline)
