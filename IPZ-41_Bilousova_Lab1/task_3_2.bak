#lang racket
(display "Білоусова, ІПЗ-41, лабораторна робота №1")
(define (enter-num)
  (display "Please, enter a number:")
  (let ((num (read)))
    (if (integer? num); вводимо початкове число
        num
        (enter-num))))

(define k 2)
(define t 2)

(define (primes n k); функцыя для знаходження найменших дільників

  (when (= (remainder n k) 0); якщо остача від ділення = 0, тоді виводимо дільник 
      (display k)
    (display " ")
      (set! t k)
      (set! k 2)
      (primes (quotient n t) k))
  
  (when (not (equal? (remainder n k) 0)) ;якщо остача від ділення не дорівнює 0, 
      (set! k (add1 k))                  ;тоді до k додаємо 1 і перевіряємо його
      (primes n k))
  )
(define (main); головна функція
  (let ((n (enter-num)))
    (display (primes n k))
    (newline)                
      )
  )

(newline)
(main)
(newline)
