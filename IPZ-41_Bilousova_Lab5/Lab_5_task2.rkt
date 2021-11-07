#lang racket

(display "Лаб5, завдання 3.2, Білоусова Наталія, ІПЗ-41/1\n")

(define (real-part z) (car z))
(define (imagine-part z) (cdr z))

(define (create-complex-value a b) (cons a b))

(define (create-complex-list input-list)
  (if (null? input-list)
    '()  
    (append (list
             (string->number (~a (real-part (car input-list)) "+" (imagine-part (car input-list)) "i"))
             )
            (create-complex-list (cdr input-list))))
)
(newline)

; користувацький список комплексних чисел в алгебраїчній формі
(define complex-list (create-complex-list(list
                      (create-complex-value 5 3)
                      (create-complex-value 4 2)
                      (create-complex-value 6 3)
                      (create-complex-value 9 1)
                      (create-complex-value 6 8))))

(display "Користувацький список комплексних чисел (алгебраїчна форма): ")
(display complex-list)

(newline)
(display "Сума всіх елементів: ")
(display (apply + complex-list))