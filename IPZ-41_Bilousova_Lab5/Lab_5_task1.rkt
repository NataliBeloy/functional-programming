#lang racket

(display "Лаб5, завдання 3.1, Білоусова Наталія, ІПЗ-41/1\n")

(define (numerator x) (car x)) ; чисельник
(define (denominator x) (cdr x)) ; знаменник
(define (create-fraction n d) (cons n d)); створення пари

(define(print-fraction x) ; друк пари 
  (display (numerator x))
  (display "/")
  (display (denominator x)))

(define (add-fraction x y)
  (make-rat (+ (* (numerator x) (denominator y)
                    )
                   (* (numerator y) (denominator x)
                    )
                 )
                 (* (denominator x) (denominator y)
                 )
       )
    )

(define (divide-fraction x y)
        (create-fraction (* (numerator x) (denominator y))
                          (* (denominator x) (numerator y))))

(define (make-rat n d)
    (let ((nod (gcd n d)))
         (cons (/ n nod) (/ d nod))))

(define quattra (create-fraction 1 4))
(define half-of-geese (create-fraction 1 2))
(define all-part (create-fraction 2 1))
(define all-geese-value 99)

(newline)

; друкуємо рівняння, яке необхідно розв'язати
(display "Сформоване рівняння згідно поставленої задачі: ")
(begin
  (display "(")(print-fraction all-part)(display ")x + ") ;дві повні частини
  (display "(")(print-fraction half-of-geese)(display ")x + ") ; x - половина кількості гусей
  (display "(")(print-fraction quattra)(display ")x") ; x - чверть кількості гусей
  (display " = ")
  (display all-geese-value) ; 100 гусей мінус 1
)
(newline)

(define add-value (add-fraction quattra half-of-geese))
(define s-add-value (add-fraction add-value all-part))
; друкуємо результат після додавання
(display "Результат після додавання дробів: ")
(begin
  (display "(") (print-fraction s-add-value)(display ")x") ; x - сумма усіх дробів
  (display " = ")  
  (display all-geese-value)
)
(define x (divide-fraction (create-fraction  all-geese-value  1) s-add-value))
(newline)
(display "Знайдена початкова кількість гусей: ")
(begin
  (display "x = ")
  (print-fraction x)
  (display " = ")
  (display (/ (numerator x) (denominator x)))
)

