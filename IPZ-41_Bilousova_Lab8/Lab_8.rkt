#lang racket

(display "Лаб8, завдання 3.1, Білоусова Наталія, ІПЗ-41/1\n")

(display "Визначіть максимальну степінь поліному:")
(define degree (read))

(define (create-polynom  degree )
  (for/list ((i (+ degree 1)))
             (add1 (random -100 100)))
  )


(define polynom (create-polynom degree))
(display polynom)
(newline)
(display "Створений поліном:")

(define (display-polynom polynom degree)  
  (if (= degree 1)
      (print (~a "(" (car polynom) ")*x+(" (car (cdr polynom)) ")"))
       
      ((display "(")
       (display (car polynom))       
       (display ")*x^")
       (display degree)
       (display "+")
       (display-polynom (cdr polynom) (- degree 1)))
      )  
  )              


;(display-polynom polynom degree)
(newline)
; чисельник и знаменник є поліномами, які тут просто списки коефіцієнтів

(define (extended-synthetic-division dividend divisor)
  (define out (list->vector dividend)) ; Скопіюйте чисельник для загального поліноміального
  ;ділення, нам потрібно нормалізувати,
  ;поділивши коефіцієнт на перший коефіцієнт знаменника
  (define normaliser (car divisor))
  (define divisor-length (length divisor)) 
  (define out-length (vector-length out)) 
 
  (for ((i (in-range 0 (- out-length divisor-length -1))))
    (vector-set! out i (quotient (vector-ref out i) normaliser))
    (define coef (vector-ref out i))
    (unless (zero? coef) ; марно множити, якщо коефіцієнт дорівнює 0
      (for ((i+j (in-range (+ i 1) ; при синтетичному діленні ми завжди пропускаємо перший коефіцієнт дільника,
                                   ; оскільки він використовується лише для нормалізації коефіцієнтів дільника
                           (+ i divisor-length))) 
            (divisor_j (in-list (cdr divisor))))  
        (vector-set! out i+j (+ (vector-ref out i+j) (* coef divisor_j -1))))))
  ; Отриманий результат містить як частку, так і залишок, залишок є розміром дільника
  ; (залишок обов’язково має той самий ступінь, що і дільник, оскільки це те, що ми не могли поділити від дільника),
  ; тому ми обчислюємо індекс, де це розділення є, і повертає частку та залишок.
 
  ; повернути частку, залишок
  (split-at (vector->list out) (- out-length (sub1 divisor-length))))
 
(module+ main
  (displayln "POLYNOMIAL DIVISION")
  (define N '(2 3 6 -4))
  (define D '(-1 0.5))
  (define-values (Q R) (extended-synthetic-division N D))
  (define Q1 (map (lambda (number)
         (* -1 number))
       Q))
  (printf "~a/~a = ~a remainder ~a~%" N D Q1 R)
  (if (= (car R) 0)
      "Число є коренем поліному"
      "Число не є коренем поліному"
  ))