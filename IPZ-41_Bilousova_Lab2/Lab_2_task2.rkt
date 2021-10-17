#lang racket

(define (fraction precision) 
  (+ 3 (fraction_step 1 precision)) ;ціла частина дробу = 3
  )

(define (fraction_step k precision)
  (let(
       (div_result_for_n (divider 1 1 k)); розрахунок для поточного кроку
       (div_result_for_n1 (divider 1 1 (+ 1 k))); розрахунок для наступного кроку
       )
    (if (> (abs (- div_result_for_n1 div_result_for_n)) precision); якщо різниця між n та n+1 менша
        (fraction_step(+ 1 k) precision)                          ; за точність, тоді пошук припиняється
        div_result_for_n
        )
    )
  )

(define (divider numerator step n_precision)
  (let (
        (divider_part (* (+ step 2) (+ step 2)));чисельник підводимо в квадрат на збільшуємо на 2
        )
    (if (= n_precision 0); поки кількість кроків не дорівнює нулю продовжуємо ділити
        divider_part
        (/ numerator (+ 6 (divider divider_part (+ 2 step) (- n_precision 1))))
        )
    )
)
  
(newline)
(display "Обчислити нескінчений ланцюговий дріб, задавши значення точності")
(newline)
(display "Введіть точність = ")
(define precision (read (current-input-port)))

(display (fraction precision))


