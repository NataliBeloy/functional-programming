#lang racket

(define (mainf )
  (condition-loop -2)
  )
(define (condition-loop value)
  (cond
    ((and (>= value -2)(<= value 2))
     ( begin
       (task-iter value)
       (condition-loop (+ value 0.5) ))))
  )

(define (task-iter x  )
  (display "-----------------------------------")
  (newline)
  (display "x: ")
  (display x)
  (newline)
  (display "Difference: ")
  (display(abs (- (y x) (yst x))))
  (newline)
  (display "Result of my exponent. function: ")
  (display(expo x ))
  (newline)
  (display "Result of SCHEME exponent. function: ")
  (display(exp x))
  (newline)
  )
  
(define (factorial n)
(if (= n 0)
1
(* n (factorial (- n 1)))))

(define (expo x . n)   ;розкладання exp(-x) в ряд Тейлора
  (cond ((not (null? n))
         (cond ((< 25 (car n))
                0)
               (else (+ (/ (expt x (car n)) (factorial (car n))) 
                        (expo x (+ 1 (car n) ) ) ) ) ) )
        (else (+ 1 (expo x 1)))))
 
(define (y x) ; обчислення функції exp(-x) для x>2, x=2, x<2
  (if (> x 2)
       (/ 1 (expo -(+ x 5 )))
        (if (and(>= x 0) (<= x 2))
           (+ (expo x) (expo (* 2 x)))
           (expo (- x 1))
        )
  ))

(define (yst x) ; обчислення  вбудованої функції exp(-x) для x>2, x=2, x<2
  (if (> x 2)
       (/ 1 (exp -(+ x 5 )))
        (if (and(>= x 0) (<= x 2))
           (+ (exp x) (exp (* 2 x)))
           (exp (- x 1))
        )
  ))
(display "Білоусова Наталія, ІПЗ-41, Лабораторна робота 2")
(newline)
(mainf)

