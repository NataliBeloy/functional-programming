#lang racket
(display "Білоусова Наталія, ІПЗ-41, Лабораторна робота 4")
(newline)
(display "===============================================")
(newline)
(define lst (range 1 6))
 
lst
 
(define (cons-with num lst from)
 
  (if (< (length from) num)
    (display "Error")
    
  (if (zero? num)
     lst
     (cons (car from) (cons-with (- num 1) lst (cdr from))))))
 
 
(cons-with 3 lst '(10 20 30 40 50 60 70)) 

(define (user-reverse input-list)
   (if (null? input-list)
       '()
       (append (user-reverse (cdr input-list))
               (list (car input-list)))))

(display "Позиція для старту інвертування:")
(define reverse-position (read))


(set! lst (append (take lst reverse-position) (user-reverse (list-tail lst reverse-position) )))
(begin (display "Список після інвертування з заданої позиції: ")(display lst)(display "\n"))
(define list2 (list 1))
(set! lst (range 1 6))
 
(display "Список, який сформований з квадратів вихідного списку: ")
(map (lambda (lst list2)
(* lst list2))

lst
'(1 2 3 4 5 ))


