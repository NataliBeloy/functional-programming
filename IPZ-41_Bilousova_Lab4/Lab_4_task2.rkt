#lang racket
(display "Білоусова Наталія, ІПЗ-41, Лабораторна робота 4")
(newline)
(display "===============================================")
(newline)
(define students (list -10 13 -15 20 17 2 -4 6 8 22))
(begin (display "Студенти: ") (display students) (display "\n"))
(display "Cтуденти, які сдали екзамен: ")
(filter positive? students)
(newline)
(display "Cтуденти, які не сдали екзамен: ")
(filter negative? students)
          
                    
      
  


