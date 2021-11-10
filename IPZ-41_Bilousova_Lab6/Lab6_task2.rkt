#lang racket
(display "Лаб6, завдання 3.2, Білоусова Наталія, ІПЗ-41/1\n")


(define (is-eng-letter? v)
  (if (or (and (>= (char->integer v) 65) (<= (char->integer v) 90))
          (and (>= (char->integer v) 97) (<= (char->integer v) 122)))
      true
      false
      )
  )


(define (letter-generator)
  (integer->char (+ 33 (random 122)))
  )


(define (remove-last struct)
  (cond
    ((empty? (rest struct)) empty)
    ((cons? struct) (cons (first struct) (remove-last (rest struct))))
    )
  )


(define (is-queue-empty? q)
  (null? q)
  )

(define (push-stack s v)
  (let
      (
       (news (append s (list v)))
       )
    (display "Додаємо елемент в чергу: ")
    (displayln news)
    news
    )
  )

(define (push-queue q v)
  (let
      (
      (newq (cons v q))
    )
  (display "Додаємо новий елемент в стек: ")
  (displayln newq)
  newq
  )
)

(define (pop-queue q)
  (if (is-queue-empty? q)
      null
      (let
          (
           (newq (remove-last q))
           )
        (display "Дістаємо елемент зі стеку: ")
        (displayln newq)
        (list newq (last q))
      )
  )
)

(define (mainf)
  (define (generate-queue q sizeq [i 0])
    (if (< i sizeq)
        (generate-queue (push-queue q (letter-generator)) sizeq (+ i 1))
        q
        )
    )
  (define (generate-stack s q)
    (if (is-queue-empty? q)
        s
        (let
            (
             (resq (pop-queue q))
             )
          (if (is-eng-letter? (last resq))
              (generate-stack (push-stack s (last resq)) (first resq))
              (generate-stack s (first resq))
              )))
    )
  (let
        ((q (generate-queue '() 8)))
        (let((s (generate-stack '() q)))
            (printf "Остаточна черга: ~a~n" s)
        )
    )
)

(mainf)
        
      
