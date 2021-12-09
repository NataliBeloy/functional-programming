#lang racket

(display "Лаб7, завдання 3.1, Білоусова Наталія, ІПЗ-41/1\n")

(define (list->file lst file)
  (display-lines-to-file lst
                         file
                         #:exists 'replace
                         #:mode 'text))

(define (next-line-it file)
  (let ((line (read-line file 'any)))
    (if (eof-object? line)
      '()  
      (begin(displayln line)
      (append (list line) (next-line-it file))) ; зберігаємо порядково записи файлу в список
     )
   )
)

(define (replace-word str old-word new-word)
  (cond
    [(non-empty-string? str)
     (string-replace str old-word new-word #:all? #true)     
     ]
    [else
     ""]))

(define (replace-sentences-of-list user-list old-word new-word)
  (if (null? user-list)
    '()
    (append (list (replace-word (car user-list) old-word new-word)) (replace-sentences-of-list (cdr user-list) old-word new-word)) 
 ))
(define words-list (list
                    "These brothers bathe with those brothers,"
                    "Those brothers bathe with these brothers."
                    "If these brothers didn’t bathe with those brothers,"
                    "Would those brothers bathe with these brothers?"
                    )
) ; текст, що заданий програмно, за допомогою списків

(list->file words-list "C:\\Users\\Natalia\\source\\repos\\functional-programming\\IPZ-41_Bilousova_Lab7\\input.txt") ; записуємо текст до файлу
(define old-word "brothers")
(define new-word "sisters")
(displayln "Вміст файлу input.txt:")
(define words-list-updated (call-with-input-file "C:\\Users\\Natalia\\source\\repos\\functional-programming\\IPZ-41_Bilousova_Lab7\\input.txt" next-line-it)) ; виводимо його вміст
(set! words-list-updated (replace-sentences-of-list words-list-updated old-word new-word)); Робимо заміну слів
(list->file words-list-updated "C:\\Users\\Natalia\\source\\repos\\functional-programming\\IPZ-41_Bilousova_Lab7\\output.txt") ; записуємо модифікований текст до результуючого файлу
(display "Успішно записано в output.txt")
