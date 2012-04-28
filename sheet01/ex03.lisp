; a) Write a recursive function which behaves like nth (returns the nth element of a list)
(defun myNth (n l)
  (cond ((null l) nil)
        ((eq n 1) (car l))
        (T (myNth (- n 1) (cdr l)))))

(setq alphabet '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
(format t "(myNth 5 '(~{~S~^ ~})) = ~S~%" alphabet (myNth 5 alphabet))

; b) Write a recursive function which behaves like append (appends one list to another)
(defun myAppend (l1 l2)
  (cond ((null l1) l2)
        (T (cons (car l1) (myAppend (cdr l1) l2)))))

(setq l1 '(1 2 3 4 5))
(setq l2 '(6 7 8 9 10))
(format t "(myAppend '(~{~D~^ ~}) '(~{~D~^ ~})) = (~{~D~^ ~})~%" l1 l2 (myAppend l1 l2))
