; a) Write a higher-order function twice which will apply f twice on x
(defun twice (f x)
  (funcall f (funcall f x)))

(format t "(twice((lambda (x) (* x 2)) 2)) = ~D~%" (twice (lambda (x) (* x 2)) 2))

; b) Write a higher-order function myMap which behaves like map using the if function
(defun myMap (f l)
  (if (null l) nil (cons (funcall f (car l)) (myMap f (cdr l)))))

; Use the myMap function:
; 1) increase every number by one
; 2) replace negative numbers by 0
; 3) replace every element x by a list only containing x

(format t "1) (1 2 3 4) => (~{~D~^ ~})~%" (myMap (lambda (x)(+ x 1)) '(1 2 3 4)))
(format t "2) (-1 1 -2 2 -3 3) => (~{~D~^ ~})~%" (myMap (lambda (x)(if (< x 0) 0 x)) '(-1 1 -2 2 -3 3)))
(format t "3) (A B C D) => (~{~S~^ ~})~%" (myMap (lambda (x)(cons x '())) '(A B C D)))
