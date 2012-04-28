; a) Write a function sum which sums numbers 0..n (n int > 0)
(defun sum (n)
  (cond ((eq n 0) 0)
        (T (+ (sum (- n 1)) n))))

(format t "(sum ~D~^) = ~D~%" 10 (sum 10))

; b) Write a function pow which calculates b^e (b,e int)
(defun pow (b e)
  (cond ((eq e 1) b)
        (T (* b (pow b (- e 1))))))

(format t "(pow ~D~^ ~D~^) = ~D~%" 2 8 (pow 2 8) )
