(defun down (x)
  (cond ((eq x 0) 0) (T (down (- x 1)))))
(print (down 5))

; LENGTH

(defun myLength (l)
  (cond ((null l) 0) (T (+ 1 (myLength (cdr l))))))

(print (myLength '(1 2 3 4 5)))

(defun myLengthA (l a)
  (cond ((null l) a) (T (myLengthA (cdr l) (+ 1 a)))))

(print (myLengthA '(1 2 3 4 5) 0))


; FAC

(defun fac (n)
  (cond ((eq n 0) 1) (T (* n (fac (- n 1))))))

(print (fac 5))

(defun facA (n a)
  (cond ((eq n 0) a) (T (facA (- n 1) (* n a)))))

(print (facA 5 1))
