(format t "13 + (-12) = ~D~%" (+ 13 -12))

(format t "5 * (1 + 2) - 16 = ~D~%" (- (* 5 (+ 1 2)) 16))

(setq res (* 2 3 4))
(format t "2 * 3 * 4 = ~D~%" res)

(format t "(~{~D~^, ~})~%" (cons 1 (cons 2 nil)))

(setq myList '(1 2 3 4))
(format t "(~{~D~^, ~})~%" myList)

(defun avg (a b) (/ (+ a b) 2))
(format t "(avg 2 4) = ~D~%(avg 10 15) = ~D~%" (avg 2 4) (avg 10 15))

(defun sign  (x)
  (cond ((< x 0) -1)
        ((eq x 0) 0)
        (T 1)))
(format t "(sign -25) = ~D~%(sign 0) = ~D~%(sign 13) = ~D~%" (sign -25) (sign 0) (sign 13))
