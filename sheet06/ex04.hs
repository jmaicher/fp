-- ex04)
within eps (h1 : (h2:t)) =  if abs(h1-h2) < eps
                            then h2
                            else within eps (h2:t)

nextApproach a x = (2 * x) - (a * x^2)
inverse a = within 1e-9 (iterate (nextApproach a) 0.0001)
