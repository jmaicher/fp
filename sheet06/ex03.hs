-- ex03)

-- a)
odds1 :: [Int]
odds1 = [2 * x - 1 | x <- [1..]]

-- b)
odds2 :: [Int]
odds2 = iterate (+2) 1

-- c)
prefsum :: [Int] -> [Int]
prefsum (x:y:xs) = x : prefsum(x + y:xs)

prefsum2 :: [Int] -> [Int]
prefsum2 list = (head list) : [a+b | (a,b) <- zip (prefsum2 list) (tail list)]

prefsum3 :: [Int] -> [Int]
prefsum3 list = let prefsum (h:t) s = (s+h) : prefsum t (s+h)
                in prefsum list 0
