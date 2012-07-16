-- ex02)

factorial1 :: Int -> Int
factorial1 0 = 1
factorial1 n = n * factorial1 (n - 1)

factorial2 :: Int -> Int
factorial2 n = if n > 0 then n * factorial2 (n-1) else 1

factorial3 :: Int -> Int
factorial3 n = product [1..n]

factorial4 :: Int -> Int
factorial4 n = foldl (\x y -> x * y) 1 [1..n] 
