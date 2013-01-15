myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMap f [] = []
myMap f (x:xs) = f(x):(myMap f xs)

square :: Int -> Int
square = (*2)

double :: Int -> Int
double = \x -> x * x

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) =
  quicksort [y | y <- xs, y < x] ++
  [x] ++
  quicksort [y | y <- xs, y >= x]

fibs :: [Int]
fibs = 1 : 1 : [a + b | (a, b) <- (zip fibs (tail fibs))]

from :: Int -> [Int]
from k = k : from (k + 1)

sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
primes = sieve (from 2)


{-client - server-}

reqs = client 0 resps
resps = server reqs

server (req:reqs) = (process req) : (server reqs)
client init ~(resp:resps) = init : (client (next resp) resps)

process x = x
next x = x + 1

setMerge :: Ord a => [a] -> [a] -> [a]
setMerge (x:xs) (y:ys)
  | x > y = y : (setMerge (x:xs) ys)
  | x < y = x : (setMerge xs (y : ys))
  | otherwise = x : (setMerge xs ys)

hamming :: [Int]
hamming = 1 : (setMerge (map (*2) hamming)
                (setMerge (map (*3) hamming)
                  (map (*5) hamming)))


