-- Problem 1: Multiples of 3 or 5

problem1 :: Int
problem1 = sum [3,6..999] + sum [5,10..999] - sum [15,30..999]

-- Problem 2: Even Fibonacci Numbers

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

problem2 :: Integer
problem2 = sum $ filter even $ takeWhile (< 4000000) fibs

-- https://oeis.org/A014445
thirdFibs = 2 : 0 : zipWith (\x y -> 4 * y + x) thirdFibs (tail thirdFibs)

problem2' :: Integer
problem2' = sum $ takeWhile (< 4000000) thirdFibs
