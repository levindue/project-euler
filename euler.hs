-- Problem 1: Multiples of 3 or 5

problem1 :: Int
problem1 = sum [3,6..999] + sum [5,10..999] - sum [15,30..999]

-- Problem 2: Even Fibonacci Numbers

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

problem2 :: Integer
problem2 = sum $ filter even $ takeWhile (< 4000000) fibs

-- https://oeis.org/A014445
evenFibs = 2 : 0 : zipWith (\x y -> 4 * y + x) evenFibs (tail evenFibs)

problem2' :: Integer
problem2' = sum $ takeWhile (< 4000000) evenFibs

-- Problem 3: Largest Prime Factor

problem3 :: Integer
problem3 = factor 600_851_475_143 2
  where
    factor :: Integer -> Integer -> Integer
    factor n candidate
      | candidate * candidate > n =
          if n > 1 then n else candidate
      | n `mod` candidate == 0 = factor (n `div` candidate) candidate
      | otherwise = factor n (candidate + 1)
