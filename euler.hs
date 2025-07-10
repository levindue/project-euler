import Data.Char (digitToInt)

-- Problem 1: Multiples of 3 or 5

problem1 :: Int
problem1 = sum [3,6..999] + sum [5,10..999] - sum [15,30..999]

-- Problem 2: Even Fibonacci Numbers

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

problem2 :: Integer
problem2 = sum $ filter even $ takeWhile (< 4000000) fibs

-- https://oeis.org/A014445
evenFibs :: [Integer]
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

-- Problem 4: Largest Palindrome Product

isPalindrome :: Integer -> Bool
isPalindrome n = str == reverse str
  where str = show n

problem4 :: Integer
problem4 = maximum [ x*y
                   | x <- [999,998..100]
                   , y <- [999,998..x]
                   , isPalindrome (x*y)]

-- Problem 5: Smallest Multiple

problem5 :: Integer
problem5 = foldl lcm 1 [1..20]

-- Problem 6: Sum Square Difference

sumSquareDifference :: Int -> Int
sumSquareDifference n = sqosu - suosq
  where
    sqosu = (sum [1..n]) ^ 2
    suosq = sum [x^2 | x <- [1..n]]

problem6 :: Int
problem6 = sumSquareDifference 100

-- Problem 8: Largest Product in a Series

p8in :: [Int]
p8in = map digitToInt (show 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)

window :: Int -> [a] -> [[a]]
window k xs
  | length xs < k = []
  | otherwise = take k xs : window k (tail xs)

problem8 :: Int
problem8 = maximum $ map product (window 13 p8in)
