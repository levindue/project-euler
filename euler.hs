-- Problem 1: Multiples of 3 or 5

problem1 :: Int
problem1 = sum [3,6..999] + sum [5,10..999] - sum [15,30..999]

