module Collatz where
    collatz :: Int -> [Int]
    collatz i = take 10 (iterate next i)

    next n
        | n `mod` 2 == 0 = n `div` 2
        | otherwise = 3 * n + 1

    num :: Int -> Int
    num a = length (takeWhile (/= 1) (tail(collatz a)))