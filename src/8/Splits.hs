module Splits where
    splits :: [t] -> [([t], [t])]
    splits [] = []
    splits xs = [splitAt x xs | x <- [0..(length xs)]]

    equalSides :: [Int] -> Int
    equalSides xs
        | null l = -1
        | otherwise = minimum l
        where l = [x | x <- [0..length xs], sum (take x xs) == sum (drop (x+1) xs)]

    test xs n = (take n xs, drop (n+1) xs)

    fac 0 = 1
    fac 1 = 1
    fac n = n * fac (n-1)

    zeros n = length xs where xs = testStr (show (fac n))

    testStr str
        | last str == '0' = 0:testStr (init str)
        | otherwise = []

    findOutlier :: [Int] -> Int 
    findOutlier xs 
        | [n] <- filter even xs = n
        | [n] <- filter odd  xs = n

    deleteNth :: [Int] -> Int -> [Int]
    deleteNth lst n
        | count (head lst) > n

    count :: Eq a => a -> [a] -> Int
    count x = length . filter (x==)
    



    
    