module Arithmetik where
 pow1 b e = if e == 0 then 1 else b * pow1 b (e - 1)

 pow2 b e
    | e < 0 = error "Negativer Exponent"
    | e == 0 = 1
    | e `mod` 2 == 0 = pow2 (b*b) (e `div` 2)
    | otherwise = b * pow2 (b*b) (e `div` 2)

 pow3 b e acc
    | e < 0 = error "negativer exponent"
    | e == 0 = acc
    | e `mod` 2 == 0 = pow2 (b*b) (e `div` 2)
    | otherwise = acc * pow2 (b*b) (e `div` 2)

 root e r = rootHelper 0 r
    where rootHelper a b
            | (b-a) == 1 = a
            | pow2 half e <= r = rootHelper half b
            | otherwise = rootHelper a half
            where half = (a+b) `div` 2

 isPrime n
    | n <= 0 = error "negativer wert"
    | otherwise = null $ filter (\k -> n `mod` k == 0) [2..root 2 n]