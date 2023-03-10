module Merge where
    merge :: [Integer] -> [Integer] -> [Integer]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
        | x <= y = x:merge xs (y:ys)
        | otherwise = y:merge (x:xs) (ys)
    
    primes = sieve [2..]
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

    primepowers :: Integer -> [Integer]
    primepowers n = foldr merge [] [map (^i) primes | i <- [1..n]]

    data List a = Cons a (List a)
                    | Empty
        deriving Show

    customList :: List Integer
    customList = Cons 1 (Cons 2 (Cons 3 Empty))
