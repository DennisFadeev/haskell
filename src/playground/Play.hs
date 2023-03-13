module Play where
    import Data.List
    divString :: String -> [String]
    divString xs
        | length xs > 1 = take 2 xs : divString (tail (tail xs))
        | length xs == 2 = [xs]
        | length xs == 0 = []
        | otherwise = [xs ++ "_"]
        
    persistence :: Int -> Int
    persistence n = length (persistenceList n)

    persistenceList :: Int -> [Int]
    persistenceList n
        | length (show n) == 1 = []
        | otherwise = (persist n):persistenceList (persist n) where persist n = foldr (*) 1 (map (read . pure :: Char -> Int) (show n))

    getUnique :: [Int] -> Int
    getUnique xs
        | head ys == head (tail ys) = last ys
        | otherwise = head ys
        where ys = sort xs

    uniqueInOrder str = map (\x -> head x) (fmap (nub) (group str))

    expandedForm :: Int -> String
    expandedForm n = iterateList (show n)
    iterateList xs
        | length xs == 1 = [head xs]
        | length xs == 2 && (last xs) == '0' = xs 
        | (head xs) /= '0' = [head xs] ++ (replicate (length (tail xs)) '0') ++ " + " ++ (iterateList (tail xs))
        | (head xs) == '0' = iterateList (tail xs)

    moveZeros :: [Int] -> [Int]
    moveZeros xs = (filter (/=0) xs) ++ (replicate len 0) where len = length xs - length (filter (/=0) xs)

    spinWords :: String -> String
    spinWords str = init (arrangeStr (words str))

    arrangeStr :: [String] -> String
    arrangeStr [] = []
    arrangeStr arr
        | length (head arr) > 4 = arrange (head arr) ++ " " ++ arrangeStr (tail arr)
        | otherwise = (head arr) ++ " " ++ arrangeStr (tail arr)

    arrange :: String -> String
    arrange [] = []
    arrange str = last str : arrange (init str)
    