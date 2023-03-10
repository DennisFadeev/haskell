module Play where
    divString :: String -> [String]
    divString xs
        | length xs > 1 = take 2 xs : divString (tail (tail xs))
        | length xs == 2 = [xs]
        | length xs == 0 = []
        | otherwise = [xs ++ "_"]