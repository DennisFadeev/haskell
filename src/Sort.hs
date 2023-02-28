module Sort where
    insert n [] = [n]
    insert n (x:xs)
        | x <= n = x:(insert n xs)
        | otherwise = n:x:xs

    insertSort (x:xs) = insert x (insertSort xs)
    insertSort [] = []

    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
        | x <= y = x:merge xs (y:ys)
        | otherwise = y:merge (x:xs) (ys)

    mergeSort [] = []
    mergeSort [x] = [x]
    mergeSort l = merge 
        (mergeSort (take (length l `div` 2) l)) (mergeSort (drop (length l `div` 2) l))