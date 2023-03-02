module Ropes where
    data Rope a = Leaf [a] 
        | Inner (Rope a) Int (Rope a)
        deriving Show

    example = Inner (Inner (Leaf "He") 2 (Leaf "llo")) 5 (Inner (Leaf ", w") 3 (Leaf "orld!"))
    example2 = (Leaf "Hello")
    exampleDel = Inner (Leaf "0") 1 (Leaf "123")

    ropeLength :: Rope a -> Int
    ropeLength (Leaf s) = length s
    ropeLength (Inner _ w r) = w + ropeLength r

    ropeConcat :: Rope a -> Rope a -> Rope a
    ropeConcat l r = Inner l (ropeLength l) r

    ropeSplitAt :: Int -> Rope a -> (Rope a, Rope a)
    ropeSplitAt i (Leaf s) = (Leaf (take i s), Leaf (drop i s))
    ropeSplitAt i (Inner l w r)
        | i < w = let (ll, lr) = ropeSplitAt i l in (ll, ropeConcat lr r)
        | i > w = let (rl, rr) = ropeSplitAt (i-w) r in (ropeConcat l rl, rr)
        | otherwise
        = (l, r)

    ropeInsert :: Int -> Rope a -> Rope a -> Rope a
    ropeInsert i a b = ropeConcat (ropeConcat l a) r 
        where
            (l, r) = ropeSplitAt i b

    ropeDelete :: Int -> Int -> Rope a -> Rope a
    ropeDelete i j rope = ropeConcat a b
        where 
            (a, _) = ropeSplitAt i rope
            (_, b) = ropeSplitAt j rope

    -- just for fun
    ropePrint :: Rope a -> [a]
    ropePrint (Leaf s) = s
    ropePrint (Inner l w r) = ropePrint l ++ ropePrint r