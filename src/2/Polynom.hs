module Polynom where
    type Polynom = [Double]

    cmult :: Polynom -> Double -> Polynom
    eval :: Polynom -> Double -> Double
    deriv :: Polynom -> Polynom

    cmult p d = map (d*) p
    eval p x = foldl (\a b ->a+b*x) 0 p
    deriv p = zipWith (*) [1..] (tail p)
