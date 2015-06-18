
module Utility where

class (Bounded a, Enum a, Eq a) => OrdPref a where
    r :: a -> a -> Bool
    i :: a -> a -> Bool
    i c d = r c d && r d c
    p :: a -> a -> Bool
    p c d = r c d && not (r d c)
    ord :: [a]
    ord = reverse $ sort [minBound ..]
        where
            sort [] = []
            sort (c:cs) = (sort (filter (not . (c `r`)) cs))
                ++ [c] ++ (sort (filter (c `r`) cs))
    u :: a -> Int
    u c' = util 1 ord
        where
            util n (c:cs) = if c == c'
                then n
                else if (i c (head cs))
                    then util n cs
                    else util (n + 1) cs


data Color = Red | Green | Blue | Yellow deriving (Show, Eq, Bounded, Enum)

instance OrdPref Color where
    r Yellow _ = True
    r _ Yellow = False
    r Blue _ = False
    r Red Green = True
    r Green Red = True
    r _ _ = True

