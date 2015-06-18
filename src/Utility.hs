
module Utility where

import Data.List (sortBy)

class (Bounded a, Enum a, Eq a) => OrdPref a where
    r :: a -> a -> Bool
    i :: a -> a -> Bool
    i c d = r c d && r d c
    p :: a -> a -> Bool
    p c d = r c d && not (r d c)
    ord :: [a]
    ord = sortBy ord' [minBound ..]
        where
            ord' c d
                | p c d == True = GT
                | p d c == True = LT
                | i c d == True = EQ
    u :: a -> Int
    u c' = util 1 ord
        where
            util n (c:cs)
                | c == c' = n
                | i c (head cs) = util n cs
                | otherwise = util (n + 1) cs


data Color = Red | Green | Blue | Yellow deriving (Show, Eq, Bounded, Enum)

instance OrdPref Color where
    r Yellow _ = True
    r _ Yellow = False
    r Blue _ = False
    r Red Green = True
    r Green Red = True
    r _ _ = True

