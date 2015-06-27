
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

data OrdPref c => Decision s a c = Decision {
    states :: [(s, Float)]
  , actions :: [a]
  , outcome :: s -> a -> c
  }

eu :: OrdPref c => a -> Decision s a c -> Float
eu a (Decision ss _ o) = sum $ map eu' ss
    where
        eu' (s, p) = p * u' (o s a)
        u' c = fromIntegral (u c)

data Balls = Balls Color Color deriving (Show)

bStates = [
    (Balls Yellow Yellow, 0.05)
  , (Balls Blue Yellow, 0.15)
  , (Balls Red Yellow, 0.15)
  , (Balls Green Red, 0.3)
  , (Balls Red Blue, 0.3)
  , (Balls Blue Blue, 0.05)
  ]

data Hands = LeftHand | RightHand deriving (Show)

bActions = [LeftHand, RightHand]

bOutcome (Balls l r) LeftHand = l
bOutcome (Balls l r) RightHand = r

bDecision = Decision bStates bActions bOutcome

