
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
    u :: a -> Float
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
        eu' (s, p) = p * u (o s a)

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


data Pop = ClassicCoke
         | DietCoke
         | Sprite
         deriving (Show, Eq, Bounded, Enum)

instance OrdPref Pop where
    r ClassicCoke _ = True
    r _ ClassicCoke = False
    r _ Sprite = True
    r Sprite _ = False

    u ClassicCoke = 1
    u DietCoke = 0.4
    u Sprite = 0

pStates = [
    ((ClassicCoke, DietCoke, ClassicCoke), 0.15)
  , ((ClassicCoke, DietCoke, Sprite), 0.3)
  , ((Sprite, DietCoke, ClassicCoke), 0.2)
  , ((Sprite, DietCoke, Sprite), 0.35)
  ]

pOutcome (c, _, _) ClassicCoke = c
pOutcome _ DietCoke = DietCoke
pOutcome (_, _, s) Sprite = s

pDecision = Decision pStates [ClassicCoke, DietCoke, Sprite] pOutcome


vStates = [
    ("Vietnamese Bluff", 0.7)
  , ("No Bluff", 0.3)
  ]

vActions = [
    "Bomb"
  , "Do Not Bomb"
  ]

data VietnameseOutcome =
    QuickAgreement
  | AdditionalConcessions
  | WarContinues
  deriving (Show, Eq, Bounded, Enum)

instance OrdPref VietnameseOutcome where
    r QuickAgreement _ = True
    r _ QuickAgreement = False
    r _ WarContinues = True
    r WarContinues _ = False

    u QuickAgreement = 1
    u AdditionalConcessions = 0.3
    u WarContinues = 0

vOutcome "Vietnamese Bluff" "Bomb" = QuickAgreement
vOutcome "Vietnamese Bluff" "Do Not Bomb" = AdditionalConcessions
vOutcome "No Bluff" "Bomb" = WarContinues
vOutcome "No Bluff" "Do Not Bomb" = AdditionalConcessions

vDecision = Decision vStates vActions vOutcome


