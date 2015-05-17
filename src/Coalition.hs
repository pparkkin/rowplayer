module Coalition where

import Data.Function (on)
import Data.List (subsequences
                , sortBy
                , delete
                , elemIndex)
import Data.Maybe (mapMaybe
                 , fromMaybe)
import Control.Arrow (second)

import Hagl (PlayerID
           , Extensive
           , pays)

type NumSeats = Int
type Seats p = [(p, NumSeats)]
type Payoff = Float
type Payoffs p = [(p, Payoff)]
type PayoffFunction p = [p] -> p -> Payoffs p

data CoalitionMoves = Propose | Accept | Decline

buildCoalitionGame :: (Eq p) => Seats p -> PayoffFunction p -> Extensive CoalitionMoves
buildCoalitionGame ss pof = let
    ps = map fst sorted
    sorted = sortBy (compare `on` snd) ss
        in buildGameTree ss pof ps

inf :: Fractional a => a
inf = 1/0

seatTotal :: Seats p -> NumSeats
seatTotal ss = sum (map snd ss)

partySeats :: (Eq p) => Seats p -> p -> NumSeats
partySeats ps p = fromMaybe 0 (lookup p ps)

partyPayoff :: (Eq p) => Payoffs p -> p -> Payoff
partyPayoff ps p = fromMaybe 0 (lookup p ps)

coalitionSeats :: (Eq p) => Seats p -> [p] -> NumSeats
coalitionSeats ss ps = sum $ map (partySeats ss) ps

shareOf :: Int -> Int -> Float
shareOf = (/) `on` fromIntegral

isMajority :: (Eq p) => Seats p -> [p] -> Bool
isMajority ss ps = let
    tot = seatTotal ss
    cs = coalitionSeats ss ps
        in cs `shareOf` tot > 0.5

validCoalitions :: (Eq p) => Seats p -> [[p]]
validCoalitions ss = [c | c <- subsequences (map fst ss), isMajority ss c]

assignPlayerID :: (Eq p) => Seats p -> p -> PlayerID
assignPlayerID ss p = case elemIndex p (map fst ss) of
    Just i -> i + 1
    Nothing -> error $ "Unknown party" -- FIXME: There must be a better way to handle this

buildCoalitionTree :: (Eq p)
                   => Seats p
                   -> p
                   -> Extensive CoalitionMoves
                   -> PayoffFunction p
                   -> [p]
                   -> Extensive CoalitionMoves
buildCoalitionTree ss formateur fail pof coalition = let
    partners = delete formateur coalition
    payoffs = pof coalition formateur
    accept = pays $ map (partyPayoff payoffs) (map fst ss)
    partnerIDs = map (assignPlayerID ss) partners
        in undefined

buildGameTree :: (Eq p) => Seats p -> PayoffFunction p -> [p] -> Extensive CoalitionMoves
buildGameTree ss _ [] = pays $ replicate (length ss) (-inf)
buildGameTree ss pof (p:ps) = let
    fail = buildGameTree ss pof ps
    coalitions = filter (p `elem`) (validCoalitions ss)
    coalitionTrees = map (buildCoalitionTree ss p fail pof) coalitions
        in undefined

{-
data Party = Kesk | Peruss | Kok | SDP deriving (Show, Eq)
type Portfolios = Int

seats :: Seats Party
seats = [(Kesk, 49)
       , (Peruss, 38)
       , (Kok, 37)
       , (SDP, 34)]

seatsFor :: Seats Party -> Party -> NumSeats
seatsFor ps p = fromMaybe 0 (lookup p ps)

seatsTotal = 200 :: NumSeats
portfoliosTotal = 24 :: Portfolios

division :: Int -> Int -> Float
division = (/) `on` fromIntegral

shareOf = division

seatsshares :: Seats Party -> [(Party, Float)]
seatsshares = (`sharesOfTotal` seatsTotal)

sharesOfTotal :: Seats Party -> NumSeats -> [(Party, Float)]
sharesOfTotal ss total = map (second (`shareOf` total)) ss

validcoalitions :: [(Party, Float)] -> [[Party]]
validcoalitions ss = [c | c <- subsequences (parties ss), sharestotal ss c > 0.5]

parties :: [(Party, a)] -> [Party]
parties = map fst

sharestotal :: [(Party, Float)] -> [Party] -> Float
sharestotal ss ps = sum $ mapMaybe (`lookup` ss) ps

portfolios :: Seats Party -> [(Party, Portfolios)]
portfolios = (`splitPortfolios` portfoliosTotal)

splitPortfolios :: Seats Party -> Portfolios -> [(Party, Portfolios)]
splitPortfolios ss ps = zip (parties ss) (splitAmong ps (map snd ss))

splitAmong :: Int -> [Int] -> [Int]
splitAmong n ss = splitAmong' n ss'
    where
        ss' = map (`shareOf` t) ss
        t = sum ss
        splitAmong' l [s] = [l]
        splitAmong' l (s:ss) = let k = n - r
                                   r = round ((1 - s) * fromIntegral n) in
            k : splitAmong' (l - k) ss

coalitionSeats :: Seats Party -> [Party] -> Seats Party
coalitionSeats ss = map (\p -> (p, seatsFor ss p))

coalitionPortfolios :: Seats Party -> [Party] -> [(Party, Portfolios)]
coalitionPortfolios ss ps = portfolios $ coalitionSeats ss ps

distance :: Party -> Party -> Float
distance p r = distance' p r / 4

distance' :: Party -> Party -> Float
distance' Kesk Peruss = 21.14
distance' Kesk Kok = 13.34
distance' Kesk SDP = 15.33
distance' Peruss Kesk = 21.14
distance' Peruss Kok = 26.55
distance' Peruss SDP = 26.82
distance' Kok Kesk = 13.34
distance' Kok Peruss = 26.55
distance' Kok SDP = 13.96
distance' SDP Kesk = 15.33
distance' SDP Peruss = 26.82
distance' SDP Kok = 13.96
distance' _ _ = 0

payoffs :: Party -> [Party] -> [(Party, Float)]
payoffs f c = map (\p -> (p, payoff f p)) c
    where
        payoff f p = fromIntegral (portfolios p) - distance f p
        portfolios p = fromMaybe 0 (lookup p (coalitionPortfolios seats c))

turn :: [[Party]] -> Party -> IO ()
turn cs' p = do
    let cs = filter (elem p) cs'
    print p
    mapM_ (\c -> print $ payoffs p c) cs

game :: [Party] -> IO ()
game ps = do
    let cs = validcoalitions $ seatsshares seats
    mapM_ (turn cs) (reverse ps)
-}
