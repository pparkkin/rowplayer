module Coalition where

import Data.Function (on)
import Data.List (subsequences
                , sortBy
                , delete
                , elemIndex)
import Data.Maybe (mapMaybe
                 , fromMaybe
                 , catMaybes)
import Control.Arrow (second)

import Hagl (PlayerID
           , Extensive
           , pays
           , player
           , (<|>)
           , decision)
import Hagl.More.Vis

type NumSeats = Int
type Seats p = [(p, NumSeats)]
type Payoff = Float
type Payoffs p = [(p, Payoff)]
type PayoffFunction p = [p] -> p -> Payoffs p

data CoalitionMoves = Propose | Accept | Decline deriving (Eq, Show)

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

assignPlayerID :: (Eq p) => Seats p -> p -> Maybe PlayerID
assignPlayerID ss p = elemIndex p (map fst ss)

buildAcceptChain :: Extensive CoalitionMoves
                 -> Extensive CoalitionMoves
                 -> [PlayerID]
                 -> Extensive CoalitionMoves
buildAcceptChain accept _ [] = accept
buildAcceptChain accept fail (p:ps) = player p (Decline, fail)
                                    <|> (Accept, buildAcceptChain accept fail ps)

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
    partnerIDs = catMaybes $ map (assignPlayerID ss) partners
        in buildAcceptChain accept fail partnerIDs

buildGameTree :: (Eq p) => Seats p -> PayoffFunction p -> [p] -> Extensive CoalitionMoves
buildGameTree ss _ [] = pays $ replicate (length ss) (-inf)
buildGameTree ss pof (p:ps) = let
    fail = buildGameTree ss pof ps
    coalitions = filter (p `elem`) (validCoalitions ss)
    coalitionTrees = map (buildCoalitionTree ss p fail pof) coalitions
    edges = map (\s -> (Propose, s)) coalitionTrees
        in case assignPlayerID ss p of
            Just pid -> decision pid edges
            Nothing -> fail

-- TEST
data Party = Kesk | Peruss | Kok | SDP deriving (Show, Eq)
type Portfolios = Int

seats :: Seats Party
seats = [(Kesk, 49)
       , (Peruss, 38)
       , (Kok, 37)
       , (SDP, 34)]

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

payoffs :: [Party] -> Party -> [(Party, Float)]
payoffs c f = map (\p -> (p, payoff f p)) c
    where
        payoff f p = distance f p * (-1)

-- In REPL
-- > import Hagl.More.Vis
-- > import Data.GraphViz.Commands
-- > runGraphviz (extensiveToDot (buildGameTree seats payoffs (map fst seats))) Png "test.png"

