module Coalition where

import Data.Function (on)
import Data.List (subsequences)
import Data.Maybe (mapMaybe
                 , fromMaybe)
import Control.Arrow (second)

data Party = Kesk | Peruss | Kok | SDP deriving (Show, Eq)
type Seats = Int
type Portfolios = Int

seats :: [(Party, Seats)]
seats = [(Kesk, 49)
       , (Peruss, 38)
       , (Kok, 37)
       , (SDP, 34)]

seatsFor :: [(Party, Seats)] -> Party -> Seats
seatsFor ps p = fromMaybe 0 (lookup p ps)

seatsTotal = 200 :: Seats
portfoliosTotal = 24 :: Portfolios

division :: Int -> Int -> Float
division = (/) `on` fromIntegral

shareOf = division

seatsshares :: [(Party, Seats)] -> [(Party, Float)]
seatsshares = (`sharesOfTotal` seatsTotal)

sharesOfTotal :: [(Party, Seats)] -> Seats -> [(Party, Float)]
sharesOfTotal ss total = map (second (`shareOf` total)) ss

validcoalitions :: [(Party, Float)] -> [[Party]]
validcoalitions ss = [c | c <- subsequences (parties ss), sharestotal ss c > 0.5]

parties :: [(Party, a)] -> [Party]
parties = map fst

sharestotal :: [(Party, Float)] -> [Party] -> Float
sharestotal ss ps = sum $ mapMaybe (`lookup` ss) ps

portfolios :: [(Party, Seats)] -> [(Party, Portfolios)]
portfolios = (`splitPortfolios` portfoliosTotal)

splitPortfolios :: [(Party, Seats)] -> Portfolios -> [(Party, Portfolios)]
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

coalitionSeats :: [(Party, Seats)] -> [Party] -> [(Party, Seats)]
coalitionSeats ss = map (\p -> (p, seatsFor ss p))

coalitionPortfolios :: [(Party, Seats)] -> [Party] -> [(Party, Portfolios)]
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
