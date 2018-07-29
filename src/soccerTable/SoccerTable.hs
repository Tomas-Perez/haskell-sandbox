module SoccerTable where

import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace


data Header = Header { teamAmt :: Int, matchAmt :: Int } deriving (Show)

type Team = String

data Match = Match { home :: Team, away :: Team} deriving (Show)

-- Outcome of a match
data Outcome = Home | Away | Draw deriving (Show, Eq, Ord, Enum, Bounded)

outcomeEnum = [minBound..] :: [Outcome]

type Standings = Map Team Int

data SoccerTable = SoccerTable { header :: Header, standings :: Standings, matches :: [Match] } deriving (Show)

-- Progress of the result being generated
-- Valid: The result implies team standings such that all team scores are equal or less than their final scores.
-- Invalid: At least one team score is higher than their final score.
-- Final: The result implies team standings equal to the expected final scores.
data Progress = Valid | Invalid | Final deriving Show

-- Given a list of matches and the outcomes for said matches, returns the team standings
standingsFromOutcome :: [Match] -> [Outcome] -> Standings
standingsFromOutcome ms os = standingsFromOutcome' ms os Map.empty

standingsFromOutcome' :: [Match] -> [Outcome] -> Standings -> Standings
standingsFromOutcome' ((Match h a) : ms) (o:os) standings
    | o == Home = standingsFromOutcome' ms os homeWin
    | o == Away = standingsFromOutcome' ms os awayWin
    | o == Draw = standingsFromOutcome' ms os draw
    | otherwise = standings
        where homeWin = Map.insert h (mapGetHome + 3) standings
              awayWin = Map.insert a (mapGetAway + 3) standings
              draw = Map.insert h (mapGetHome  + 1) (Map.insert a (mapGetAway  + 1) standings)
              mapGetHome = Map.findWithDefault 0 h standings 
              mapGetAway = Map.findWithDefault 0 a standings 
standingsFromOutcome' _ _ standings = standings

-- Check the progress of a Standings in reference to another
checkProgress :: Standings -> Standings -> Progress
checkProgress progress final = checkProgressDeltas (Map.elems (Map.unionWith (-) progress final))

checkProgressDeltas :: [Int] -> Progress
checkProgressDeltas (d : ds)
    | d == 0 = checkProgressDeltas ds
    | d < 0  = checkProgressDeltas' ds
    | d > 0  = Invalid
checkProgressDeltas [] = Final

checkProgressDeltas' :: [Int] -> Progress
checkProgressDeltas' (d : ds)
    | d <= 0 = checkProgressDeltas' ds
    | d > 0 = Invalid
checkProgressDeltas' [] = Valid

-- Generate all possible outcomes for 'n' matches
allPossibleOutcomes :: Int -> [[Outcome]]
allPossibleOutcomes matchAmt = rep matchAmt (listify outcomeEnum)

-- Solve the SoccerTable by brute force
bruteForceSolve :: SoccerTable -> [Outcome]
bruteForceSolve table@(SoccerTable h _ _) = bruteForceSolve' table possibilities
    where possibilities = allPossibleOutcomes (matchAmt h)

bruteForceSolve' :: SoccerTable -> [[Outcome]] -> [Outcome]
bruteForceSolve' table@(SoccerTable _ s ms) (os: oss) = 
    case checkProgress2 s ms os of
        Final -> os
        otherwise -> bruteForceSolve' table oss

listify :: [a] -> [[a]]
listify (x:xs) = [x] : listify xs
listify [] = []

prod :: [[a]] -> [[a]] -> [[a]]
prod as bs = [a ++ b | a <- as, b <- bs]

rep :: Int -> [[a]] -> [[a]]
rep n as = foldl1 prod $ replicate n as

checkProgress2 :: Standings -> [Match] -> [Outcome] -> Progress
checkProgress2 st ((Match h a):ms) (o:os) 
    | o == Home && homeWin >= 0 = checkProgress2 homeMap ms os --`debug` ("Home win: " ++ show homeMap)
    | o == Away && awayWin >= 0 = checkProgress2 awayMap ms os --`debug` ("Away win: " ++ show awayMap)
    | o == Draw && drawAway >= 0 && drawHome >= 0 = checkProgress2 drawMap ms os -- `debug` ("Draw: " ++ show drawMap)
    | otherwise = Invalid
    where 
        homeWin = getHome - 3 --`debug` (show getHome)
        awayWin = getAway - 3
        drawAway = getAway - 1
        drawHome = getHome - 1
        getHome = st Map.! h
        getAway = st Map.! a
        homeMap = Map.insert h (homeWin) st 
        awayMap = Map.insert a (awayWin) st
        drawMap = Map.insert h (drawHome) (Map.insert a (drawAway) st)
checkProgress2 st _ []
    | nonFinalStandings == Map.empty = Final
    | otherwise = Valid
    where nonFinalStandings = (Map.filter (/= 0) st) --`debug` (show st)

debug = flip trace