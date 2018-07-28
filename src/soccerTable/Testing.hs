module Testing where

import Data.Map (Map)
import qualified Data.Map as Map
import SoccerTable


match1 = Match barcelona madrid
outcome1 = Away

initialStandings = Map.fromList [(barcelona, 0), (madrid, 0)]

finalStandings = Map.fromList [(barcelona, 0), (madrid, 12)]
inProgressStandings = Map.fromList [(barcelona, 0), (madrid, 6)]
invalidStandings = Map.fromList [(barcelona, 2), (madrid, 6)]
invalidStandings2 = Map.fromList [(barcelona, 0), (madrid, 13)]

standings1 = standingsFromOutcome (take 4 (repeat match1)) (take 4 (repeat outcome1))

progress1 = checkProgress standings1 finalStandings
progress2 = checkProgress inProgressStandings finalStandings
progress3 = checkProgress invalidStandings finalStandings
progress4 = checkProgress invalidStandings2 finalStandings

headerTest1 = Header 2 1
standingsTest1 = Map.fromList [(barcelona, 1), (madrid, 1)]
matchesTest1 = [Match barcelona madrid]

table1 = SoccerTable headerTest1 standingsTest1 matchesTest1

result1 = bruteForceSolve table1

headerTest2 = Header 2 1
standingsTest2 = Map.fromList [(barcelona, 3), (madrid, 0)]
matchesTest2 = [Match madrid barcelona]

table2 = SoccerTable headerTest2 standingsTest2 matchesTest2

result2 = bruteForceSolve table2


headerTest3 = Header 6 10
standingsTest3 = Map.fromList 
    [
        (madrid, 8), 
        (barcelona, 8), 
        (valencia, 4), 
        (sevilla, 4), 
        (deportivo, 2), 
        (betis, 0)
    ]
matchesTest3 = 
    [
        Match madrid deportivo,
        Match barcelona sevilla,
        Match betis madrid,
        Match betis sevilla,
        Match barcelona deportivo,
        Match betis barcelona,
        Match madrid barcelona,
        Match sevilla deportivo,
        Match deportivo valencia,
        Match madrid valencia
    ]

table3 = SoccerTable headerTest3 standingsTest3 matchesTest3

result3 = bruteForceSolve table3


headerTest4 = Header 10 18
barcelona = "Barcelona"
madrid = "Real Madrid"
valencia = "Valencia"
sevilla = "Sevilla"
deportivo = "Deportivo"
betis = "Betis"
atlMadrid = "Atl Madrid"
athBilbao = "Ath Bilbao"
espanyol = "Espanyol"
realSociedad = "Real Sociedad"
standingsTest4 = Map.fromList 
    [
        (deportivo, 11), 
        (betis, 9), 
        (atlMadrid, 6), 
        (barcelona, 5), 
        (athBilbao, 4), 
        (madrid, 2),
        (espanyol, 2),
        (valencia, 1),
        (realSociedad, 1)
    ]
matchesTest4 = 
    [
        Match deportivo realSociedad,
        Match barcelona atlMadrid,
        Match athBilbao espanyol,
        Match atlMadrid madrid,
        Match deportivo madrid,
        Match betis deportivo,
        Match realSociedad espanyol,
        Match valencia deportivo,
        Match deportivo barcelona,
        Match madrid barcelona,
        Match espanyol sevilla,
        Match sevilla atlMadrid,
        Match madrid betis,
        Match valencia athBilbao,
        Match betis athBilbao,
        Match valencia atlMadrid,
        Match realSociedad betis,
        Match barcelona betis
    ]

table4 = SoccerTable headerTest4 standingsTest4 matchesTest4

result4 = bruteForceSolve table4