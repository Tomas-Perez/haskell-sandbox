module Queens where
import Data.List (nub)

data Position = Position {x::Int, y::Int} deriving (Eq)

instance Show Position where
    show (Position x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

equalRow :: Position -> Position -> Bool
equalRow (Position x1 _) (Position x2 _) = x1 == x2

equalCol :: Position -> Position -> Bool
equalCol (Position _ y1) (Position _ y2) = y1 == y2

equalDiag :: Position -> Position -> Bool
equalDiag (Position x1 y1) (Position x2 y2)
    | x1 - y1 == x2 - y2 = True
    | x1 + y1 == x2 + y2 = True
    | otherwise = False

possiblePositions :: Int -> [Position]
possiblePositions n = [Position x y | x <- [0..(n-1)], y <- [0..(n-1)]] 

canEat :: Position -> Position -> Bool
canEat p1 p2 = equalRow p1 p2 || equalCol p1 p2 || equalDiag p1 p2

findQueens :: Int -> Int -> [[Position]]
findQueens n qAmt = findQueens' qAmt (possiblePositions n) []

findQueens' :: Int -> [Position] -> [[Position]] -> [[Position]]
findQueens' qAmt [] current = current
findQueens' qAmt pool@(p:ps) current = case findQueens'' qAmt [] pool of
    Nothing -> findQueens' qAmt ps current
    Just a -> findQueens' qAmt ps (a:current)

findQueens'' :: Int -> [Position] -> [Position] -> Maybe [Position]
findQueens'' qAmt current [] = if length current == qAmt then Just current else Nothing
findQueens'' qAmt current (p:ps)
    | qAmt == length current = Just current
    | any (canEat p) current = findQueens'' qAmt current ps
    | otherwise = findQueens'' qAmt (p:current) ps

actualFindQueens :: Int -> Int -> [[Position]]
actualFindQueens n qAmt = nub $ findQueens3 qAmt [] (possiblePositions n) []

findQueens3 :: Int -> [Position] -> [Position] -> [[Position]] -> [[Position]]
findQueens3   _  _                   []          solutions = solutions
findQueens3 qAmt []                  pool solutions = foldMap (\x -> findQueens3 qAmt [x] (filter (x /=) pool) solutions) pool
findQueens3 qAmt current@(_:solTail) pool@(p:ps) solutions 
    | qAmt == length current = case elem current solutions of
        False -> foldMap (\x -> findQueens3 qAmt solTail (filter (x /=) filteredPool) (current:solutions)) filteredPool
        True  -> foldMap (\x -> findQueens3 qAmt solTail (filter (x /=) filteredPool) solutions) filteredPool
    | otherwise = foldMap (\x -> findQueens3 qAmt (x:current) (filter (x /=) filteredPool) solutions) filteredPool
    where filteredPool = filter (\x -> not (any (canEat x) current)) pool