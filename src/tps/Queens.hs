data Position = Position {x::Int, y::Int} deriving (Eq)

instance Show Position where
    show (Position x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

equalRow :: Position -> Position -> Bool
equalRow (Position x1 _) (Position x2 _) = x1 == x2

equalCol :: Position -> Position -> Bool
equalCol (Position _ y1) (Position _ y2) = y1 == y2

equalDiag :: Int -> Position -> Position -> Bool
equalDiag n (Position x1 y1) (Position x2 y2)
    | x1 - y1 == x2 - y2 = True
    | n - x1 - y1 == n - x2 - y2 = True
    | otherwise = False

possiblePositions :: Int -> [Position]
possiblePositions n = [Position x y | x <- [0..(n-1)], y <- [0..(n-1)]] 

-- possibleQueens :: Int -> Int -> [[Position]]
-- possibleQueens qAmt size = 

