module RandomProg1 where

changeDollars :: Float -> Float -> Int
changeDollars pay debt = floor (pay - debt)

changeCents :: Float -> Float -> Int
changeCents pay debt = round (100 * (pay - debt - dollars))
    where dollars = fromIntegral (changeDollars pay debt)

change :: Float -> Float -> (Int, Int)
change pay debt = (changeDollars pay debt, changeCents pay debt)



leapYear :: Int -> Bool
leapYear y = y `mod` 4 == 0 && 
             y `mod` 100 /= 0 || 
             y `mod` 400 == 0


linearFunction :: Num a => a -> a -> a -> a
linearFunction a b x = a * x + b

distance :: Floating a => (a, a) -> (a,a) -> a
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)