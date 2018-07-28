
{-# Language GADTs, GADTSyntax, StandaloneDeriving #-}

module Quadratic where

data Quadratic a where 
    Quadratic :: (Show a, Floating a) => a -> a -> a -> Quadratic a

deriving instance Show a => Show (Quadratic a)

data Roots a where 
    Roots :: (Show a, Floating a) => a -> a -> Roots a
    SingleRoot :: (Show a, Floating a) => a -> Roots a

deriving instance Show a => Show (Roots a)

singleRoot :: (Floating a, Eq a, Show a) => a -> a -> Maybe (Roots a)
singleRoot a b = fmap SingleRoot ((-b) `aDiv` a)

dualRoots :: (Floating a, Eq a, Show a) => a -> a -> a -> Maybe (Roots a)
dualRoots a b c = Roots <$> root1 <*> root2
    where sqrtBody = b ** 2 - 4 * a * c
          sqRoot = sqrt sqrtBody
          root1 = ((-b) - sqRoot) `aDiv` a
          root2 = ((-b) + sqRoot) `aDiv` a
          

aDiv :: (Floating a, Eq a) => a -> a -> Maybe a
aDiv x a 
    | a /= 0.0 = Just (x / (2 * a))
    | otherwise = Nothing


roots :: (Floating a, Ord a, Show a) => Quadratic a -> Maybe (Roots a)
roots (Quadratic a b c)
    | sqrtBody == 0 = singleRoot a b
    | sqrtBody >= 0 = dualRoots a b c
    | otherwise = Nothing
    where sqrtBody = b ** 2 - 4 * a * c
          sqRoot = sqrt sqrtBody

yVal :: Floating a => Quadratic a -> a -> a
yVal (Quadratic a b c) x = a * x ** 2 + b * x + c