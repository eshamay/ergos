module HaskellMD.Vector
(Position,
 vecr,
 vecr3d,
 nullVecr,
 cosAngle) where

import Data.Packed.Vector
import Numeric.Container

type Position = Vector Double

-- creates a 3d vector from the given values supplied
vecr3d :: Double -> Double -> Double -> Position
vecr3d a b c = fromList [a,b,c] :: Vector Double

-- creates a position vector from the given list of positions
vecr :: [Double] -> Position
vecr x = fromList x :: Vector Double

-- creates a null vector of the given size
nullVecr :: Int -> Position
nullVecr n = vecr $ take n $ repeat 0.0

-- the cosine of the angle between two vectors
cosAngle :: Position -> Position -> Double
cosAngle a b = (dot a b) / (norm2 a) / (norm2 b)
