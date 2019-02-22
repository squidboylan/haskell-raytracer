module Common where

type Vector = (Float, Float, Float)

addVectors                    :: Vector -> Vector -> Vector
addVectors (a, b, c) (x, y, z) = (a + x, b + y, c + z)
