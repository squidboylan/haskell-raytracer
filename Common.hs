module Common where

type Vector = (Float, Float, Float)

addVectors                    :: Vector -> Vector -> Vector
addVectors (a, b, c) (x, y, z) = (a + x, b + y, c + z)

subVectors                    :: Vector -> Vector -> Vector
subVectors (a, b, c) (x, y, z) = (a - x, b - y, c - z)

dotProduct                    :: Vector -> Vector -> Float
dotProduct (a, b, c) (x, y, z) = (a * x) + (b * y) + (c * z)

crossProduct                    :: Vector -> Vector -> Float
crossProduct (a, b, c) (x, y, z) = (b * z - c * y) + (a * z - c * x) + (a * y - b * x)

magnitude :: Vector -> Float
magnitude (a, b, c) = sqrt((a * a) + (b * b) + (c * c))

normalize :: Vector -> Vector
normalize (a, b, c) = ((a/mag), (b/mag), (c/mag))
  where mag = magnitude (a, b, c)
