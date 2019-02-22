This file handles the necessary definitions of objects for the raytracer

> module Object where
> import Common
> data Object = Sphere Vector Float Vector | Triangle (Vector, Vector, Vector) Vector Vector | Mesh [Object]
>   deriving Show

> translate                         :: Vector -> Object -> Object
> translate v (Sphere a r c)         = Sphere (addVectors a v) r c
> translate v (Triangle (x, y, z) n c) = Triangle ((addVectors x v), (addVectors y v), (addVectors z v)) n c
> translate v (Mesh ts)              = Mesh (map (translate v) ts)
