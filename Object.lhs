This file handles the necessary definitions of objects for the raytracer

> module Object where
> import Common
> data Object = Sphere Vector Float | Triangle Vector Vector Vector | Mesh [Object]
>   deriving Show

> translate                   :: Vector -> Object -> Object
> translate v (Sphere a r)     = Sphere (addVectors a v) r
> translate v (Triangle a b c) = Triangle (addVectors a v) (addVectors b v) (addVectors c v)
> translate v (Mesh ts)        = Mesh (map (translate v) ts)
