This file handles the necessary definitions of objects for the raytracer

> module Object where
> import Common
> import Camera
> data Object = Sphere Vector Float Vector | Triangle (Vector, Vector, Vector) Vector Vector | Mesh [Object]
>   deriving Show

> translate                         :: Vector -> Object -> Object
> translate v (Sphere a r c)         = Sphere (addVectors a v) r c
> translate v (Triangle (x, y, z) n c) = Triangle ((addVectors x v), (addVectors y v), (addVectors z v)) n c
> translate v (Mesh ts)              = Mesh (map (translate v) ts)

Equation for this was obtained from
https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection

also
https://gamedev.stackexchange.com/questions/96459/fast-ray-sphere-collision-code

> checkCollision :: Ray -> Object -> Maybe Float
> checkCollision (o, d) (Sphere a r c) = if c1 > 0.0 && b1 > 0.0 then Nothing else if val < 0.0 then Nothing
>                                        else if p1 < p2 then (Just p1) else if p1 > p2 then (Just p2) else Nothing
>   where p1  = -b1 - (sqrt val)
>         p2  = -b1 + (sqrt val)
>         tmp = subVectors o a
>         b1  = dotProduct d tmp
>         c1  = (dotProduct tmp tmp) - r * r
>         val = b1 * b1 - c1
