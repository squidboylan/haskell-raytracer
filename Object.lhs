This file handles the necessary definitions of objects for the raytracer

> module Object where
> import Codec.Picture
> import Common
> import Camera

> data Object = Sphere Vector Float PixelRGB8
>             | Triangle (Vector, Vector, Vector) Vector PixelRGB8
>             | Mesh [Object]
>   deriving Show

> type Collision = (Ray, Object, Float)

> translate                         :: Vector -> Object -> Object
> translate v (Sphere a r c)         = Sphere (addVectors a v) r c
> translate v (Triangle (x, y, z) n c) = Triangle ((addVectors x v), (addVectors y v), (addVectors z v)) n c
> translate v (Mesh ts)              = Mesh (map (translate v) ts)

Equation for this was obtained from
https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection

also
https://gamedev.stackexchange.com/questions/96459/fast-ray-sphere-collision-code

> checkCollision :: Object -> Ray -> [Collision]
> checkCollision (Sphere a r c) (o, d) = if c1 > 0.0 && b1 > 0.0 then []
>                                        else if val < 0.0 then []
>                                        else if p1 < p2 then [((o, d), (Sphere a r c), p1)]
>                                        else if p1 > p2 then [((o, d), (Sphere a r c), p2)]
>                                        else []
>   where p1  = -b1 - (sqrt val)
>         p2  = -b1 + (sqrt val)
>         tmp = subVectors o a
>         b1  = dotProduct d tmp
>         c1  = (dotProduct tmp tmp) - r * r
>         val = b1 * b1 - c1
> checkCollision (Mesh ts) r = checkCollisions ts r

> checkCollisions :: [Object] -> Ray -> [Collision]
> checkCollisions os r = filter (thirdEq smallestdist) $ collisions
>   where collisions = concat $ map ((flip checkCollision) r) os
>         smallestdist = foldr1 min $ map getThird $ collisions

> getColor (_, (Sphere _ _ c), _) = c
> getColor (_, (Triangle _ _ c), _) = c

> getThird (_, _, a) = a

> thirdEq x (_, _, a) = x == a
