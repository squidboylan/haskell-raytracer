This file handles the necessary definitions of objects for the raytracer

> module Object where
> import Codec.Picture
> import Common
> import Camera

> data Object = Sphere Vector Float PixelRGB8
>             | Triangle (Vector, Vector, Vector) Vector PixelRGB8
>             | Mesh [Object]
>   deriving Show

> translate                         :: Vector -> Object -> Object
> translate v (Sphere a r c)         = Sphere (addVectors a v) r c
> translate v (Triangle (x, y, z) n c) = Triangle ((addVectors x v), (addVectors y v), (addVectors z v)) n c
> translate v (Mesh ts)              = Mesh (map (translate v) ts)

Equation for this was obtained from
https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection

also
https://gamedev.stackexchange.com/questions/96459/fast-ray-sphere-collision-code

> checkCollision :: Object -> Ray -> Maybe (Ray, Object, Float)
> checkCollision (Sphere a r c) (o, d) = if c1 > 0.0 && b1 > 0.0 then Nothing
>                                        else if val < 0.0 then Nothing
>                                        else if p1 < p2 then (Just ((o, d), (Sphere a r c), p1))
>                                        else if p1 > p2 then (Just ((o, d), (Sphere a r c), p2))
>                                        else Nothing
>   where p1  = -b1 - (sqrt val)
>         p2  = -b1 + (sqrt val)
>         tmp = subVectors o a
>         b1  = dotProduct d tmp
>         c1  = (dotProduct tmp tmp) - r * r
>         val = b1 * b1 - c1

> checkCollisions :: [Object] -> Ray -> [(Ray, Object, Float)]
> checkCollisions os r = filter (thirdEq smallestdist) $ collisions
>   where collisions = unwrapMaybe $ map ((flip checkCollision) r) os
>         smallestdist = foldr1 min $ map getThird $ collisions

> getColor (_, (Sphere _ _ c), _) = c
> getColor (_, (Triangle _ _ c), _) = c

> getThird (_, _, a) = a

> thirdEq x (_, _, a) = x == a

https://stackoverflow.com/questions/40327699/filtering-nothing-and-unpack-just

> unwrapMaybe xs = [ x | Just x <- xs ]
