In order to build a raytracer we need some concept of where we are viewing the
world from. This consists of several pieces, the focal point of the camera, the
field of view, the direction the camera is pointing, and finally where the top
of the camera faces. With all of these components together we can generate a
list of rays, each one representing a pixel of our camera. Then in another
module we will take the list of rays and generate our image from it.

> module Camera where
> import Common

We will create a Ray type which is a pair where the first Vector is the Ray
origin and the second Vector is its normalized direction.

> type Ray = (Vector, Vector)

Now we will create a generateRays function which will return a list of lists of
Rays, each Ray represents a pixel and each list represents a row of pixels.

> generateRays :: Vector -> Float -> [Ray]
> generateRays f fov = [ (f, dir) | let h_x_res = x_res/2,
>                                   let h_y_res = y_res/2,
>                                   y <- [-h_y_res, -h_y_res + 1..h_y_res - 1],
>                                   x <- [-h_x_res, -h_x_res + 1..h_x_res - 1],
>                                   let inv_w = 1.0/x_res,
>                                   let inv_h = 1.0/y_res,
>                                   let angle = tan ((pi * fov/180.0) / 2.0),
>                                   let y_vec = (1.0 - 2.0 * ((y + 0.5) * inv_h)) * angle,
>                                   let x_vec = (2.0 * ((x + 0.5) * inv_h) - 1.0) * angle * (x_res/y_res),
>                                   let dir = normalize (x, y, -1.0)]
>  where x_res = 800
>        y_res = 600

> generateRay :: Vector -> Float -> Float -> Float -> Float -> Float -> Ray
> generateRay f fov x_res y_res a b = (f, dir)
>   where h_x_res = x_res/2
>         h_y_res = y_res/2
>         x = a - h_x_res
>         y = b - h_y_res
>         inv_w = 1.0/x_res
>         inv_h = 1.0/y_res
>         angle = tan (pi * 0.5 * fov/180.0)
>         y_vec = (2.0 * ((y + 0.5) * inv_h)) * angle
>         x_vec = (2.0 * ((x + 0.5) * inv_w)) * angle * (x_res/y_res)
>         dir = normalize (x_vec, y_vec, -1.0)
