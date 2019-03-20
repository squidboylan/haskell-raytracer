> module Lighting where
> import Codec.Picture
> import Common
> import Object
> import Camera

This file will contain all code that produces lighting. The first function is a
rather simple one and assumes no lighting, which means we just generate pixel
values directly based off the Object color

> noLight :: [(Ray, Object, Float)] -> PixelRGB8
> noLight [] = PixelRGB8 255 255 255
> noLight xs = getColor $ head xs
