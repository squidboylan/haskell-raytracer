module Main where
import Codec.Picture
import Object

main :: IO ()
--main = writePng "derp.png" $ generateImage pixelRenderer 250 300
--   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
main = print(Sphere (3.0, 1.0, 2.0) 1.0 (0.2, 0.1, 0.5))