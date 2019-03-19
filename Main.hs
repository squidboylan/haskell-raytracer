module Main where
import Codec.Picture
import Object
import Camera
import Lighting

main :: IO ()
main = writePng "derp.png" $ generateImage pixelRenderer 800 600
   where pixelRenderer x y = noLight $ checkCollisions objects $ generateRay (0.0, 0.0, 10.0) 60 800 600 (fromIntegral x) (fromIntegral y)
         objects = [(Sphere (0.0, 0.0, 0.0) 5.0 (PixelRGB8 255 0 0))]
