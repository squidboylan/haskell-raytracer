module Main where
import Codec.Picture
import Object
import Camera
import Lighting

main :: IO ()
main = writePng "derp.png" $ generateImage pixelRenderer 800 600
   where pixelRenderer x y = noLight
                           $ checkCollisions objects
                           $ ray (fromIntegral x) (fromIntegral y)
         objects = [(Sphere (0.0, 0.0, 0.0) 5.0 (PixelRGB8 255 0 0)),
                    (Sphere (1.5, 0.0, 5.0) 1.0 (PixelRGB8 0 255 255)),
                    (Sphere (-1.5, 0.0, 5.0) 1.0 (PixelRGB8 0 255 0))]
         camera = (0.0, 0.0, 10.0)
         ray = generateRay camera 60 800 600
