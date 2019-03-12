module Main where
import Codec.Picture
import Object
import Camera

main :: IO ()
--main = writePng "derp.png" $ generateImage pixelRenderer 250 300
--   where pixelRenderer x y = PixelRGBF (fromIntegral x) (fromIntegral y) 128
main = print(length vals)
  where vals = map (checkCollision (Sphere (0.0, 0.0, 0.0) 1.0 (1.0, 0.0, 0.0)))
             $ generateRays (0.0, 10.0, 0.0) 90
