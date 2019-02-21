module Main where
import Codec.Picture

main :: IO ()
main = writePng "derp.png" $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
