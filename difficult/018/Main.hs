-- https://www.reddit.com/r/dailyprogrammer/comments/qit4p/352012_challenge_18_difficult/

module Main where

import System.Environment
import Data.Function
import Codec.Picture

black :: Pixel8
black = 0

white :: Pixel8
white = 255

spiral :: Int -> Int -> Int -> Image Pixel8
spiral k w h = (generateImage (pixels . (+1)) `on` (k*)) w h where
  pixels x' y' =
    let x = (x' + k - 1) `quot` k
        y = y' `quot` k in
    if (even y && y <= x     && x <= h - y)
    || (even y && h - y <= x && x <= y)
    || (odd x  && w - x < y  && y < x)
    || (odd x  && x < y      && y < w - x)
    then black else white
  

main :: IO ()
main = do
  (m : n : k : _) <- getArgs
  let k' = read k
  writeBitmap "spiral.bmp" $ (spiral k' `on` (1+) . (2*) . read) m n
