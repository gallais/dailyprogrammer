-- https://www.reddit.com/r/dailyprogrammer/comments/3c9a9h/20150706_challenge_222_easy_balancing_words/

module Main where

import Data.Maybe
import Data.List
import Control.Monad

weight :: Char -> Int
weight c = 1 + fromJust (elemIndex c ['A'..'Z'])

value :: [(Int, Char)] -> Int
value = sum . map (uncurry $ \ k c -> k * weight c)

balance :: String -> Maybe (String, Char, String, Int)
balance str = msum attempts where
  bound    = length str - 2
  attempts =
    flip fmap [1..bound] $ \ pos -> do
      let (ys, z : zs) = splitAt pos str
      let left         = value $ zip [1..] $ reverse ys
      let right        = value $ zip [1..] zs
      guard $ left == right
      return (ys, z, zs, left) 

main :: IO ()
main = do
  str <- getLine
  putStrLn $ maybe 
           (str ++ " DOES NOT BALANCE")
           (\ (ys, z, zs, w) -> ys ++ " " ++ z : " " ++ zs ++ " - " ++ show w)
           $ balance str
