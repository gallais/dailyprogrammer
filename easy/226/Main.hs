-- https://www.reddit.com/r/dailyprogrammer/comments/3fmke1/20150803_challenge_226_easy_adding_fractions/

module Main where

import Data.Rational
import Control.Applicative

main :: IO ()
main = do
  n  <- read <$> getLine
  qs <- mapM (const $ read <$> getLine) [1..n]
  print $ foldr add (Rational 0 1) qs
