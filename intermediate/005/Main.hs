module Main where

import System.Environment
import Control.Monad
import Data.List

count :: [String] -> Int
count = sum . filter (> 1) . fmap length . group . sort . fmap sort

main :: IO ()
main = do
  (fp : _) <- getArgs
  xs       <- lines <$> readFile fp
  print $ count xs
