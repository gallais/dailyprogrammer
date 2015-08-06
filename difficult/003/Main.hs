-- https://www.reddit.com/r/dailyprogrammer/comments/pkwgf/2112012_challenge_3_difficult/

module Main where

import Data.Map as Map
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

type Dictionary = Map BS.ByteString BS.ByteString

getDictionary :: IO Dictionary
getDictionary = do
  ws <- fmap BS.lines $ BS.readFile "dict.txt"
  return $ fromList $ zip (fmap BS.sort ws) ws

loop :: Dictionary -> IO ()
loop dict = do
  w <- BS.getLine
  if BS.unpack w == ":q" then return ()
  else do
    BS.putStrLn $ fromJust $ Map.lookup (BS.sort w) dict
    loop dict

main :: IO ()
main = do
  dict <- getDictionary
  loop dict
