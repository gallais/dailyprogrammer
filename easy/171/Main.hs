-- https://www.reddit.com/r/dailyprogrammer/comments/2ao99p/7142014_challenge_171_easy_hex_to_8x8_bitmap/

module Main where

import Data.Bits
import Data.Word
import Data.Text      as T
import Data.Text.Read as TR
import qualified Data.Text.IO   as TIO


readHexPair :: Text -> Maybe (Word8, Word8)
readHexPair txt = do
  let (l : r : _)   = chunksOf 1 txt
  let Right (wl, _) = hexadecimal l
  let Right (wr, _) = hexadecimal r
  return $ (wl, wr)

decodeLine :: Text -> Maybe [Bool]
decodeLine txt = do
  (l , r) <- readHexPair txt
  return $ convert l ++ convert r
  where convert w = fmap (testBit w) $ Prelude.reverse [0..3]

decode :: Text -> Maybe [[Bool]]
decode txt = mapM decodeLine $ T.words txt

main :: IO ()
main = do
  xs <- TIO.getLine
  maybe (return ())
        (mapM_ (putStrLn . fmap (\ b -> if b then 'x' else ' ')))
        $ decode xs

