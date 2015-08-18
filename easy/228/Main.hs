-- https://www.reddit.com/r/dailyprogrammer/comments/3h9pde/20150817_challenge_228_easy_letters_in/

module Main where

import System.IO
import System.Environment
import Data.Char
import Data.ByteString.Char8 as BS

inAlphabeticalOrder :: ByteString -> Bool
inAlphabeticalOrder w = sort w == w

inReverseAlphabeticalOrder :: ByteString -> Bool
inReverseAlphabeticalOrder = inAlphabeticalOrder . BS.reverse

dispatch :: Handle -> Handle -> ByteString -> IO ()
dispatch alpha ralpha w =
  if      not (BS.all isAlpha w)       then return ()
  else if inAlphabeticalOrder w        then BS.hPutStrLn alpha  w
  else if inReverseAlphabeticalOrder w then BS.hPutStrLn ralpha w
  else return ()


main :: IO ()
main = do
  args   <- getArgs
  let fp = case args of
             []         -> "/usr/share/dict/american-english"
             (dict : _) -> dict
  ws     <- BS.readFile fp
  alpha  <- openFile "alphabetical.txt"  WriteMode
  ralpha <- openFile "ralphabetical.txt" WriteMode
  mapM_ (dispatch alpha ralpha) $ BS.lines ws
  hClose alpha
  hClose ralpha

