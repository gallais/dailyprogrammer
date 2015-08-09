-- https://www.reddit.com/r/dailyprogrammer/comments/2avd5i/7162014_challenge_171_intermediate_zoom_rotate/

module Main where

import System.Exit
import System.IO
import System.Environment
import System.Console.ANSI

import Control.Monad.State

import Data.Bits (testBit)
import Data.Word
import qualified Data.Text      as T
import Data.Text.Read as TR
import qualified Data.Text.IO   as TIO

-- This is taken from easy 171
readHexPair :: T.Text -> Maybe (Word8, Word8)
readHexPair txt = do
  let (l : r : _)   = T.chunksOf 1 txt
  let Right (wl, _) = hexadecimal l
  let Right (wr, _) = hexadecimal r
  return $ (wl, wr)

decodeLine :: T.Text -> Maybe [Bool]
decodeLine txt = do
  (l , r) <- readHexPair txt
  return $ convert l ++ convert r
  where convert w = fmap (testBit w) $ Prelude.reverse [0..3]

decode :: T.Text -> Maybe [[Bool]]
decode txt = mapM decodeLine $ T.words txt

-- These are the new features
zoom :: [[a]] -> [[a]]
zoom = concatMap (double . concatMap double)
  where double x = [x, x]

dezoom :: [[a]] -> [[a]]
dezoom = evens . fmap evens
  where evens = fmap snd . filter (even . fst) . zip [0..]

rotateClockwise :: [[a]] -> [[a]]
rotateClockwise = fmap reverse . preRotate

rotateAntiClockwise :: [[a]] -> [[a]]
rotateAntiClockwise = reverse . preRotate

preRotate :: [[a]] -> [[a]]
preRotate []       = []
preRotate [x]      = fmap (:[]) x
preRotate (x : xs) = zipWith (:) x $ preRotate xs

invert :: [[Bool]] -> [[Bool]]
invert = fmap (fmap not)

loop :: StateT [[Bool]] IO ()
loop = do
  c <- lift getChar
  case c of
    '+' -> modify zoom
    '-' -> modify dezoom
    'c' -> modify rotateClockwise
    'a' -> modify rotateAntiClockwise
    'i' -> modify invert
    'q' -> lift exitSuccess
    _   -> return ()
  lift clearScreen
  display
  loop

main :: IO ()
main = do
  (fp : _) <- getArgs
  xs       <- TIO.readFile fp
  case decode xs of
    Nothing  -> putStrLn $ "Invalid image stored in " ++ fp
    Just img -> do
      hSetBuffering stdin NoBuffering
      evalStateT (display >> loop) img

display :: StateT [[Bool]] IO ()
display = do
  xs <- get
  lift $ do
    clearScreen
    setCursorPosition 0 0
    mapM_ (putStrLn . fmap (\ b -> if b then 'x' else ' ')) xs

