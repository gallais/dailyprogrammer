module Main where

import Data.Text    as T
import Data.Text.IO as TIO
import Data.List    as List
import Control.Monad.State

initSpaces :: Text -> Int
initSpaces = T.length . T.takeWhile (' ' ==)

nextPassage :: StateT [Text] Maybe [Text]
nextPassage = do
  xs <- get
  if List.null xs then lift Nothing
  else do
    let ys = List.dropWhile ((< 4) . initSpaces) xs
    let (zs, as) = List.span ((4 <=) . initSpaces) ys
    () <- put as
    return zs

findPassage :: Text -> StateT [Text] Maybe [Text]
findPassage pass = do
  txt <- nextPassage
  if List.any (pass `T.isInfixOf`) txt then return txt
  else findPassage pass

main :: IO ()
main = do
  txt  <- TIO.readFile "macbeth.txt"
  pass <- TIO.getLine
  case evalStateT (findPassage pass) (T.lines txt) of
    Nothing -> TIO.putStrLn $ (T.pack "Could not find ") `T.append` txt
    Just xs -> TIO.putStr $ T.unlines $ fmap (T.dropWhile (' ' ==)) xs
  
