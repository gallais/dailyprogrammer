-- https://www.reddit.com/r/dailyprogrammer/comments/2xoxum/20150302_challenge_204_easy_remembering_your_lines/

module Main where

import Data.Text    as T
import Data.Text.IO as TIO
import Data.List    as List
import Control.Monad.State

data Position = Position
  { act       :: Text
  , scene     :: Text
  , speaker   :: Text
  , rest      :: [Text] }

initSpaces :: Text -> Int
initSpaces = T.length . T.takeWhile (' ' ==)

grabPassage :: Monad m => StateT Position m [Text]
grabPassage = do
  pos <- get
  let (xs , ys) = List.span ((4 ==) . initSpaces) $ rest pos
  () <- put $ pos { rest = ys }
  return xs

grabActOrScene :: Monad m => StateT Position m ()
grabActOrScene = do
  pos <- get
  let (x : xs) = rest pos
  if T.pack "ACT " `T.isPrefixOf` x
    then put $ pos { act = grabInfo x, rest = xs }
    else if T.pack "SCENE " `T.isPrefixOf` x
      then put $ pos { scene = grabInfo x, rest = xs }
      else put $ pos { rest = xs }

grabInfo :: Text -> Text
grabInfo = T.takeWhile ('.' /=)

nextPassage :: StateT Position Maybe [Text]
nextPassage = do
  pos <- get
  case rest pos of
    []       -> lift Nothing
    (x : xs) -> do
      let (ys, zs) = T.span (' ' ==) x
      case T.length ys of
        0 -> grabActOrScene >> nextPassage
        2 -> put (pos { speaker = grabInfo zs, rest = xs }) >> nextPassage
        4 -> grabPassage
          
findPassage :: Text -> StateT Position Maybe [Text]
findPassage pass = do
  txt <- nextPassage
  if List.any (pass `T.isInfixOf`) txt then return txt
  else findPassage pass

main :: IO ()
main = do
  txt  <- TIO.readFile "macbeth.txt"
  pass <- TIO.getLine
  let dummyText = T.pack ""
  let initState = Position dummyText dummyText dummyText $ T.lines txt
  case runStateT (findPassage pass) initState of
    Nothing         -> TIO.putStrLn $ (T.pack "Could not find ") `T.append` txt
    Just (xs , pos) -> TIO.putStr $ T.unlines
                       $ [ act pos, scene pos, speaker pos ]
                       ++ fmap (T.dropWhile (' ' ==)) xs
  
