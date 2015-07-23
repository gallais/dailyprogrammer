-- https://www.reddit.com/r/dailyprogrammer/comments/pjsdx/difficult_challenge_2/

module Main where

import System.IO
import System.Environment
import Data.Time.Clock

data StopWatch =
  StopWatch { passed :: Integer
            , paused :: Bool
            , latest :: UTCTime }

displayPassed :: StopWatch -> String
displayPassed sw =
  "[" ++ padding 2 (show mins) ++
  ":" ++ padding 2 (show ss)   ++
  ":" ++ padding 3 (show ms)   ++ "] "
  where
    padding n str = replicate (max 0 $ n - length str) '0' ++ str
    (s   , ms) = passed sw `quotRem` 1000
    (mins, ss) = s `quotRem` 60

updateTime :: StopWatch -> IO StopWatch
updateTime sw =
  if paused sw then return sw
  else do
    curr <- getCurrentTime
    let diff = truncate $ 1000 * (diffUTCTime curr $ latest sw)
    return $ sw { passed = passed sw + diff
                , latest = curr }

pause :: Handle -> StopWatch -> IO StopWatch
pause hdl sw = do
  let msg = if paused sw then "Start" else "Stop"
  hPutStrLn hdl $ displayPassed sw ++ msg
  return $ sw { paused = not (paused sw) }

lap :: Handle -> StopWatch -> IO ()
lap hdl sw =
  if paused sw then return ()
  else hPutStrLn hdl $ displayPassed sw ++ "Lap"

loop :: Handle -> StopWatch -> IO ()
loop hdl sw = do
  c   <- getChar
  sw' <- updateTime sw
  case c of
    'q' -> return ()
    'l' -> lap   hdl sw' >>  loop hdl sw'
    ' ' -> pause hdl sw' >>= loop hdl
    _   -> loop hdl sw'

main :: IO ()
main = do
  (fp : _) <- getArgs
  hSetBuffering stdin NoBuffering
  hdl <- openFile fp WriteMode
  getCurrentTime >>= loop hdl . StopWatch 0 True
  hClose hdl
