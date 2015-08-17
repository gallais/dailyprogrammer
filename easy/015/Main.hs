module Main where

import System.Environment

leftJustify :: [String] -> [String]
leftJustify = fmap $ dropWhile (' ' ==)

rightJustify :: [String] -> [String]
rightJustify xs = ys where
  (ys , n) = foldr magic ([], 0) xs

  magic l (ls' , n') = (l' : ls', max m' n')
    where m' = length l
          l' = replicate (n - m') ' ' ++ l

main :: IO ()
main = do
  (fp : d : _) <- getArgs
  xs <- fmap lines $ readFile fp
  mapM_ putStrLn $
    case d of
      "l" -> leftJustify xs
      "r" -> rightJustify xs
      _   -> ["*** Error: direction should be \"l\" or \"r\""]
