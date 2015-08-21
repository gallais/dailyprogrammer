-- https://www.reddit.com/r/dailyprogrammer/comments/32goj8/20150413_challenge_210_easy_intharmonycom/

module Main where

import Data.Bits
import Data.Int
import Data.Function

getBits :: FiniteBits a => a -> [Bool]
getBits bs = fmap (testBit bs) $ reverse [0..n] where
  n = finiteBitSize bs - 1

fromBool :: Num a => Bool -> a
fromBool b = if b then 1 else 0

fromBools :: Num a => [Bool] -> a
fromBools = foldl (\ ih b -> 2 * ih + fromBool b) 0

avoid :: Num a => [Bool] -> a
avoid = fromBools . fmap not

match :: [Bool] -> [Bool] -> Float
match xs ys = finish $ zipWith (==) xs ys where
  finish ns = ((/) `on` fromIntegral) total numberBits where
    total      = 100 * (sum $ fmap fromBool ns)
    numberBits = length ns

intHarmony :: (Show a, FiniteBits a) => a -> a -> [String]
intHarmony m n = compat : fmap shouldAvoid [ (m, bsM), (n, bsN) ] where
  compat      = show (match bsM bsN) ++ "% Compatibility"
  shouldAvoid = \ (i, bsI) -> show i ++ " should avoid " ++ show (avoid bsI)
  bsM         = getBits m
  bsN         = getBits n

readInt32 :: String -> Int32
readInt32 = read

main :: IO ()
main = do
  cmd <- words <$> getLine
  case cmd of
    (m : n : _) -> mapM_ putStrLn ((intHarmony `on` readInt32) m n) >> main
    _           -> return ()

