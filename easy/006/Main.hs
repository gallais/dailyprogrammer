-- https://www.reddit.com/r/dailyprogrammer/comments/pp53w/2142012_challenge_6_easy/
module Main where

import Data.Natural
import Data.Ratio
import Data.Rational
import Data.CauchyReal as CR

-- Power Series
---------------------

type PowerSeries = Natural -> Rational

unfold :: (s -> (a, s)) -> s -> [a]
unfold next seed = a : unfold next s
  where (a, s) = next seed

partialSums :: PowerSeries -> Rational -> CauchyReal
partialSums ps x = unfold next (0, O) where
  next (v, n) = let w = v + x ^ (fromNatural' n) * ps n
                in w `seq` (w, (w, S n))

approximate :: PowerSeries -> Rational -> Natural -> Rational
approximate ps x n = CR.approximate n $ partialSums ps x

-- Arctan
---------------------

atanPS :: PowerSeries
atanPS n = ((- 1) ^ (fromNatural' n)) % (2 * fromNatural n + 1)

arctan :: Rational -> CauchyReal
arctan x = fmap (x *) $ partialSums atanPS (x * x)

-- Pi
---------------------

-- Awfully slow definition of Pi
myPi :: CauchyReal
myPi = fmap (4 *) $ arctan 1

-- Machin's formula
machin :: CauchyReal
machin = zipWith (\ a b -> 4 * (4 * a - b)) (arctan (1 % 5)) (arctan (1 % 239))


main :: IO ()
main = do
  let threshold2 = toNatural 2
  let threshold30 = toNatural 30
  putStrLn $ CR.display threshold30 machin
  putStrLn $ CR.display threshold2 myPi
