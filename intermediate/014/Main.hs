module Main where

import Data.List
import Control.Monad.State

sundaram :: Integer -> [Integer]
sundaram n =
  flip execState [1..n] $ do
    let bnd = (n - 1) `quot` 3
    forM [1..bnd] $ \ j ->
      forM [1..j] $ \ i ->
        let candidate = i + j + 2 * i * j in
        if candidate <= n
        then modify $ delete candidate
        else return ()

sieve :: Integer -> [Integer]
sieve n = fmap ((1+) . (2*)) $ sundaram $ (n - 1) `quot` 2

main :: IO ()
main = do
  let primes = sieve 10000
  print primes

