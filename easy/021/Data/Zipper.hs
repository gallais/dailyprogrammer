module Data.Zipper where

import Data.Maybe

data Zipper a = Nil | Cons [a] a [a]

left :: Zipper a -> Maybe (Zipper a)
left (Cons (a : as) b bs) = Just $ Cons as a (b : bs)
left _                    = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Cons as a (b : bs)) = Just $ Cons (a : as) b bs
right _                    = Nothing

foldr :: ([a] -> a -> [a] -> b -> b) -> b -> Zipper a -> b
foldr cons nil = go where
  go Nil               = nil
  go xs@(Cons as a bs) = cons as a bs $ go $ fromMaybe Nil $ right xs

fromList :: [a] -> Zipper a
fromList []       = Nil
fromList (a : as) = Cons [] a as
